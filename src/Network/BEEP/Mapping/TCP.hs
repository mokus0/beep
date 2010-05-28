{-# LANGUAGE 
        EmptyDataDecls,
        MultiParamTypeClasses,
        TypeFamilies,
        GeneralizedNewtypeDeriving
  #-}
module Network.BEEP.Mapping.TCP
    ( Tcp, PeerAddr(TcpAddr), PeerSpec(TcpSocket)
    , openTCPConnection, acceptTCPConnection
    ) where

import Prelude hiding (catch)
import Control.Exception

import Network.BEEP.Core.Mapping
import Network.BEEP.Mapping.TCP.Seq
import Network.BEEP.Core.DataFrame
import Network.BEEP.Core.DataFrame.Get (getDataFrameOr, Result(..))
import Network.BSD
import Network.Socket
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Socket.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket.ByteString as BS

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Loops

-- could use IntMap, but there's no guarantee that Int is as big as ChannelId
import qualified Data.Map as M

data Tcp

data TcpChannelState = TcpChannelState
    { tcpChannelRecvWindow  :: TVar Window
    , tcpChannelSendWindow  :: TVar Window
    , tcpChannelLastAck     :: TVar AckNo
    }

newTcpChannelState :: STM TcpChannelState
newTcpChannelState = do
    rWin <- newTVar  4096
    sWin <- newTVar  4096
    ack <- newTVar  0
    return TcpChannelState
        { tcpChannelRecvWindow  = rWin
        , tcpChannelSendWindow  = sWin
        , tcpChannelLastAck     = ack
        }

withConn cxt (TcpState {tcpHandle = hstream}) action = bracket
    (takeMVar hstream)
    (putMVar hstream)
    (maybe (fail $ cxt ++ ": connection already closed") (\sock -> handle (onErr sock) (action sock)))
    where 
        onErr :: Socket -> SomeException -> IO a
        onErr sock err = do
            sClose sock
            throw err

instance Mapping IO Tcp where
    data PeerAddr Tcp 
        = TcpAddr String PortNumber
        | SockAddr SockAddr
    data PeerSpec Tcp = TcpSocket Socket
    
    data PeerHandle Tcp = TcpState
        { tcpRole           :: Role
        , tcpHandle         :: MVar (Maybe Socket)
        , tcpReceivedFrames :: Chan DataFrame
        , tcpOutboundFrames :: Chan DataFrame
        , tcpChannels       :: TVar (M.Map ChannelId TcpChannelState)
        }
    
    initiate (TcpAddr host port) = do
        sock        <- openTCPConnection host port
        createPeer (TcpSocket sock) Initiator
        
    createPeer (TcpSocket sock) role = do
        handle      <- newMVar (Just sock)
        inFrames    <- newChan
        outFrames   <- newChan
        channels    <- newTVarIO M.empty
        
        let conn = TcpState
                { tcpRole           = role
                , tcpHandle         = handle
                , tcpReceivedFrames = inFrames
                , tcpOutboundFrames = outFrames
                , tcpChannels       = channels
                }
        
        forkTcpManager sock conn
        return conn
    
    getPeerAddr conn =  withConn "getPeerAddr" conn $ \handle -> do
        sockAddr <- getPeerName handle
        return (SockAddr sockAddr)
    
    disconnect = terminate
    terminate (TcpState {tcpHandle = handle}) = 
        bracket 
            (takeMVar handle)
            (\_ -> putMVar handle Nothing)
            (maybe (return ()) sClose)
    
    isConnected TcpState{tcpHandle = handle} = bracket 
        (takeMVar handle)
        (putMVar  handle)
        (maybe (return False) sIsConnected)

    
    newChannel TcpState{tcpChannels = channels} chanId = atomically $ do
        channelsMap <- readTVar channels
        
        when (chanId `M.member` channelsMap) $ fail "newChannel: channel already exists"
        newState <- newTcpChannelState
        writeTVar channels (M.insert chanId newState channelsMap)
    
    freeChannel TcpState{tcpChannels = channels} chanId = atomically $ do
        channelsMap <- readTVar channels
        
        when (chanId `M.notMember` channelsMap) $ fail "freeChannel: channel does not exist"
        writeTVar channels (M.delete chanId channelsMap)
    
    role conn = withConn "role" conn $ \_ -> return (tcpRole conn)
    
    -- TODO: improve behavior on closed connection
    -- (should return any frames received before close, but not block
    -- if the received-frame queue is empty)
    receive conn = withConn "receive" conn $ \_ -> do
         readChan (tcpReceivedFrames conn)
    
    send conn frame = withConn "send" conn $ \_ -> do
        writeChan (tcpOutboundFrames conn) frame

doReceive recvSize stash sock = do
    -- INVARIANT: stash MVar must remain full upon exit.
    -- at any time, not just when listening for other packets
    let go parse stashed = do
            stuff <- case stashed of 
                Nothing -> BS.recv sock recvSize
                Just x -> return x
            case parse stuff of
                Fail bs cx err -> do
                    putMVar stash Nothing
                    fail ("receive: Parse failure (" ++ err ++ ") near " ++ show stuff)
                Partial parseRest -> do
                    -- frame is incomplete, get more data
                    go parseRest Nothing
                Done rest frame -> do
                    putMVar stash $ if BS.null rest
                        then Nothing
                        else Just rest
                    
                    return frame
    
    stashed <- takeMVar stash
    go (getDataFrameOr seqFrame) stashed

doSend :: PeerHandle Tcp -> DataFrame -> Socket -> IO ()
doSend conn frame sock = do
    let sendFrame f = sendLazyByteString sock (putDataFrame f)
    
    chanState <- requireChannelState "send" conn (channelId frame)
    allowed   <- getAllowedSize chanState (seqNo frame)
    
    if payloadSize frame <= allowed
        then sendFrame frame
        else if allowed > 0
            then do
                let (allowedFrame, mbDeferFrame) = splitDataFrameAt allowed frame
                sendFrame allowedFrame
            
                case mbDeferFrame of
                    Nothing -> return ()
                    Just f  -> deferSend conn f
            else do
                deferSend conn frame

sendLazyByteString sock bs = sendBL bs (BL.length bs)
    where
        sendBL bs len = do
            sent <- BL.send sock bs
            case sent `compare` len of
                LT -> sendBL (BL.drop sent bs) (len - sent)
                EQ -> return ()
                GT -> fail "send: programming error: more data seems to have been sent than was intended"

deferSend conn frame = do
    
    fail "deferSend: write me!"

requireChannelState cxt conn chan = do
    mbChanState <- getChannelState conn chan
    case mbChanState of
        Nothing -> fail (cxt ++ ": SEQ frame receiver for non-existent channel " ++ show chan)
        Just ch -> return ch

recvSeq conn (Seq chan ack win) = do
    chanState <- requireChannelState "receive" conn chan
    
    atomically $ do
        writeTVar (tcpChannelLastAck chanState) ack
        writeTVar (tcpChannelSendWindow chanState) win

getAllowedSize :: TcpChannelState -> SeqNo -> IO Size
getAllowedSize chanState seqno = atomically $ do
    ack   <- readTVar (tcpChannelLastAck chanState)
    win         <- readTVar (tcpChannelSendWindow  chanState)
    
    return (allowedSize win ack seqno)

allowedSize :: Window -> AckNo -> SeqNo -> Size
allowedSize (Window sz) (AckNo ack) seqno
    | allowed < maxSize = fromIntegral allowed
    | otherwise         = 0
    where 
        maxSize = fromIntegral (maxBound :: Size)
        lastAllowed = extendSeqNo ack sz
        allowed = lastAllowed - seqno

getChannelState :: PeerHandle Tcp -> ChannelId -> IO (Maybe TcpChannelState)
getChannelState conn chan = do
    channelStates <- atomically (readTVar (tcpChannels conn))
    return (M.lookup chan channelStates)

forkTcpManager :: Socket -> PeerHandle Tcp -> IO ()
forkTcpManager sock conn = do
    let onErr :: SomeException -> IO ()
        onErr err = do
            try (terminate conn) :: IO (Either SomeException ())
            throw err
    
    recvSize    <- getSocketOption sock RecvBuffer
    stash       <- newMVar Nothing
    maxSeg      <- getSocketOption sock MaxSegment
    forkOS . whileM_ (isConnected conn) . handle onErr $ do
        -- TCP receiver thread
        frame <- doReceive (fromIntegral recvSize) stash sock
        case frame of 
            Left seqFrame -> do
                recvSeq conn seqFrame
            
            Right dataFrame -> do
                chanState <- requireChannelState "TCP receive thread" conn (channelId dataFrame)
                window <- atomically $ readTVar (tcpChannelRecvWindow chanState)
                acknowledgeFrame sock dataFrame window
                writeChan (tcpReceivedFrames conn) dataFrame
    
    forkOS . whileM_ (isConnected conn) . handle onErr $ do
        -- TCP sender thread
        -- TODO: check for deferred sends
        frame <- readChan (tcpOutboundFrames conn)
        doSend conn frame sock
    
    return ()

seqForFrame dataFrame win = Seq chan ack win
    where
        chan = channelId dataFrame
        ack = AckNo (extendSeqNo (seqNo dataFrame) (size dataFrame))

acknowledgeFrame sock dataFrame win = do
    let bs = putSeqFrame (seqForFrame dataFrame win)
    sendLazyByteString sock bs

-- |Convenience function to open a TCP socket given the hostname and port of
-- a computer that is listening for a connection
openTCPConnection :: String -> PortNumber -> IO Socket
openTCPConnection host port = do
    hostInfo <- getHostByName host
    
    let hostAddrs = hostAddresses hostInfo
        addr = SockAddrInet port (head hostAddrs)
    
    when (null hostAddrs) $ fail ("openTCPConnection: No addresses known for host " ++ host)
    
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    
    return sock

-- |Convenience function to listen for exactly one connection on a given port.
-- Uses the first-reported local address.  If that's not what you need, you'll have
-- to write your own ;)
acceptTCPConnection :: PortNumber -> IO Socket
acceptTCPConnection port = do
    hostInfo <- getHostEntry
    let hostAddrs = hostAddresses hostInfo
        addr = SockAddrInet port (head hostAddrs)
    
    when (null hostAddrs) $ fail ("acceptTCPConnection: No addresses known for host")

    listenSock <- socket AF_INET Stream defaultProtocol
    setSocketOption listenSock ReuseAddr 1
    bindSocket listenSock addr
    listen listenSock 0
    (sock, sockAddr) <- accept listenSock
    sClose listenSock
    
    return sock