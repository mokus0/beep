{-# LANGUAGE
        FlexibleContexts
  #-}
module Network.BEEP.Core.Connection where

import Network.BEEP.Core.DataFrame
import qualified Network.BEEP.Core.Mapping as M
import qualified Network.BEEP.Core.Profile as P
import Network.BEEP.Profile.ChannelManagement

import Data.IORef

data Connection m = Connection
    { connPeerAddr  :: Maybe (M.PeerAddr m)
    , connHandle    :: M.PeerHandle m
    , connIsOpen    :: IORef Bool
    , connChanZero  :: Channel m ChannelManagement
    }

connect :: M.Mapping IO m => M.PeerAddr m -> IO (Either String (Connection m))
connect addr = do
    isOpen <- newIORef True
    let onClose = writeIORef isOpen False
    handle <- M.connect addr onClose
    chan0  <- P.initialize ChannelManagement
    case chan0 of
        Right chan0 -> 
            let conn = Connection
                    { connPeerAddr  = Just addr
                    , connHandle    = handle
                    , connChanZero  = Channel
                        { chanConnection    = conn
                        , chanId            = 0
                        , chanProfile       = ChannelManagement
                        , chanProfileState  = chan0
                        }
                    }
             in return (Right conn)
        Left err    -> return (Left err)

data Channel m p = Channel
    { chanConnection    :: Connection m
    , chanId            :: ChannelId
    , chanProfile       :: p
    , chanProfileState  :: P.ChannelState p
    }