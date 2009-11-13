{-# LANGUAGE
        FlexibleContexts, RecursiveDo
  #-}
module Network.BEEP.Core.Session
    ( Session, Channel
    , initiateSession, createSession
    , disconnectSession, terminateSession
    ) where

import Network.BEEP.Core.Session.Types
import Network.BEEP.Core.DataFrame
import qualified Network.BEEP.Core.Mapping as M
import qualified Network.BEEP.Core.Profile as P
import {-# SOURCE #-} Network.BEEP.Profile.ChannelManagement

import Data.IORef

initiateSession :: M.Mapping IO m => M.PeerAddr m -> IO (Session m)
initiateSession addr = mdo
    handle <- M.initiate addr
    chan0  <- P.initialize ChannelManagement session
    let session = Session
            { sessionPeerAddr  = Just addr
            , sessionHandle    = handle
            , sessionChanZero  = Channel
                { chanSession    = session
                , chanId            = 0
                , chanProfile       = ChannelManagement
                , chanProfileState  = chan0
                }
            }
    return session

createSession :: M.Mapping IO m => M.PeerSpec m -> M.Role -> IO (Session m)
createSession peer role = mdo
    handle <- M.createPeer peer role
    addr   <- M.getPeerAddr handle
    chan0  <- P.initialize ChannelManagement session
    let session = Session
            { sessionPeerAddr  = Just addr
            , sessionHandle    = handle
            , sessionChanZero  = Channel
                { chanSession    = session
                , chanId            = 0
                , chanProfile       = ChannelManagement
                , chanProfileState  = chan0
                }
            }
    return session

disconnectSession :: M.Mapping IO m => Session m -> IO ()
disconnectSession session = do
    M.disconnect (sessionHandle session)

terminateSession :: M.Mapping IO m => Session m -> IO ()
terminateSession session = do
    M.terminate (sessionHandle session)
