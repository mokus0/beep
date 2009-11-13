module Network.BEEP.Core.Session.Types where

import Network.BEEP.Core.DataFrame.Types
import qualified Network.BEEP.Core.Mapping as M
import qualified Network.BEEP.Core.Profile as P
import {-# SOURCE #-} Network.BEEP.Profile.ChannelManagement

data Session m = Session
    { sessionPeerAddr  :: Maybe (M.PeerAddr m)
    , sessionHandle    :: M.PeerHandle m
    , sessionChanZero  :: Channel m ChannelManagement
    }

data Channel m p = Channel
    { chanSession       :: Session m
    , chanId            :: ChannelId
    , chanProfile       :: p
    , chanProfileState  :: P.ChannelState p
    }