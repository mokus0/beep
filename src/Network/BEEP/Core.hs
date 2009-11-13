{-# LANGUAGE TypeFamilies #-}
module Network.BEEP.Core
    ( Mapping, Role(..), PeerAddr, PeerSpec
    , module Network.BEEP.Mapping.TCP
    , Session
    , initiateSession, createSession
    , disconnectSession, terminateSession
    , Channel
    ) where

import Network.BEEP.Core.DataFrame
import Network.BEEP.Core.Mapping
import Network.BEEP.Core.Profile
import Network.BEEP.Core.Session
import Network.BEEP.Mapping.TCP
import Network.BEEP.Profile.ChannelManagement