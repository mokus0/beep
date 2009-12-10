{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Session
import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Data.ByteString.Lazy
import Data.ByteString.Class
import Network.URI

data ChannelManagement = ChannelManagement

instance (Monad f, Mapping f m) => Profile f m ChannelManagement where
    data ProfileState ChannelManagement = CMState
    data Message ChannelManagement
        = Start     -- start a channel
        | Close     -- close a channel
    
    data Reply ChannelManagement 
        = Greeting [URI]    -- positive reply to session start
        | Profile           -- positive reply to Start
        | Ok                -- positive reply to Close
        | Error             -- negative reply
    
    initialize ChannelManagement session = do
        return CMState
    
    receiveMessage CMState _ = undefined
    receiveReply   CMState _ = undefined

instance LazyByteString (Message ChannelManagement) where
instance LazyByteString (Reply   ChannelManagement) where
    toLazyByteString (Greeting profiles) = mkGreeting profiles

-- needed because "data Message ChannelManagement" can't be
-- in the hs-boot file, but the Greeting data constructor
-- is needed in the Session initialization thing.
mkGreeting :: [URI] -> ByteString
mkGreeting = undefined