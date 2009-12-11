{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement
    ( module Network.BEEP.Profile.ChannelManagement.Types
    ) where

import Network.BEEP.Profile.ChannelManagement.Types
import Network.BEEP.Profile.ChannelManagement.Input
import Network.BEEP.Profile.ChannelManagement.Output

import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Data.ByteString.Class
import Network.URI

instance (Monad f, Mapping f m) => Profile f m ChannelManagement where
    data ProfileState ChannelManagement = CMState
    data Message ChannelManagement
        = Start     -- start a channel
        | Close     -- close a channel
    
    data Reply ChannelManagement 
        = Greeting [Feature] [Language] [URI]   -- positive reply to session start
        | Profile           -- positive reply to Start
        | Ok                -- positive reply to Close
        | Error             -- negative reply
    
    initialize ChannelManagement session = do
        return CMState
    
    receiveMessage CMState _ = undefined
    receiveReply   CMState _ = undefined

instance LazyByteString (Message ChannelManagement) where
instance LazyByteString (Reply   ChannelManagement) where
    toLazyByteString (Greeting features langs profiles) = mkGreeting features langs profiles
