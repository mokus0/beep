{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts, GADTs
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

data Greeting
    = Greeting [Feature] [Language] [URI]   -- positive reply to session start
data StartReply
    = Profile           -- positive reply to Start message indicating selected profile
data CloseReply
    = Ok                -- positive reply to Close message
data Error
    = Error             -- negative reply to any channel management message

instance (Monad f, Mapping f m) => Profile f m ChannelManagement where
    data ProfileState ChannelManagement = CMState
    data Message ChannelManagement resp where
        Start :: Message ChannelManagement (Either Error StartReply)    -- start a channel; requires a set of profiles to offer
        Close :: Message ChannelManagement (Either Error CloseReply)    -- close a channel
    
    initialize ChannelManagement session = do
        return CMState
    
    
