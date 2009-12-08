{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Session
import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Data.ByteString.Class
import Network.URI

data ChannelManagement = ChannelManagement

data CMMessage  -- should the types distinguish how the messages may be used?  Probably...
    = Greeting [URI]
    | Start
    | Profile
    | Error
    | Close
    | Ok
data CMReply
    = 
-- use type system to tie messages to frame types?

instance (Monad f, Mapping f m) => Profile f m ChannelManagement where
    data ProfileState ChannelManagement = CMState
    type Message      ChannelManagement = CMMessage
    
    initialize ChannelManagement session = do
        return CMState

instance LazyByteString CMMessage