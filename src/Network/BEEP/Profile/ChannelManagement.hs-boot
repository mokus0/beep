{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Data.ByteString.Class
import Network.URI

data ChannelManagement = ChannelManagement
instance (Monad f, Mapping f m) => Profile f m ChannelManagement

data CMMessage  -- should the types distinguish how the messages may be used?  Probably...
    = Greeting [URI]
    | Start
    | Profile
    | Error
    | Close
    | Ok

instance LazyByteString CMMessage