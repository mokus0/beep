{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Session
import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

data ChannelManagement = ChannelManagement

data CMInitMessage = Greeting
data CMMessage
    = Start
    | Profile
    | Error
    | Close
    | Ok
-- use type system to tie messages to frame types?

instance Mapping IO m => Profile IO m ChannelManagement where
    type InitMessage    ChannelManagement = CMInitMessage
    type ProfileMessage ChannelManagement = CMMessage