{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Profile

data ChannelManagement = ChannelManagement

data CMInitMessage = Greeting
data CMMessage
    = Start
    | Profile
    | Error
    | Close
    | Ok
-- use type system to tie messages to frame types?

instance Profile IO ChannelManagement where
    type InitMessage    ChannelManagement = CMInitMessage
    type ProfileMessage ChannelManagement = CMMessage