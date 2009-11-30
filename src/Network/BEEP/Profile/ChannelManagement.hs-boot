{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Network.URI

data ChannelManagement = ChannelManagement
instance (Monad f, Mapping f m) => Profile f m ChannelManagement

data Message ChannelManagement = Greeting [URI]
