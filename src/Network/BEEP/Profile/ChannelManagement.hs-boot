{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleInstances,
        FlexibleContexts
  #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

data ChannelManagement = ChannelManagement
instance Mapping IO m => Profile IO m ChannelManagement