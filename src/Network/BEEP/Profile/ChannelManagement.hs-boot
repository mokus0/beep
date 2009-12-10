{-# LANGUAGE
        MultiParamTypeClasses, FlexibleInstances,
        FlexibleContexts
  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Network.BEEP.Profile.ChannelManagement where

import Network.BEEP.Core.Profile
import Network.BEEP.Core.Mapping

import Data.ByteString.Lazy
import Network.URI

data ChannelManagement = ChannelManagement
instance (Monad f, Mapping f m) => Profile f m ChannelManagement

mkGreeting :: [URI] -> ByteString
