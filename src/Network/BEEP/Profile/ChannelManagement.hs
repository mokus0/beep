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
        = Greeting [URI]
        | Start
        | Profile
        | Close
    
    data Reply ChannelManagement 
        = Ok
        | Error
    
    initialize ChannelManagement session = do
        return CMState

instance LazyByteString (Message ChannelManagement)
instance LazyByteString (Reply ChannelManagement)

-- needed because "data Message ChannelManagement" can't be
-- in the hs-boot file, but the Greeting data constructor
-- is needed in the Session initialization thing.
mkGreeting :: [URI] -> ByteString
mkGreeting = toLazyByteString . Greeting