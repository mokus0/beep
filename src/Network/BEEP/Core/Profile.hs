{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleContexts
  #-}
module Network.BEEP.Core.Profile where

import {-# SOURCE #-} Network.BEEP.Core.Session.Types
import Network.BEEP.Core.Mapping

import Data.ByteString.Class

class ( Mapping f m
      , LazyByteString (Message p)
      , LazyByteString (Reply   p)
      ) => Profile (f :: * -> *) m p where
    data ProfileState p
    
    initialize :: p -> Session f m -> f (ProfileState p)
    
    -- partial message support?
    data Message p
    data Reply   p
    
    -- what does it do...
    -- need to be able to react to messages, etc...
    -- and do what?
    -- deliver to client via hooks, Chan, etc?
    -- 
    -- react :: p -> DataFrame -> ChannelState p -> f (ChannelState p)
