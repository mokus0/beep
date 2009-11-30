{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Core.Profile where

import {-# SOURCE #-} Network.BEEP.Core.Session.Types
import Network.BEEP.Core.Mapping

import Data.ByteString.Lazy

class Mapping f m => Profile (f :: * -> *) m p where
    data ProfileState p
    
    initialize :: p -> Session f m -> f (ProfileState p)
    
    -- partial message support?
    data Message p
    data Reply   p
    data Answer  p
    data Error   p
    
    fmtMessage :: Message p -> ByteString
    
    -- what does it do...
    -- need to be able to react to messages, etc...
    -- and do what?
    -- deliver to client via hooks, Chan, etc?
    -- 
    -- react :: p -> DataFrame -> ChannelState p -> f (ChannelState p)
