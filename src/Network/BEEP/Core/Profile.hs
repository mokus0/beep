{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Core.Profile where

import {-# SOURCE #-} Network.BEEP.Core.Session.Types
import Network.BEEP.Core.Mapping

data MalformedMsgResponse p
    = CloseChannel
    | ErrorMsg (ProfileMessage p)

class Mapping f m => Profile (f :: * -> *) m p where
    type ChannelState p
    
    type InitMessage p
    type ProfileMessage p
    
    initialize :: p -> Session m -> f (ChannelState p)
    
    -- what does it do...
    -- need to be able to react to messages, etc...
    -- and do what?
    -- deliver to client via hooks, Chan, etc?
    -- 
    -- react :: p -> DataFrame -> ChannelState p -> f (ChannelState p)
