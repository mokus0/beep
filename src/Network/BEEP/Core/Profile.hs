{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Core.Profile where

data MalformedMsgResponse p
    = CloseChannel
    | ErrorMsg (ProfileMessage p)

class Profile (f :: * -> *) p where
    type ChannelState p
    
    type InitMessage p
    type ProfileMessage p
    
    initialize :: p -> f (Either String (ChannelState p))
    
    -- what does it do...
    -- need to be able to react to messages, etc...
    -- and do what?
    -- deliver to client via hooks, Chan, etc?
    -- 
    -- react :: p -> DataFrame -> ChannelState p -> f (ChannelState p)
