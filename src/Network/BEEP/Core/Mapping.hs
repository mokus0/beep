{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Core.Mapping where

import Network.BEEP.Core.DataFrame

class {- (?) Monad f => -} Mapping f m {- (?) | m -> f -} where
    data PeerAddr m
    data PeerHandle m
    
    connect     :: PeerAddr m -> f () -> f (PeerHandle m)
    disconnect  :: PeerHandle m -> f ()
    isConnected :: PeerHandle m -> f Bool
    
    send        :: PeerHandle m -> DataFrame -> f ()
    receive     :: PeerHandle m -> f (DataFrame)
