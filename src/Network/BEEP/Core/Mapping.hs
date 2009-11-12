{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies
  #-}
module Network.BEEP.Core.Mapping where

import Network.BEEP.Core.DataFrame

data Role = Listener | Initiator
    deriving (Eq, Show, Ord, Enum, Bounded)

class {- (?) Monad f => -} Mapping f m {- (?) | m -> f -} where
    data PeerAddr m
    data PeerSpec m -- TODO: better name?
    data PeerHandle m
    
    
    -- |Initiate a connection
    initiate    :: PeerAddr m   -> f (PeerHandle m)
    
    -- |Start a connection in the specified role.  Due to wide
    -- variation in ways of establishing listener-role connections,
    -- this is the preferred mechanism for starting a listener,
    -- in addition to any functions provided by the purpose by the
    -- mapping module itself.
    createPeer :: PeerSpec m -> Role -> f (PeerHandle m)
    
    -- |Get the address of the remote peer.  Used when creating a peer
    -- from a "PeerSpec m"
    getPeerAddr :: PeerHandle m -> f (PeerAddr m)
    
    -- |Gracefully close a connection
    disconnect  :: PeerHandle m -> f ()
    
    -- |Terminate a connection without concern for the other side
    terminate   :: PeerHandle m -> f ()
    
    -- |Check whether the connection is still open
    isConnected :: PeerHandle m -> f Bool
    
    role        :: PeerHandle m -> f Role
    
    -- |Used to inform the mapping of a new channel
    newChannel  :: PeerHandle m -> ChannelId -> f ()
    -- |Used to inform the mapping that a channel is no longer in use
    freeChannel :: PeerHandle m -> ChannelId -> f ()
    
    -- | non-blocking try-to-send only for now...
    send        :: PeerHandle m -> DataFrame -> f Bool
    receive     :: PeerHandle m -> f DataFrame
