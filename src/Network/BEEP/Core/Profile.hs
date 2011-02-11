{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleContexts, RankNTypes
  #-}
module Network.BEEP.Core.Profile where

import {-# SOURCE #-} Network.BEEP.Core.Session.Types
import Network.BEEP.Core.Mapping

import Data.ByteString.Lazy (ByteString)

class (Mapping f m) => Profile (f :: * -> *) m p where
    data ProfileState p
    
    initialize :: p -> Session f m -> f (ProfileState p)
    
    data Message p :: * -> *
    
    encodeMessage  :: ProfileState p -> Message p resp -> f ByteString
    decodeMessage  :: ProfileState p -> ByteString -> (forall resp. Message p resp -> f a) -> f a
    
    encodeResponse :: ProfileState p -> Message p resp -> resp -> f ByteString
    decodeResponse :: ProfileState p -> Message p resp -> ByteString -> f resp
