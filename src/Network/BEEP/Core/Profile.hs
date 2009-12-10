{-# LANGUAGE
        MultiParamTypeClasses, TypeFamilies, FlexibleContexts
  #-}
module Network.BEEP.Core.Profile where

import {-# SOURCE #-} Network.BEEP.Core.Session.Types
import Network.BEEP.Core.Mapping

import Data.ByteString.Class

-- |In the future these "LazyByteString" contexts may be replaced with
-- something more specialized.  Goals in that change may include
-- improving parsing failure modes and adding support for partial messages.
class ( Mapping f m
      , LazyByteString (Message p)
      , LazyByteString (Reply   p)
      ) => Profile (f :: * -> *) m p where
    data ProfileState p
    
    initialize :: p -> Session f m -> f (ProfileState p)
    
    data Message p
    data Reply   p
    
    receiveMessage  :: ProfileState p -> Message p -> f ()
    receiveReply    :: ProfileState p -> Reply   p -> f ()
