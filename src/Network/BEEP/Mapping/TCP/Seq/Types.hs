{-# LANGUAGE
        GeneralizedNewtypeDeriving
  #-}
module Network.BEEP.Mapping.TCP.Seq.Types
    ( Seq(..)
    , AckNo(..)
    , Window(..)
    , winSize
    , decreaseWindow, increaseWindow
    ) where

import Network.BEEP.Core.DataFrame 
    ( ChannelId(..), SeqNo(..), Size(..)
    , HasTag(..), Tag(..)
    , mapReadsPrec
    )

import Data.Bits (Bits)

data Seq = Seq ChannelId AckNo Window deriving (Eq, Show)
instance HasTag Seq where tag = Tag "SEQ"

newtype AckNo = AckNo SeqNo deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
instance Show AckNo where showsPrec p (AckNo x) = showsPrec p x
instance Read AckNo where readsPrec = mapReadsPrec AckNo

newtype Window = Window Size deriving (Eq, Ord, Bounded, Enum, Num, Real, Integral, Bits)
instance Show Window where showsPrec p (Window x) = showsPrec p x
instance Read Window where readsPrec = mapReadsPrec Window

winSize (Window sz) = sz

decreaseWindow (Window w) dw = Window (w - dw)
increaseWindow (Window w) dw = Window (w + dw)

