module Network.BEEP.Core.DataFrame.Put where

import Network.BEEP.Core.DataFrame.Types
import Data.ByteString.Lazy.Char8

-- only guaranteed-stable interface is the 'putDataFrame' function
putDataFrame :: DataFrame -> ByteString
putDataFrame = undefined