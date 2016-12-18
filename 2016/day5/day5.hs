import Control.Arrow ((&&&))
import Data.List
import Data.Ord

import Data.Hash.MD5
import Data.Word
import Data.Monoid
import Data.Bits
import Text.Printf

abcdA :: ABCD -> Word32
abcdA (ABCD (a,_,_,_)) = a

printHex :: Word32 -> String
printHex n = printf "%x" n

-- swap32 :: Word32 -> Word32
-- swap32 w =   (w `shr` 24)            .|.  (w `shl` 24)
--         .|. ((w `shr` 8) .&. 0xff00) .|. ((w .&. 0xff00) `shl` 8)
--   where
--     shl = unsafeShiftL
--     shr = unsafeShiftR

zeroPrefix :: Word32 -> Bool
zeroPrefix x = (x .&. 0x00f0ffff) == 0

sixthHex :: Word32 -> Word32
sixthHex x = (x `shift` (-16)) .&. 0xf

seventhHex :: Word32 -> Word32
seventhHex x = (x `shift` (-28)) .&. 0xf

-- foo
--   = map (printHex . sixthHex . swap32)
--   . map (abcdA . md5)
--   $ [Str "abc3231929"]

doorId :: String
doorId = "cxdnnyjw"

password1 :: String -> String
password1 doorId
  = foldMap (printHex . sixthHex)
  . take 8
  . filter zeroPrefix
  . map (abcdA . md5 . Str . (doorId <>) . show)
  $ [0..]


validPosition x = sixthHex x >= 0 && sixthHex x <= 7

-- FIXME: no duplicates!!!!
-- password2 "abc"
-- [(1,5),(4,14),(7,3),(3,12),(0,0),(6,14),(7,11),(6,6)]
--password2 :: String -> String
password2 doorId
  -- = foldMap (printHex . snd)
  -- . sortBy (comparing fst)
  = map (sixthHex &&& seventhHex)
  . take 8
  . filter (\x -> zeroPrefix x && validPosition x)
  . map (abcdA . md5 . Str . (doorId <>) . show)
  $ [0..]
