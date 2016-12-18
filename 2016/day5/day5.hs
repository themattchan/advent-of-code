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

-- foo
--   = map (printHex . sixthHex . swap32)
--   . map (abcdA . md5)
--   $ [Str "abc3231929"]

doorId :: String
doorId = "cxdnnyjw"

password :: String -> String
password doorId
  = foldMap (printHex . sixthHex)
  . take 8
  . filter zeroPrefix
  . map (abcdA . md5 . Str . (doorId <>) . show)
  $ [0..]
