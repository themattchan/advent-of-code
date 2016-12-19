import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Hash.MD5
import Data.Word
import Data.Monoid
import Data.Bits
import Text.Printf

abcdA :: ABCD -> Word32
abcdA (ABCD (a,_,_,_)) = a

printHex :: Word32 -> String
printHex n = printf "%x" n

zeroPrefix :: Word32 -> Bool
zeroPrefix x = (x .&. 0x00f0ffff) == 0

sixthHex :: Word32 -> Word32
sixthHex x = (x `shift` (-16)) .&. 0xf

seventhHex :: Word32 -> Word32
seventhHex x = (x `shift` (-28)) .&. 0xf

doorId :: String
doorId = "cxdnnyjw"

password1 :: String -> String
password1 doorId
  = foldMap (printHex . sixthHex)
  . take 8
  . filter zeroPrefix
  . map (abcdA . md5 . Str . (doorId <>) . show)
  $ [0..]

password2 :: String -> String
password2 doorId
  = foldMap printHex . mconcat $ unfoldr go (0, S.fromList [0..7])
  where
    go (i,r)
      | S.null r = Nothing
      | otherwise = Just (h, (i+1, r'))
      where
        hash   = (abcdA . md5 . Str . (doorId <>)) (show i)
        x      = sixthHex hash
        y      = seventhHex hash
        (h,r') | zeroPrefix hash && x `S.member` r
               = (M.singleton x y, x `S.delete` r)
               | otherwise
               = (M.empty, r)
