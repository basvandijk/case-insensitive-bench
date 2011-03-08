module Main where

import Data.Char (toUpper)

import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as BC8

import qualified Data.CaseInsensitive as CI

import Data.Ascii

import Criterion.Main (defaultMain, bgroup, bench, Pure, nf)

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  bs <- BC8.readFile "/usr/share/dict/words"
  let ls = BC8.lines bs
  putStrLn $ "nr of lines: " ++ show (length ls)

  defaultMain 
    [ bgroup "allEq"
      [ bench "CI Ascii" $ allEq CI.mk     ls
      , bench "CIAscii"  $ allEq toCIAscii ls
      ]
    , bgroup "totalSum"
      [ bench "CI Ascii" $ totalSum CI.mk     CI.original ls
      , bench "CIAscii"  $ totalSum toCIAscii fromCIAscii ls
      ]
    ]

--------------------------------------------------------------------------------

allEq :: Eq ci => (Ascii -> ci) -> [B.ByteString] -> Pure
allEq mk ls = nf (all eq) ls
    where
      eq l = let a1 = unsafeFromByteString l
                 a2 = unsafeFromByteString (upper l)
             in mk a1 == mk a2
      upper = BC8.map toUpper

totalSum :: (Ascii -> ci) 
         -> (ci -> Ascii) 
         -> [B.ByteString] 
         -> Pure
totalSum mk orig ls = nf (sum . map f) ls
    where
      f = B.length . toByteString . orig . mk . unsafeFromByteString

--------------------------------------------------------------------------------
