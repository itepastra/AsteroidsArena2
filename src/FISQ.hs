{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-          ______
       .d$$$******$$$$c.
    .d$P"            "$$c
   $$$$$.           .$$$*$.
 .$$ 4$L*$$.     .$$Pd$  '$b
 $F   *$. "$$e.e$$" 4$F   ^$b
d$     $$   z$$$e   $$     '$.
$P     `$L$$P` `"$$d$"      $$
$$     e$$F       4$$b.     $$
$b  .$$" $$      .$$ "4$b.  $$
$$e$P"    $b     d$`    "$$c$F
'$P$$$$$$$$$$$$$$$$$$$$$$$$$$
 "$c.      4$.  $$       .$$
  ^$$.      $$ d$"      d$P
    "$$c.   `$b$F    .d$P"
      `4$$$c.$$$..e$$P"
          `^^^^^^^`          -}

module FISQ where

import Data.Bits (shiftR)
import GHC.Float (stgFloatToWord32, stgWord32ToFloat)
import Prelude (Num (..))
import GHC.Exts (Float (F#), shiftRL#)
import GHC.Word (Word32(W32#))
import GHC.Prim
import GHC.Int (Int(I#))

fisqrt :: Float -> Float
fisqrt z@(F# f#) = y * (1.5 - (x2 * y * y))
  where
    x2 = 0.5 * z
    (W32# o#) = 0x5f3759df
    (I# i#) = 1
    a# = stgFloatToWord32 f#
    b# = shiftRL# a# i#
    c# =  minusWord# o# b#
    y = F# (stgWord32ToFloat c#)