{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-          ______
       .d$$$******$$$$c.
    .d$P"            "$$c
   $$$$$.           .$$$*$.
 .$$ 4$L*$$.     .$$Pd$  '$b
 $F   *$. "$$e.e$$" 4$F   ^$b
d$     $$   z$$$e   $$     '$.
\$P     `$L$$P` `"$$d$"      $$
\$$     e$$F       4$$b.     $$
\$b  .$$" $$      .$$ "4$b.  $$
\$$e$P"    $b     d$`    "$$c$F
'$P$$$$$$$$$$$$$$$$$$$$$$$$$$
 "$c.      4$.  $$       .$$
  ^$$.      $$ d$"      d$P
    "$$c.   `$b$F    .d$P"
      `4$$$c.$$$..e$$P"
          `^^^^^^^`          -}

module FISQ (fisqrt) where

import GHC.Exts (Float (F#), Int (I#), minusWord#, uncheckedShiftRL#)
import GHC.Float (stgFloatToWord32, stgWord32ToFloat)
import GHC.Num (Num ((*), (-)))
import GHC.Word (Word32 (W32#))
import Numeric (Floating (..))

-- uses the quake fast inverse square root algorithm
fisqrt :: Float -> Float
fisqrt z@(F# f#) = y * (1.5 - (x2 * y * y))
  where
    x2 = 0.5 * z
    (W32# o#) = 0x5f3759df
    (I# i#) = 1
    a# = stgFloatToWord32 f#
    b# = uncheckedShiftRL# a# i#
    c# = minusWord# o# b#
    y = F# (stgWord32ToFloat c#)


-- uses the same algorithm as haskell itself for the sqrt, but negative
f2isqrt :: Float -> Float
f2isqrt z = exp (log (z * (-0.5)))