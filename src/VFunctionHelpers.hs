module VFunctionHelpers where
import VFunctions (VFunction (..), simplifyOne, simplifyTwo, simplifyThree)

    
addTermWithOp :: Num b => (VFunction b a -> VFunction b a -> VFunction b a) -> a -> b -> VFunction b a -> VFunction b a
addTermWithOp op v x f = op f (Constant x * Variable v)

addLinearTerm :: Num b => a -> b -> VFunction b a -> VFunction b a
addLinearTerm = addTermWithOp (+)


collapse :: (Eq a, Ord b, Floating b) => VFunction b a -> VFunction b a
collapse f
  | f' == f = f
  | otherwise = collapse f'
  where
    f' = simplify f

simplify :: (Eq a,  Floating b, Ord b) => VFunction b a -> VFunction b a
simplify (OneIn op f1) = simplifyOne op (simplify f1)
simplify (TwoIn op f1 f2) = simplifyTwo op (simplify f1) (simplify f2)
simplify (ThreeIn op f1 f2 f3) = simplifyThree op (simplify f1) (simplify f2) (simplify f3)
simplify a = a