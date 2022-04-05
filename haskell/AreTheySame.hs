-- Given two arrays a and b write a function comp(a, b) (orcompSame(a, b)) that checks whether the two arrays
-- have the "same" elements, with the same multiplicities (the multiplicity of a member is the number of times
-- it appears). "Same" means, here, that the elements in b are the elements in a squared, regardless of the order.

import Control.Monad.RWS
import Data.List

comp :: [Integer] -> [Integer] -> Bool
comp as bs = getAll $ foldMap All $ (length as == length bs) : zipWith (==) as' bs'
  where
    as' = sort $ map (^ 2) as
    bs' = sort bs
