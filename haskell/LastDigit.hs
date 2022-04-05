import Data.Char (digitToInt)

lastDigit :: Integer -> Integer -> Integer
lastDigit a b = s !! fromInteger (b `div` seqLen)
  where
    lastOf = toInteger . digitToInt . last . show
    lastOfA = lastOf a
    s = iterate (\x -> lastOf (x * lastOfA)) 1
    seqOfPower = takeWhile (== head s) s
    seqLen = toInteger $ length seqOfPower
