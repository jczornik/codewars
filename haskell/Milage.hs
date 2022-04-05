import Control.Monad.RWS

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

canBe :: Integer -> Integer -> Bool
canBe x y = x >= y

followedByZeros :: Integer -> Bool
followedByZeros x = read (tail $ show x) == 0

allSame :: Integer -> Bool
allSame x = foldMap (`compare` head numbers) numbers == EQ
  where
    numbers :: String
    numbers = show x

followingDigits :: Integer -> Bool
followingDigits = followingDigits' . show
  where
    followingDigits' :: String -> Bool
    followingDigits' [] = True
    followingDigits' [_] = True
    followingDigits' ('0' : xs)
      | not (null xs) = False
    followingDigits' ('9' : '0' : zs) = followingDigits' $ '0' : zs
    followingDigits' (x : y : zs)
      | succ x == y = followingDigits' $ y : zs
      | otherwise = False

followingDigitsDesc :: Integer -> Bool
followingDigitsDesc = followingDigits' . show
  where
    followingDigits' :: String -> Bool
    followingDigits' [] = True
    followingDigits' [_] = True
    followingDigits' ('0' : xs)
      | not (null xs) = False
    followingDigits' (x : y : zs)
      | succ y == x = followingDigits' $ y : zs
      | otherwise = False

isPalindrom :: Integer -> Bool
isPalindrom x = s == reverse s
  where
    s = show x

isAwesome :: Integer -> [Integer] -> Bool
isAwesome x xs = x `elem` xs

rules =
  [ followedByZeros,
    allSame,
    followingDigits,
    followingDigitsDesc,
    isPalindrom
  ]

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
  | canBe x 100 && f x xs == Any True = Yes
  | canBe x 99 && f (x + 1) xs == Any True || canBe x 98 && f (x + 2) xs == Any True = Almost
  | otherwise = No
  where
    f y ys = foldMap (Any . ($ y)) rules <> Any (isAwesome y ys)
