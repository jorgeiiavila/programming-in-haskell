i :: Double
i = 1.0

e :: Double
e = 0.00001

sqroot :: Double -> Double
sqroot n = (fst . head) (dropWhile (\(a, b) -> abs (a - b) > e) zipped)
  where
    as = iterate (`next` n) i
    zipped = zip as (tail as)

next :: Fractional a => a -> a -> a
next a n = (a + n / a) / 2
