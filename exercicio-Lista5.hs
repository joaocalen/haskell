-- num 1

sum' x xs = if(x == length xs -1) then
              xs!!x
            else
              xs!!x + sum' (x+1) xs

-- num 2

maior xs = comparaMaior 0 (xs!!0) xs
            where
              comparaMaior i x xs | i == length xs = x
                                  | xs!!i > x = comparaMaior (i+1) (xs!!i) xs
                                  | otherwise = comparaMaior (i+1) x xs

maior2 [x] = x
maior2 (x:xs) | x > head xs = maior2 ([x] ++ tail(xs))
              | otherwise = maior2 xs

-- num 8
tWhile pred xs = tWhileYs pred xs []
              where
                tWhileYs pred (x:xs) ys | pred x = tWhileYs pred xs (x:ys)
                                        | otherwise = ys

-- num 9
dWhile pred (x:xs) | pred x = dWhile pred xs
                   | otherwise = (x:xs)
