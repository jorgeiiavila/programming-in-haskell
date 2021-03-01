qsort [] _ = []
qsort (x:xs) reverse = qsort xss reverse ++ [x] ++ qsort ys reverse
               where
                    getOpr False = (<=)
                    getOpr True = (>)
                    xss = [a | a <- xs, (getOpr reverse) a x]
                    ys = [b | b <- xs, (getOpr (not reverse)) b x]