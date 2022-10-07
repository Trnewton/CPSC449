myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a,b):(myzip as bs)

-- hzip :: [a] -> [b] -> [(a,b)]
-- hzip as bs = foldr (\b f -> f(b))
