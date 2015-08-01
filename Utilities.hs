module Utilities where

update :: (a -> a) -> Int -> [a] -> [a]
update _ _ []     = []
update f 0 (a:as) = f a : as
update f i (a:as) = a   : update f (i-1) as
