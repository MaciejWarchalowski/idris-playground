data Ex = Val Integer
        | Add Ex Ex
        | Sub Ex Ex
        | Mul Ex Ex

eval : Ex -> Integer
eval (Val  x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing some@(Just x) = some
maxMaybe some@(Just x) Nothing = some
maxMaybe (Just x) (Just y) = case compare x y  of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x
