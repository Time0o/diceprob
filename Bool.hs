module Bool where

fromBool :: Bool -> Integer
fromBool = toInteger . fromEnum

and' a b = fromBool $ (a /= 0) && (b /= 0)
or' a b  = fromBool $ (a /= 0) || (b /= 0)
not' a   = fromBool $ not (a /= 0)

equal a b    = fromBool $ a == b
notEqual a b = fromBool $ a /= b
smaller a b  = fromBool $ a < b
greater a b  = fromBool $ a > b
atLeast a b  = fromBool $ a >= b
atMost a b   = fromBool $ a <= b
