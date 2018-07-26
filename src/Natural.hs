module Natural where

data Natural = Zero | Succ Natural
    deriving (Eq)

instance Show Natural where
    show = show . fromNat

fromNat :: Integral a => Natural -> a
fromNat Zero = 0
fromNat (Succ x) = 1 + (fromNat x)

instance Num Natural where
    x + y = fromInteger $ fromNat x + fromNat y
    x - y = fromInteger $ fromNat x - fromNat y
    x * y = fromInteger $ fromNat x * fromNat y
    abs n = n
    signum Zero = Zero
    signum _    = Succ Zero
    fromInteger x 
        | x < 0     = error $ "Negative numbers aren't defined in Naturals"
        | x == 0    = Zero
        | otherwise = Succ $ fromInteger (x - 1)

instance Ord Natural where
    x <= y = fromNat x <= fromNat y
