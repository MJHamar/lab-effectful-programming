--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Effectful programming                                                 --
--------------------------------------------------------------------------------

module Lab where

import Prelude hiding (Either(..), catch, mapM)

--------------------------------------------------------------------------------
-- safediv

-- | This is our usual function for safe division, which returns Nothing if
-- the second argument is 0, or the result of dividing the first argument by
-- the second wrapped into the Just constructor instead.
safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

--------------------------------------------------------------------------------
-- Using the Maybe monad

duckily :: [(String, String)]
duckily =
    [ ("Grandduck", "Grand duckster")
    , ("Baby Duck", "Parent Duck")
    , ("Duckling", "Older duckling")
    , ("Parent Duck", "Grandduck")
    ]

-- | A function to find the grandduck of a duck in the duck family.
grandduck :: [(String, String)] -> String -> Maybe String
grandduck tbl name = lookup name tbl >>= flip lookup tbl

grandduck' :: [(String, String)] -> String -> Maybe String
grandduck' tbl name = lookup name tbl >>= \x -> lookup x tbl

grandduck'' :: [(String, String)] -> String -> Maybe String
grandduck'' tbl name = do 
    x <- lookup name tbl
    lookup x tbl

--------------------------------------------------------------------------------
-- Generic functions

-- | mapM, but without do-notation
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = f x       >>= \y ->
                mapM f xs >>= \ys ->
                return (y:ys)

-- | the same as above, but with Applicative instead of Monad
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (x:xs) = (:) <$> f x <*> mapA f xs

-- | Like zipWith, but the function returns a computation (has an effect).
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f [] _ = return []
zipWithM f _ [] = return []
zipWithM f (x:xs) (y:ys) = do 
    r  <- f x y 
    rs <- zipWithM f xs ys 
    return (r:rs) 

--------------------------------------------------------------------------------
-- Either is a Monad

-- | The Either type is defined in the Prelude and has Functor, Applicative,
-- and Monad instances there. For the purpose of this lab, we do not import it
-- and instead define it here ourselves.
data Either a b = Left a | Right b
    deriving (Eq, Show)

instance Functor (Either e) where
    fmap _ (Left x)  = Left x 
    fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
    pure = Right

    Left err <*> _ = Left err 
    Right f  <*> g = f <$> g

instance Monad (Either e) where
    Left err >>= _ = Left err 
    Right x  >>= f = f x

--------------------------------------------------------------------------------
