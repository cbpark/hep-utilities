module HEP.Vector
    ( Vector(..)
    , Metric(..)
    ) where

import           Control.Applicative (Applicative, liftA, liftA2)

infixl 6 .+., .-.
infixl 7 .*, *., ./

class Applicative f => Vector f where
    zero :: Num a => f a

    negated :: Num a => f a -> f a
    negated = fmap negate

    (.+.) :: Num a => f a -> f a -> f a
    (.+.) = liftA2 (+)

    (.-.) :: Num a => f a -> f a -> f a
    u .-. v = u .+. negated v

    (*.) :: Num a => a -> f a -> f a
    (*.) x = liftA (x*)

    (.*) :: Num a => f a -> a -> f a
    v .* x = liftA (*x) v

    (./) :: Fractional a => f a -> a -> f a
    v ./ x = liftA (/x) v

class Vector f => Metric f where
    dot :: Num a => f a -> f a -> a

    norm :: Floating a => f a -> a
    norm v = sqrt (v `dot` v)

    distance :: Floating a => f a -> f a -> a
    distance u v = norm (u .-. v)
