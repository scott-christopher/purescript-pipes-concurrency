module Pipes.Concurrent.Output
    ( Output(Output)
    , send
    ) where

import Prelude
import Control.Monad.Aff.AVar (AffAVar)
import Data.Either (Either(Left, Right))
import Data.Functor.Contravariant (class Contravariant)
import Data.Functor.Contravariant.Divisible (class Decide, class Decidable, class Divisible, class Divide)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(Tuple))
import Unsafe.Coerce (unsafeCoerce)



data Void

newtype Output e a = Output (a -> AffAVar e Boolean)

send :: forall a e. Output e a -> a -> AffAVar e Boolean
send (Output f) = f

instance outputSemigroup :: Semigroup (Output e a) where
    append (Output f1) (Output f2) = Output (\a -> (||) <$> f1 a <*> f2 a)

instance outputMonoid :: Monoid (Output e a) where
    mempty = Output (\_ -> return false)

instance outputContravariant :: Contravariant (Output e) where
    cmap f (Output a) = Output (a <<< f)

instance outputDivide :: Divide (Output e) where
    divide f (Output f1) (Output f2) = Output $ \a -> case f a of
        Tuple b c -> (||) <$> f1 b <*> f2 c

instance outputDivisible :: Divisible (Output e) where
    conquer = mempty

instance outputDecide :: Decide (Output e) where
    decide f (Output f1) (Output f2) = Output $ \a -> case f a of
        Left  b -> f1 b
        Right c -> f2 c

instance outputDecidable :: Decidable (Output e) where
    lose f = Output (absurd <<< f)
        where absurd = unsafeCoerce :: forall a. Void -> a
