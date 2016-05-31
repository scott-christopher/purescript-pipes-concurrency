module Pipes.Concurrent.Input
    ( Input(Input)
    , recv
    ) where

import Prelude
import Control.Alt (class Alt, (<|>), alt)
import Control.Alternative (class Alternative)
import Control.Monad.Aff.AVar (AffAVar)
import Control.Plus (class Plus, empty)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(Tuple))


newtype Input  e a = Input (AffAVar e (Maybe a))

recv :: forall a e. Input e a -> AffAVar e (Maybe a)
recv (Input var) = var

instance inputFunctor :: Functor (Input e) where
    map f (Input var) = Input (map (map f) var)

instance inputApply :: Apply (Input e) where
    apply (Input varF) (Input varX) = Input ((<*>) <$> varF <*> varX)

instance inputApplicative :: Applicative (Input e) where
    pure r = Input (pure (pure r))

instance inputBind :: Bind (Input e) where
    bind (Input var) f = Input $ do
        ma <- var
        case ma of
            Nothing -> return Nothing
            Just a  -> recv (f a)

instance inputAlt :: Alt (Input e) where
    alt x y = Input $ do
        Tuple i ma <- map (Tuple y) (recv x) <|> map (Tuple x) (recv y)
        case ma of
            Nothing -> recv i
            Just a  -> return (Just a)

instance inputPlus :: Plus (Input e) where
    empty = Input (return Nothing)

instance inputAlternative :: Alternative (Input e)

instance inputSemigroup :: Semigroup (Input e a) where
    append = alt

instance inputMonoid :: Monoid (Input e a) where
    mempty = empty
