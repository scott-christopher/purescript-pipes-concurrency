module Pipes.Concurrent
    ( module Pipes.Concurrent.Input
    , module Pipes.Concurrent.Output
    , Buffer
    , fromInput
    , spawn'
    , spawn
    , toOutput
    , unbounded
    ) where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Aff (finally, later)
import Control.Monad.Aff.AVar (AffAVar, modifyVar, makeVar', putVar, makeVar, takeVar, AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (fst, Tuple(Tuple))
import Pipes (await, yield)
import Pipes.Concurrent.Input (Input(Input), recv)
import Pipes.Concurrent.Output (Output(Output), send)
import Pipes.Core (Consumer_, Producer_)


toOutput :: forall a e m. MonadAff (avar :: AVAR | e) m => Output e a -> Consumer_ a m Unit
toOutput output = loop
    where
        loop = do
            a     <- await
            alive <- lift $ liftAff $ send output a
            when alive loop

fromInput :: forall a e m. MonadAff (avar :: AVAR | e) m => Input e a -> Producer_ a m Unit
fromInput input = loop
    where
        loop = do
            ma <- lift $ liftAff $ recv input
            case ma of
                Nothing -> return unit
                Just a  -> yield a *> loop

spawn :: forall a e. Buffer a -> AffAVar e (Tuple (Output e a) (Input e a))
spawn buffer = fst <$> spawn' buffer

spawn' :: forall a e. Buffer a -> AffAVar e (Tuple (Tuple (Output e a) (Input e a)) (AffAVar e Unit))
spawn' buffer = do
    Tuple write read <- case buffer of
        Unbounded -> do
            q <- makeVar
            return $ Tuple (putVar q) (takeVar q)

    sealed <- makeVar' false
    let seal = modifyVar (const true) sealed

        sendOrEnd a = do b <- takeVar sealed
                         putVar sealed b
                         if b then return false
                              else (write a) *> return true

        readOrEnd = (Just <$> read) <|> (do b <- takeVar sealed
                                            putVar sealed b
                                            if b then return Nothing
                                                 else later readOrEnd)

    return $ Tuple (Tuple (Output sendOrEnd) (Input readOrEnd)) seal

withSpawn :: forall a e. Buffer a -> (Tuple (Output e a) (Input e a) -> AffAVar e a) -> AffAVar e a
withSpawn buffer action = do
    Tuple outputInput seal <- spawn' buffer
    finally (action outputInput :: AffAVar e a) seal

data Buffer a = Unbounded

unbounded :: forall a. Buffer a
unbounded = Unbounded
