{-# LANGUAGE StrictData #-}
module State where

import Relude hiding (State, get, put, evalState, modify, splitAt, runState)


newtype MyState s g a = MyState {runState :: s -> g -> (a, s, g)}

instance Functor (MyState s g) where
    fmap :: (a -> b) -> MyState s g a -> MyState s g b
    fmap f (MyState a) = MyState g
        where
            fst3 (x, _y, _z) = x
            g s t= (f $ fst3 (a s t), s, t)

instance Applicative (MyState s g) where
    pure :: a -> MyState s g a
    pure a = MyState $ \s g -> (a, s, g)

    (<*>) :: MyState s g (a -> b) -> MyState s g a -> MyState s g b
    (MyState f) <*> (MyState a0) = MyState $ \s g -> go s g f a0
        where
            go s0 g0 fn fa = let
                -- call fn with s and t, to get the function ab and new state s1, t1
                (ab, s1, g1) = fn s0 g0
                -- call fa with s1, t1, to get value a and new states s2, t2
                (a, s2, g2) = fa s1 g1
                -- return the new states, along with the result of applying ab to a
                in (ab a, s2, g2)

instance Monad (MyState s g) where
    (>>=) :: MyState s g a -> (a -> MyState s g b) -> MyState s g b
    (!p) >>= (!f) = MyState $ \(!s) (!g) ->
        let (x, s1, g1) = runState p s g
        in runState (f x) s1 g1


evalMyState :: MyState s g a -> s -> g -> a
evalMyState st s g = a
    where
        (a, _, _) = runState st s g
