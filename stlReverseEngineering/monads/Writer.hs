-----------------------------------------------------------
--                        Notes
--
-- This one went after reader implementation,
-- and it got easiser, both because there
-- is no function inside the type, but just
-- a tuple (less logical hops needed) and
-- this is the second monad reverse engineered
-- after a long period of time not writing haskell
--
-- confusions:
-- tell function type confused me at first,
-- I was thinking how could I return nothing ()
-- for the computation (of type a), and not ruin
-- the work done before `tell` invocation?
-- Well, even though it will return a reader
-- with a void computaion value, we won't need it!
-- The main thing with monads so far is that
-- we CAN stack up multiple functions that will
-- be returning Writer whithin another function
-- that itself returs Writer, thus allowing us to access
-- agruments of functions that are higher in the
-- nesting hierarchy. In the do notation this
-- is actually exactly how we get access to the
-- `unwraped` actions, also called `binded` values.
--
-- To sum up - there is no better way to understand
-- shit other than reverse engineering that shit
-----------------------------------------------------------


module Writer (
    Writer(..),
    tell
) where

newtype Writer l a = Writer {
    runWriter :: (a, l)
}

instance (Monoid l) => Functor (Writer l) where
    fmap f (Writer (a, l)) = Writer (f a, l)

instance (Monoid l) => Applicative (Writer l) where
    pure a = Writer (a, mempty)
    (Writer (f, l1)) <*> (Writer (a, l2)) =
        Writer (f a, l1 `mappend` l2)

tell :: l -> Writer l ()
tell message = Writer ((), message)

instance (Monoid l) => Monad (Writer l) where
    -- Writer w a   >>= (a -> Writer w b) -> Writer w b
    return = pure
    Writer (a, log') >>= f = do
        let (a', log'') = runWriter $ f a
        Writer (a', log' `mappend` log'')


-----------------------------------------------------------
--                       Tests
-----------------------------------------------------------


factorial :: Int -> Writer String Int
factorial n
    | n == 0 = Writer (1, "base case, returning 1 ")
    | otherwise = factorial (n - 1) >>= (\subResult ->
        tell ("multipliying by " ++ show n) >>= (\_ ->
            return $ subResult * n
        ))

-- output
-- (24,"base case, returning 1 multipliying by 1
--   multipliying by 2multipliying by 3multipliying by 4")

factorial' :: Int -> Writer String Int
factorial' n
    | n == 0 = Writer (1, "base case, returning 1 ")
    | otherwise = tell ("multipliying by " ++ show n) >>=
        (\_ -> factorial' (n - 1) >>= (\subResult ->
            return $ subResult * n
        ))

-- output
-- (24,"multipliying by 4multipliying by 3
--   multipliying by 2multipliying by 1base case, returning 1 ")


testFactorial :: Int -> (Int -> Writer String Int) -> (Int, String)
testFactorial arg implementation = runWriter $ implementation arg
