module Reader (
    Reader(..)
) where

-- The key for me in understanding monads all over
-- again was to stop remember that monads allow us
-- to build a chain of functions with the next ones
-- beeng able to access results of previous ones,
-- while monad would be holding context of their execution.
--
-- The binding operator sort of concatinates monads with each other,
-- building one big ass monad, that can change the
-- parametrizable type along the way, but will always
-- be of the same type as the last monad chained.
--
-- With the reader monad the main difficulty was that
-- it itself holds a function. So you have to remember
-- that every time you use a <- arrow, the value bound
-- to the variable is actually of type (env -> value).
-- This is why you never see expressions of different
-- type than Reader without return (like in let statements).
-- For instance, this results in compile error:
--
--     testReader = do
--         env <- Reader (\env -> env)
--         intermediate <- env + 4
--         return $ show intermediate ++ "lool"
--
-- but with this modification it would work:
--
--     let intermediate = env + 4
--
-- ask implementation is also kind of weird. It is basically
-- just Reader const. In our convenient <- operator
-- this will explicitly bind a 'hidden' environment value
-- passed all around the monad to the following
-- expressions.
--
-- And final note, with monads, everything is kind of
-- making more sence if going `backwards` in code.
-- For instance when the composed compuation will start
-- executing, it will enter the monad that was binded
-- the latest. From there it will propagate the context
-- to the previous and previous .. binded monads.


newtype Reader s a = Reader {
    runReader :: (s -> a)
}

test :: [Int]
test = [4, 5] >>= (\x -> do
        y <- return $ x + 1
        return $ y + x + 2
    )

instance Functor (Reader s) where
    -- fmap (a -> b) -> f a -> f b
    f `fmap` m = Reader (\env ->
            let value = runReader m env
            in f value
        )

instance Applicative (Reader s) where
    -- pure :: (Applicative f) => a -> f a
    pure x = Reader (\_ -> x)
    -- Reader s (a -> b) -> Reader s a -> Reader s b
    r1 <*> r2 = Reader $ \env ->
        let f = runReader r1 env
            arg = runReader r2 env
        in f arg

instance Monad (Reader s) where
    return = pure
    -- >>= Reader s a -> (a -> Reader s b) -> Reader s b
    -- >>= (s -> a) -> (a -> (s -> b)) -> (s -> b)
    r >>= f = Reader $ \env ->
        let subcomputationValue = runReader r env
            finalComputation = f subcomputationValue
            finalValue = runReader finalComputation env
            in finalValue

ask :: Reader s s
ask = Reader id


testReader :: Reader Int String
testReader = Reader (\env -> show env ++ "lol1")
    >>= (\previousComputation ->
        Reader (\env -> "this is env again: "
            ++ show env ++ "and how prev computation"
            ++ previousComputation))

testReader' :: Reader Int String
testReader' = do
    previousComputation <- Reader (\env -> show env ++ "lol1")
    finalComputation <- Reader (\env -> "this is env again: "
        ++ show env ++ "and how prev computation"
        ++ previousComputation)
    return finalComputation

testReader'' :: Reader Int String
testReader'' = do
    env <- Reader (\env -> env)
    env1 <- Reader id
    env2 <- ask
    let intermediate = env + 4
    return $ show intermediate ++ "lool"

-- [5, 6, 4] >>= (\x -> [x + 1]) >>= (\y -> [y + 1])
--
-- do
--     x <- [5, 6, 4]
--     y <- return x + 1
--     return y + 1
--
-- [5, 6, 4] >>= (\x -> [x + 1] >>= (\y -> [y + x + 1]))
