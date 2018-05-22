module State (
    State(..)
) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f state = State $ (\s ->
            let (a, s') = runState state s
            in (f a, s')
        )

instance Applicative (State s) where
    pure a = State (\s -> (a, s))
    -- (<*>) :: State (s -> (a -> b, s)) State (s -> (a, s)) -> State (s -> (b, s))
    s1 <*> s2 = State (\s ->
            let (f, s') = runState s1 s
                (a, s'') = runState s2 s'
            in (f a, s'')
        )

instance Monad (State s) where
    state >>= f = State $ \s ->
        let (a, s') = runState state s
            (a', s'') = runState (f a) s'
        in (a', s'')


get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)


countMatches :: Int -> State (Int, [Int]) Int
countMatches x = do
    (counter, array) <- get
    if counter >= (length array)
        then return 0
        else do
            put (counter + 1, array)
            newCount <- countMatches x
            if x == (array !! counter)
                then return $ newCount + 1
                else return newCount
