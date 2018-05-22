module WriterT (WriterT(..))
where

-- I think the key here was to remember that
-- monadic functions can only be binded
-- if the type of the monad returned by each
-- is always the same (obviously the final
-- monad will be of the same type as the
-- intial one). The lift function
-- helps here. One way to add this `liftable`
-- behaivior is to make the xxxT monad
-- an instance of MonadTrans class,
-- which requires the implementation of lift.
--
-- From what I remember, there are also special
-- types of lift functions that lift a computation
-- from a specific monad. Such as IO:
--
--     liftIO :: IO a -> m a
--
-- Since IO often times lives on the very
-- bomttom of the nested monad, the implementation
-- in one of the transformer monads that is an instance
-- of MonadIO class (requiring liftIO implementation)
-- lookes something like this:
--     liftIO = lift . liftIO
--     or
--     liftIO m = lift . liftIO m
--
-- also some information of lifting from
-- Haskell Wiki
--     We usually start with a (covariant) functor,
--     for simplicity we will consider the Pair functor first.
--     Haskell doesn't allow a type Pair a = (a, a)
--     to be a functor instance, so we define our own
--     Pair type instead.
--
--     data Pair a = Pair a a deriving Show
--     instance Functor Pair where
--         fmap f (Pair x y) = Pair (f x) (f y)
--
--     If you look at the type of fmap
--     (Functor f => (a -> b) -> (f a -> f b)
--     ), you will notice that fmap
--     already is a lifting operation: It transforms
--     a function between simple types a
--     and b into a function between pairs of these types.
--
--     lift :: (a -> b) -> Pair a -> Pair b
--     lift = fmap
--
--     plus2 :: Pair Int -> Pair Int
--     plus2 = lift (+2)
--     -- plus2 (Pair 2 3) ---> Pair 4 5
--
-- Somewhere along this point I have realized that my
-- implementation is not only incorrect (although somehow
-- I was able to get it compiled), but the way I
-- was thinking about stacking up monads was wrong.
--
-- My understanding was that a transformer monad
-- provides the same type of functionality as its
-- monad sibling, but with an additional monad
-- (the type of which we pass to the type constructor)
-- INSTEAD of a normal value that is being passed
-- around when binding monadic functions. It turns out
-- that the hierarchy is different, it is `flipped`,
-- so the sibling monad is kind of inside the other
-- monad, over which type our xxxT type is parametrized.
--
-- for instance my previous implementation looked like this:
--     newtype WriterT l m a = WriterT { runWriterT :: (m a, l) }
-- vs:
--     newtype WriterT l m a = WriterT { runWriterT :: m (a, l) }
--
-- So when I read "usually IO or Identity monads
-- are on the `bottom`", the way I understood `bottom`
-- is very literally.
--
-- Ok, so this the WriterT implementation does not make
-- sense, but I cannot quite understand why at this point.
-- Especially after making a small pause and seen that
-- both StateT, ReaderT are kind of implemented in the
-- was I was thinking before.
--
-- So, I guess it does not really matter where the m type
-- goes whithin the monad. the only thing that matters, is
-- that when you are using xxxT monad as your topmost monad
-- (type of your final do expression), the functionallity
-- you should have is exactly the one of xxx monad.
-- Also this totally makes sence if you think about
-- xxx a == xxxT a Identity
-- So Identity would just wrap computations
-- without adding any features (even lifting
-- Identity won't help, since it is designed
-- to be useless beeeatch).
--
-- And the second most importand thing is that
-- if you want to access the functionallity of
-- the inner monad, you would need to use lift.
-- But what if you have, lets say, 2 level monad?
-- Like this: StateT [String: Int] ReaderT String IO ()??
-- well, if you would like to use the features of Read monad
-- you would only need to to one `lift` call, but if
-- you were to access IO?
-- lets say something like `getLine`?
--
-- foo :: StateT [String: Int] (ReaderT String (IO ()))
-- foo = do
--     currentState <- get
--     currentEnvironment <- lift ask
--     inputLine <- lift $ lift getLine
--
-- This might get real shitty real soon, so mtl
-- (Monad Transformer Library) came up with a pretty
-- cool idea of creating additional classes for
-- monads defined within the library such as IO, Reader, Writer, State
-- (MonadIO, MonadReader, MonadWriter, MonadState, MonadXXX)
-- that require you to implement functions
-- of the correspoding monad XXX to become an instance
-- (which will just be done by lifting inner monads, so
-- no boilerplate logic needed). Then made both XXX and XXXT
-- instances of MonadXXX, moreover, when new monad
-- transformers are created that wrap around
-- a monad that is an instance of MonadXXX, one should
-- also make it an instance of MonadXXX, to keep the chain
-- running. This will allow to transform the example
-- given above to something more consise:
--
-- foo = do
--     currentState <- get
--     currentEnvironment <- ask -- no more lift needed,
--                               -- since now StateT is
--                               -- an instance of MonadReader
--                               -- <-> provides ask :: StateT s m a
--     inputLine <- liftIO getLine -- with liftIO it is a little
--                                 -- bit different, since there
--                                 -- is no additional function
--                                 -- call to return IO a, but instead
--                                 -- most of the time we would want
--                                 -- to just give an IO action (monad)
--                                 -- so we need something to do the lifting
--                                 -- for us - liftIO helps with that
--
-- but honestly this article will probably have a better
-- explanation than I have came up with, even for the future me lol
--
-- https://wiki.haskell.org/Monad_Transformers_Explained
--
-- just one quote that I want to explicitly include here:
--
-- "Monad transformers are like onions. At first, they make you cry but then
-- you learn to appreciate them. Like onions, they're also made of layers.
-- Each layer is the functionality of a new monad, you lift monadic
-- functions to get into the inner monads and you have transformerised
-- functions to unwrap each layer. They're also like a present in
-- that regard: in this example we unwrapped the outer wrapping paper
-- to get to the present: an object of type IO (), which lets us
-- make Haskell do something."
--
-- and I hope I am almost done crying
--
-- the fun part is that lift should not be used in any of the instance
-- declarations :( And I have used it like starting from Functor
-- up to Monad lol


class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

newtype WriterT l m a = WriterT { runWriterT :: m (a, l) }

-- no lifts are actually needed to perform any of this
-- fuuuuuck, still crying

instance (Monad m, Monoid l) => Functor (WriterT l m) where
    -- (a -> b) -> WriterT l m a -> WriterT l m b
    fmap f writerT = WriterT $
        let m = runWriterT writerT
        in fmap (\(a, l) -> (f a, l)) m


instance (Monad m, Monoid l) => Applicative (WriterT l m) where
    pure a = WriterT $ return (a, mempty)
    -- (WriterT m ((a -> b), l)) <*> (WriterT m (a, l)) -> WriterT m (b, l)
    (WriterT mf) <*> (WriterT ma) = WriterT $ do
        (f, l) <- mf
        (a, l') <- ma
        return (f a, l `mappend` l')

instance (Monad m, Monoid l) => Monad (WriterT l m) where
    -- >>= :: WriterT m (a, l) -> (a -> WriterT m (b, l)) -> Writer m (b, l)
    (WriterT m) >>= f = WriterT $ do
        (a, l) <- m
        let writer' = f a
        (b, l') <- runWriterT writer'
        return (b, l `mappend` l')

instance (Monoid l) => MonadTrans (WriterT l) where
    lift m = WriterT $ m >>= (\a -> return (a, mempty))

tell :: (Monoid l, Monad m) => l -> WriterT l m ()
tell message = WriterT $ return ((), message)

readInt :: IO Int
readInt = fmap read getLine

annotateInput :: WriterT String IO ()
annotateInput = do
    tell "reading next int: \n"
    -- a <- readInt, this would not work!, need to lift monad
    a <- lift readInt
    if a > 10 then
        tell "this thing is huge! Please stop\n"
    else do
        tell "not impressed, next please\n"
        annotateInput

-- input:
-- 4
-- 6
-- 10
-- 11
-- output:
-- ((),"reading next int: \nnot impressed, next please\nreading next int: \nnot impressed, next please\nreading next int: \nnot impressed, next please\nreading next int: \nthis
-- thing is huge! Please stop\n")
