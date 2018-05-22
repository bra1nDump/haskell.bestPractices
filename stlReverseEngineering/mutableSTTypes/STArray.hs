-- Alright, so this thing is reeealy weird.

-- I guess that I had (and am still having)
-- the hardest time understanding how does this
-- work intermally, but the high level idea is
-- this:
--   In order to mutate something in haskell
--   we have to either copy-with-modification
--   and remain pure, or for real carry out
--   our side effect in some kind of global
--   state monad. The most understood by me
--   example of such a monad is IO monad,
--   which allows us to interact with the real
--   world by allowing us to directly interact
--   with components outside of our haskell code
--   i.e. make networking calls, write/read
--   to/from our file system and so on.

--   Now if we think about our goal -
--   truly mutate some state, we can see how
--   this can be achieved using IO monad.

--   A very common task that cannot be performed
--   efficiently without in place mutation is
--   updating an element of a contiguous array
--   at a given index. Well, if we are carrying out
--   computations whithin IO monad, we could
--   easily create a file, write our initial
--   array into it and then when we would like
--   to update an element in the array at a given
--   index we would just calculate the offset
--   (pure computation), set the cursor in the
--   file at that offset and do our write at O(1)
--   time complexity.

--   This was just a simple example of this approach,
--   but even thogh it sounds great, it might give
--   us exessive freedom in terms of what we could do.
--   And what would be the point of using a purely
--   functional language if just because of trying to
--   speed up a small part of some given function
--   by using truly mutable types we
--   would have to lift all of the computations
--   whithin that function to the IO monad?
--   Neither will this look neat, nor will it
--   be safe to assume that one of the fellow
--   developers in the team will not take advantage
--   of seeing the IO in the signature of the
--   function and start throwing other IO code
--   non related to the only data structure
--   you have added IO because of.

--   This introduces the need to somehow
--   not include any global state monad
--   into our top level binding type, but still
--   be able to create and then mutate some data
--   in this binding. Here comes the ST monad.
--   ST monad stores the state of the type
--   that is not accessible to the user outside of
--   of the monad. Moreover, type variable s in
--   ST s a is enforced to be any type (by using
--   forall s .) language directive, which ensures
--   that it will be impossible to extract the actual
--   state, you don't even know what type is it!

--   So when working with mutable data types,
--   one creates sort of a black box monad (ST s) which
--   will store (index over a state) the mutable data in
--   its hidden state of unknown type and allow to apply
--   modifications as long as one is whithin the monad
--   (you have to have access to that `dirty` state
--   in order to modify it). Once all the nessesary
--   modifications to the data are done, one can extract
--   the `frozen` data and let it runaway from the s (in
--   ST s) state, so it can be further used in your pure
--   code.

--   The main (and so far the only I understand) difference
--   between ST s and IO monads is that ST does not allow
--   you to interact with its state directly, instead
--   you interact with it through the mutable data type
--   interface that requires all of its computations to
--   run in ST s monad (in other words all of the
--   expression types will have a final type of ST s something)
--   i.e.:
--     if we have a look at the STUArray that i was using
--     in this example we can see that we are using the
--     interface of MArray (Mutable array) class. And if
--     now we take a look at the final type of the bindings
--     we can see that they are of type ST s desiredType.
--     this is nessesary to `force` all of the functions
--     of this type to be called whithin the ST monad.

--     Since once we exit the ST s context (if we were
--     to return a type whitout ST s prefix), the state
--     becomes inaccessible, and as we know it is needed
--     to store our data. Examples of MArray class
--     instance STArray

--     MArray (STArray s) e (ST s) where
--       getBounds :: Ix i => STArray s i e -> ST s (i, i)
--       getNumElements :: Ix i => STArray s i e -> ST s Int
--       newArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
--       newArray_ :: Ix i => (i, i) -> ST s (STArray s i e)
--       unsafeNewArray_ :: Ix i => (i, i) -> ST s (STArray s i e)
--       unsafeRead :: Ix i => STArray s i e -> Int -> ST s e
--       unsafeWrite :: Ix i => STArray s i e -> Int -> e -> ST s ()

--    Notice that this class is parametrized over the
--    type of elements that can be stored in the array
--    (e), this is what is commonly reffered to a `boxed`
--    type. But there are also unboxed alternatives for
--    unboxed types (fixed base type that the language
--    uses very efficiently internally). i.e.:
--       instance MArray (STUArray s) Int64 (ST s)
--    would contain specialixed for Int64 type
--    function implementations
--    required by MArray class. These specialized implementations
--    are usually more efficient in terms of memory
--    and execution times.
--
--  This is a somewhat condesed version of my understanding
--  of how do mutable types work in haskell with help of
--  ST s monad. (If this was done correctly, one shoudl
--  just be able to replace ST s with IO and all the statements
--  should remain valid, since there are alternative types
--  for IO monad, such as: IOArray IOUArray and they
--  are also instances of MArray class.
----------------------------------------------------------------------

import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

test :: Array Int Bool
test = runSTArray $ do
  array <- newArray (1,10) False  -- :type ST s (STArray s i e)
  writeArray array 2 True         -- :type ST s ()
  return array                    -- :type ST s (STArray s i e)

-- Also notice that we never fix the type variable
-- s, even though it is used in the STArray type
-- constructor.

main :: IO ()
main = do
  let frozenArray = test
  mapM_ (\value -> print value) $ elems frozenArray
