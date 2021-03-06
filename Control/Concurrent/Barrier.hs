module Control.Concurrent.Barrier
    ( Barrier

    , barrier
    , latchBarrier
    ) where

import Control.Monad (forM_)
import Control.Concurrent.MVar

type Barrier = IO ()

barrier' :: (Int -> Int) -> Int -> IO Barrier
barrier' reset count = do
  b <- newMVar (count, [])
  return $ do
    w <- modifyMVar b $ \(c, waiting) ->
           case c of
             0 -> return ((0, []), Nothing)
             1 -> do forM_ waiting $ flip putMVar ()
                     return ((reset count, []), Nothing)
             _ -> do w <- newEmptyMVar
                     return ((c-1, w:waiting), Just w)

    maybe (return ()) takeMVar w

-- | Self-resetting barrier.  'barrier' blocks until a specified
-- number of threads have reached it, and then they are all allowed to
-- run.  The barrier is then reset so that a further 'count' threads
-- can block on it.  Typical usage is:
-- 
-- > do b <- barrier 3
-- >    forkIO $ b >> putStrLn "1"  -- blocked
-- >    forkIO $ b >> putStrLn "2"  -- blocked
-- >    forkIO $ b >> putStrLn "3"  -- all three threads run
barrier :: Int -- ^ count - number of threads required before barrier is opened
        -> IO Barrier
barrier = barrier' id
{-# INLINE barrier #-}

-- | Latching barrier.  This is the same as 'barrier', except once the
-- barrier has opened (the requisite number of threads has reached
-- it), it remains open, allowing all subsequent threads through
-- unblocked.
latchBarrier :: Int -- ^ count - number of threads required before barrier is opened
             -> IO Barrier
latchBarrier = barrier' (const 0)
{-# INLINE latchBarrier #-}
