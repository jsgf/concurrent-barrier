{-# LANGUAGE DoAndIfThenElse #-}
module Control.Concurrent.Barrier
    ( barrier
    , latchBarrier
    ) where

import Control.Monad (forM_)
import Control.Concurrent.MVar

barrier' :: (Int -> Int) -> Int -> IO (IO ())
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

-- | Self-resetting barrier.  'barrier' blocks until 'count' threads hit
-- it, and then they are all allowed to run.  The barrier is then
-- reset so that a further 'count' threads can block on it.
-- Typical usage is:
-- 
-- > do b <- barrier 3
-- >    forkIO $ b >> putStrLn "1"  -- blocked
-- >    forkIO $ b >> putStrLn "2"  -- blocked
-- >    forkIO $ b >> putStrLn "3"  -- all three threads run
barrier :: Int -> IO (IO ())
barrier = barrier' id

-- | Latching barrier.  This is the same as 'barrier', except once the
-- barrier has opened (the requisite number of threads has reached
-- it), it remains open, allowing all subsequent threads through
-- unblocked.
latchBarrier :: Int -> IO (IO ())
latchBarrier = barrier' (\_ -> 0)
