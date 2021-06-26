-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Feedback
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A monad for keeping track of log, warning, and error messages.
--
-------------------------------------------------------------------------------

module CCO.Feedback (
    -- * Messages
    Message (Log, Warning, Error)
  , isError                          -- :: Message -> Bool
  , fromMessage                      -- :: Message -> Doc

    -- * The Feedback monad
  , Feedback                         -- abstract, instances: Functor, Monad
  , trace                            -- :: Int -> String -> Feedback ()
  , trace_                           -- :: String -> Feedback ()
  , warn                             -- :: Int -> String -> Feedback ()
  , warn_                            -- :: String -> Feedback ()
  , errorMessage                     -- :: Doc -> Feedback ()
  , message                          -- :: Message -> Feedback ()
  , messages                         -- :: [Message] -> Feedback ()
  , wError                           -- :: Feedback a -> Feedback a
  , succeeding                       -- :: Feedback a -> Bool
  , failing                          -- :: Feedback a -> Bool
  , runFeedback                      -- :: Feedback a -> Handle -> IO (Maybe a)
) where

import CCO.Feedback.Message
import CCO.Printing          (Doc, text)
import System.IO             (Handle)
import Control.Monad
import Control.Applicative

-------------------------------------------------------------------------------
-- The Feedback monad
-------------------------------------------------------------------------------

-- | The @Feedback@ monad.
-- Keeps track of 'Message's, failing if an 'Error' message is encountered.
data Feedback a
  = Succeed [Message] a
  | Fail [Message]

instance Functor Feedback where
  fmap f (Succeed msgs x) = Succeed msgs (f x)
  fmap _ (Fail msgs)      = Fail msgs

instance Monad Feedback where
  return x = Succeed [] x

  Succeed msgs x >>= f = case f x of
                           Succeed msgs' y -> Succeed (msgs ++ msgs') y
                           Fail msgs'      -> Fail (msgs ++ msgs')
  Fail msgs      >>= _ = Fail msgs

instance MonadFail Feedback where
  fail msg             = Fail [Error (text msg)]

instance Applicative Feedback where
  pure = return
  (<*>) = ap

-- | Issues a list of 'Message's.
-- Fails if the list contains an 'Error' message.
messages :: [Message] -> Feedback ()
messages msgs | any isError msgs = Fail msgs
              | otherwise        = Succeed msgs ()

-- | Issues a 'Message'.
-- Fails if an 'Error' message is issued.
message :: Message -> Feedback ()
message msg = messages [msg]

-- | Issues an 'Error' message.
errorMessage :: Doc -> Feedback a
errorMessage doc = Fail [Error doc]

-- | Issues a 'Log' message at a specified verbosity level containing a
-- specified text.
trace :: Int -> String -> Feedback ()
trace v = message . Log v . text

-- | Issues a 'Log' message at the default verbosity level 1 containing a
-- specified text.
trace_ :: String -> Feedback ()
trace_ = trace 1

-- | Issues a 'Warning' message at a specified severity level containing a
-- specified text.
warn :: Int -> String -> Feedback ()
warn w = message . Warning w . text

-- | Issues a 'Warning' message at the default severity level 1 containing a 
-- specified text.
warn_ :: String -> Feedback ()
warn_ = warn 1

-- | Turns all 'Warning' messages into 'Error' messages.
wError :: Feedback a -> Feedback a
wError (Fail msgs)      = Fail (fatalizeWarnings msgs)
wError (Succeed msgs x) = let msgs' = fatalizeWarnings msgs
                          in  if   any isError msgs'
                              then Fail msgs'
                              else Succeed msgs' x

-- | Retrieves whether a 'Feedback' computation will succeed.
succeeding :: Feedback a -> Bool
succeeding (Succeed _ _) = True
succeeding _             = False

-- | Retrieves whether a 'Feedback' computation will fail.
failing :: Feedback a -> Bool
failing (Fail _) = True
failing _        = False

-- | Runs a 'Feedback' computation at a specified verbosity and severity level,
-- pretty printing messages onto a specified
-- 'Handle'.
runFeedback :: Feedback a -> Int -> Int -> Handle -> IO (Maybe a)
runFeedback (Succeed msgs x) v w h = do let msgs' = filterMessages v w msgs
                                        putMessages h msgs'
                                        return (Just x)
runFeedback (Fail msgs)      v w h = do let msgs' = filterMessages v w msgs
                                        putMessages h msgs'
                                        return Nothing
