-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Feedback.Message
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Log, warning, and error messages.
--
-------------------------------------------------------------------------------

module CCO.Feedback.Message (
    -- * Messages
    Message (Log, Warning, Error)
  , isError                          -- :: Message -> Bool
  , fromMessage                      -- :: Message -> Doc
  , fatalizeWarnings                 -- :: [Message] -> [Message]
  , filterMessages                   -- :: Int -> Int -> [Message] -> [Message]
  , putMessages                      -- :: Handle -> [Message] -> IO ()
) where

import CCO.Printing (Doc, render_, renderHeight_)
import System.IO    (Handle, hPutStrLn)

-------------------------------------------------------------------------------
-- Messages
-------------------------------------------------------------------------------

-- | Type of messages.
-- Each @Message@ holds a pretty-printable document in which the text for the
-- message is stored.
data Message
  = Log Int Doc        -- ^ A log message at a specified verbosity level, the
                       --   default level being 1.
  | Warning Int Doc    -- ^ A warning message at a specified severity level,
                       --   the default level being 1.
  | Error Doc          -- ^ An error message.

-- | Indicates whether a 'Message' is an 'Error' message.
isError :: Message -> Bool
isError (Error _) = True
isError _         = False

-- | Retrieves the 'Doc' stored in a 'Message'.
fromMessage :: Message -> Doc
fromMessage (Log _ doc)     = doc
fromMessage (Warning _ doc) = doc
fromMessage (Error doc)     = doc

-- | Turns 'Warning' messages into 'Error' messages.
fatalizeWarnings :: [Message] -> [Message]
fatalizeWarnings = map f
  where
    f (Warning _ doc) = Error doc
    f msg             = msg

-- | Filters 'Message's that do not exceed specified verbosity and severity
-- levels.
filterMessages :: Int -> Int -> [Message] -> [Message]
filterMessages v w = filter p
  where
    p (Log v' _)     = v' <= v
    p (Warning w' _) = w' <= w
    p _              = True

-- | Pretty prints the 'Doc' stored in a 'Message' onto a 'Handle'.
putMessages :: Handle -> [Message] -> IO ()
putMessages h = putMsgs
  where
    putMsgs []           = return ()
    putMsgs [msg]        = hPutStrLn h (render_ 79 (fromMessage msg))
    putMsgs (msg : msgs) = do
      let (s, height) = renderHeight_ 79 (fromMessage msg)
      hPutStrLn h s
      if height >= 0 then hPutStrLn h "" else return ()
      putMsgs msgs