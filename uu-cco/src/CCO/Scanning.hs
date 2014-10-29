-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Scanning
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A library of scanner combinators.
--
-------------------------------------------------------------------------------

module CCO.Scanning (
  ) where

import Control.Applicative (Applicative (..), (<$>), Alternative (..))

-------------------------------------------------------------------------------
-- The Scanner type
-------------------------------------------------------------------------------

data Scanner a = S (Action a) [(Char -> Bool, Scanner (Char -> a))]

instance Functor Scanner where
  fmap f (S act d) = S (fmap f act) (fmap f' d)
    where
      f' (p, s) = (p, fmap (f .) s)

instance Applicative Scanner where
  pure x = S (pure x) []

  S None df  <*> r = S None [(p, flip <$> s <*> r) | (p, s) <- df]
  S af@Ignore df <*> r@(S ax dx) = S (af <*> ax) $
    [(p, S Ignore [] <*> s) | (p, s) <- dx] ++
    [(p, flip <$> s <*> r) | (p, s) <- df]
  S af@(Return f) df <*> r@(S ax dx) = S (af <*> ax) $
    [(p, (f .) <$> s) | (p, s) <- dx] ++ [(p, flip <$> s <*> r) | (p, s) <- df]

instance Alternative Scanner where
  empty               = S None[]
  S ax dx <|> S ay dy = S (ax <|> ay) (dx ++ dy)

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

data Action a = None
              | Return a
              | Ignore

instance Functor Action where
  fmap _ None       = None
  fmap f (Return x) = Return (f x)
  fmap f Ignore     = Ignore

instance Applicative Action where
  pure x = Return x
  
  -- <*>: None dominates Return and Ignore; Ignore dominates Return.
  None <*> _            = None
  _    <*> None         = None
  Return f <*> Return x = Return (f x)
  Ignore <*> _          = Ignore
  _ <*> Ignore          = Ignore

instance Alternative Action where
  empty = None

  -- <|>: left-biased; Return dominates None and Ignore; Ignore dominates None.
  None <|> ay                     = ay
  ax@(Return _) <|> _             = ax
  _             <|> ay@(Return _) = ay
  ax            <|> _             = ax