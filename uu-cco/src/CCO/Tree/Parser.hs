-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A set of simple parsing utilities that facilitate the conversion from
-- 'ATerm's to 'Tree's.
-------------------------------------------------------------------------------

module CCO.Tree.Parser (
    -- * Parsing ATerms
    TreeParser        -- abstract, instance: Functor
  , integer           -- :: TreeParser Integer
  , float             -- :: TreeParser Double
  , string            -- :: TreeParser String
  , app               -- :: Con -> ArgumentParser a -> TreeParser a
  , tuple             -- :: ArgumentParser a -> TreeParser a
  , list              -- :: [TreeParser a] -> TreeParser a
  , parseTree         -- :: [TreeParser a] -> ATerm -> Feedback a

    -- * Parsing arguments
  , ArgumentParser    -- abstract, instances: Functor, Applicative
  , arg               -- :: Tree a => ArgumentParser a
) where

import CCO.Feedback                (Feedback, succeeding)
import CCO.Tree.Base               (Tree (toTree))
import CCO.Tree.ATerm              (Con, ATerm (..))
import CCO.Tree.Parser.Validation  (Scheme (..), validate)
import Control.Applicative         (Applicative (..))

-------------------------------------------------------------------------------
-- Parsing ATerms
-------------------------------------------------------------------------------

infix 4 `app`

-- | Parser that consumes 'ATerm's and, if successful, produces a value of a
-- specified type.
data TreeParser a = T Scheme (ATerm -> Feedback a)
                     -- ^ Holds the 'Scheme' of the 'ATerm' to be consumed and
                     --   the actual parsing function.

instance Functor TreeParser where
  fmap f (T scheme parse) = T scheme (fmap f . parse)

-- | A 'TreeParser' that consumes an 'Integer' term.
integer :: TreeParser Integer
integer = T IntegerTerm (\(Integer n) -> return n)

-- | A 'TreeParser' that consumes a 'Float' term.
float :: TreeParser Double
float = T FloatTerm (\(Float r) -> return r)

-- | A 'TreeParser' that consumes a 'String' term.
string :: TreeParser String
string = T StringTerm (\(String s) -> return s)

-- | A 'TreeParser' that consumes an application of a given constructor and
-- that uses a specified 'ArgumentParser' to parse the argument terms of the
-- application.
app :: Con -> ArgumentParser a -> TreeParser a
app conid argumentParser =
  T (AppTerm conid (arity argumentParser)) $ \(App _ arguments) ->
    parseArguments argumentParser arguments

-- | A 'TreeParser' that consumes a tuple term and that uses a specified
-- 'ArgumentParser' to parse the component terms of the tuple.
tuple :: ArgumentParser a -> TreeParser a
tuple argumentParser =
  T (TupleTerm (arity argumentParser)) $ \(Tuple components) ->
    parseArguments argumentParser components

-- | A 'TreeParser' that consumes a list term and that uses a specified
-- family of 'TreeParser's to parse the elements of the list.
list :: [TreeParser a] -> TreeParser [a]
list atermParsers =
  T ListTerm $ \(List elements) -> mapM (parseTree atermParsers) elements

-- | Retrieves the 'Scheme's to be consumed by a given family of
-- 'TreeParser's.
schemes :: [TreeParser a] -> [Scheme]
schemes atermParsers = [scheme | T scheme _ <- atermParsers]

-- | Uses a family of 'TreeParser's to parse a given 'ATerm'.
parseTree :: [TreeParser a] -> ATerm -> Feedback a
parseTree atermParsers (Ann a _) = parseTree atermParsers a
parseTree atermParsers aterm     = do
  scheme <- validate aterm (schemes atermParsers)
  select [parse aterm | T scheme' parse <- atermParsers, scheme' == scheme]

-------------------------------------------------------------------------------
-- Parsing arguments
-------------------------------------------------------------------------------

-- | Parser that consumes a list of 'ATerm's and, if successful, produces a
-- value of a specified type.
data ArgumentParser a = A Int ([ATerm] -> Feedback (a, [ATerm]))
                        -- ^Holds the number of 'ATerm's to be consumed and
                        -- the actual parsing function.

instance Functor ArgumentParser where
  fmap f (A n parse) = A n $ \aterms -> do (x, aterms') <- parse aterms
                                           return (f x, aterms')

instance Applicative ArgumentParser where
  pure x = A 0 $ \aterms -> return (x, aterms)

  A m parse <*> A n parse' = A (m + n) $ \aterms -> do
                               (f, aterms')  <- parse aterms
                               (x, aterms'') <- parse' aterms'
                               return (f x, aterms'')

-- | An 'ArgumentParser' that consumes a single 'ATerm', converting it to
-- a 'Tree' of the appropriate type.
arg :: Tree a => ArgumentParser a
arg = A 1 $ \(aterm : aterms) -> do x <- toTree aterm
                                    return (x, aterms)

-- | Retrieves the number of 'ATerm's to be consumed by a specified
-- 'ArgumentParser'.
arity :: ArgumentParser a -> Int
arity (A n _) = n

-- | Uses a specified 'ArgumentParser' to parse a list of 'ATerm's.
parseArguments :: ArgumentParser a -> [ATerm] -> Feedback a
parseArguments (A _ parse) = fmap fst . parse

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Selects the first succeeding 'Feedback' computation from a nonempty list.
-- Returns the first computation in the list if all computations in the list
-- are failing.
select :: [Feedback a] -> Feedback a
select us@(v : _) = sel us
  where
    sel []                      = v
    sel (u : us) | succeeding u = u
                 | otherwise    = sel us