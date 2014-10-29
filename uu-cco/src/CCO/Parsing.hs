{-# LANGUAGE Rank2Types #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Parsing
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  non-portable (uses rank-2 types)
--
-- A library of parser combinators that expose their functionality through an
-- 'Applicative' interface.
--
-- Based on work by Doaitse Swierstra.
--
-------------------------------------------------------------------------------

module CCO.Parsing (
    -- * Symbols
    Symbol (describe)

    -- * Parsers
  , Parser               -- abstract, instances: Functor, Applicative,
                         -- Alternative
  , satisfy              -- :: Symbol s => (s -> Bool) -> Parser s s
  , eof                  -- :: Symbol s => Parser s ()
  , sourcePos            -- :: Parser s SourcePos
  , lexeme               -- :: Parser s String
  , (<!>)                -- :: Parser s a -> String -> Parser s a

    -- * Derived combinators
  , choice               -- :: Symbol s => [Parser s a] -> Parser s a
  , opt                  -- :: Symbol s => Parser s a -> a -> Parser s a
  , chainl               -- :: Symbol s =>
                         --    Parser s (a -> a -> a) -> Parser s a ->
                         --    Parser s a
  , chainr               -- :: Symbol s =>
                         --    Parser s (a -> a -> a) -> Parser s a ->
                         --    Parser s a
  , manySepBy            -- :: Symbol s =>
                         --    Parser s b -> Parser s a -> Parser s [a]
  , someSepBy            -- :: Symbol s =>
                         --    Parser s b -> Parser s a -> Parser s [a]

    -- * Parsing
  , parse                -- :: Parser s a -> Symbols s ->
                         --    Feedback (a, Symbols s)
  , parse_               -- :: Lexer s -> Parser s a -> Source -> String ->
                         --    Feedback a
) where

import CCO.Feedback                 (Feedback, errorMessage)
import CCO.Lexing hiding (satisfy)
import CCO.SourcePos                (Source (..), Pos (..), SourcePos (..))
import CCO.Printing hiding (empty)
import qualified CCO.Printing as P  (empty)
import Control.Applicative
import Data.Function                (on)
import Data.List                    (nub)
import Prelude hiding               (lex)

-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

-- | The class of types that describe input symbols.
--
-- A minimal complete definition must supply the method @describe@.
class Symbol s where
  describe :: s -> String -> String
  -- ^ Retrieves a textual description from a value that describes a symbol and
  --   the lexeme that constitutes the symbol.

instance Symbol Char where
  describe _ = show

-------------------------------------------------------------------------------
-- Steps
-------------------------------------------------------------------------------

-- | Describes the construction of a value produced by a 'Parser'.
data Steps a
  = Done a 
    -- ^ The actual construction of a value.
  | Fail SourcePos (Maybe String) ([String] -> [String])
    -- ^ Failure to construct a value.
    --   Holds the position at which parsing failed, optionally a description
    --   of the unexpected symbol that was encountered at that postition,
    --   and an accumulator for descriptions of the terminals that were
    --   actually expected.
  | LexFail (Maybe String) SourcePos String
    -- ^ Failure to construct a value because of a lexical error.
    --   Holds an optional error message, the position of the erroneous lexeme
    --   and the lexeme itself.
  | Step (Steps a)
    -- ^ A single step in the construction of a value.

-- | Selects the best of two value constructions.
-- If both alternatives actually construct a value, the shortest construction
-- is selected; if both constructions require the same amount of steps, the
-- first takes precedence over the second.
best :: Steps a -> Steps a -> Steps a
best steps@(LexFail _ _ _) _    = steps
best _ steps@(LexFail _ _ _)    = steps
best (Fail srcpos unexp acc) (Fail _ _ acc')
                                = Fail srcpos unexp (acc . acc')
best (Fail _ _ _) steps         = steps
best steps (Fail _ _ _)         = steps
best steps@(Done _) _           = steps
best _ steps@(Done _)           = steps
best (Step steps) (Step steps') = Step (best steps steps')

-- | Actually tries to constucts a value.
eval :: Steps a -> Feedback a
eval (Done x)                 = return x
eval (Fail srcpos unexp acc)  = errorMessage . pp $
                                UnexpectedSymbol srcpos unexp (nub (acc []))
eval (LexFail mmsg srcpos lx) = errorMessage (pp (LexicalError mmsg srcpos lx))
eval (Step steps)             = eval steps

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

infixl 2 <!>

-- | The type of parsers that consume symbols described by tokens of type @s@
-- and produces values of type @a@.
newtype Parser s a
  = P {unP :: forall h r.
                ((h, a) -> Maybe ([String] -> [String]) ->
                  Symbols s -> Steps r) ->           -- continuation
                h ->                                 -- previous values
                Maybe ([String] -> [String]) ->      -- productions
                Symbols s ->                         -- input
                Steps r
      }
    -- ^ Takes a continuation, a stack of previously produced values, an
    --   optional accumulator for textual descriptions of associated grammar 
    --   productions, and an input stream.
    --
    -- Note: the stack and result types are existentially quantified, which
    -- makes that P has a rank-2 type.    

instance Functor (Parser s) where
  fmap f (P p) = P (\k -> p (\(h, x) -> k (h, f x)))

instance Applicative (Parser s) where
  pure x      = P (\k h -> k (h, x))
  P p <*> P q = P (\k -> p (q (\((h, f), x) -> k (h, f x))))

instance Symbol s => Alternative (Parser s) where
  empty = P $ \_ _ macc (Symbols src units) -> case units of
            []                   -> Fail (SourcePos src EOF) Nothing
                                      (maybe id id macc)
            Token s pos lx _ : _ -> Fail (SourcePos src pos)
                                      (Just (describe s lx)) (maybe id id macc)
            Error pos lx _ : _   -> LexFail Nothing (SourcePos src pos) lx
            Msg msg pos lx _ : _ -> LexFail (Just msg) (SourcePos src pos) lx 

  P p <|> P q = P (\k h macc syms -> p k h macc syms `best` q k h macc syms)

-- | Produces a 'Parser' that recognises a single symbol satisfying a given
-- predicate.
satisfy :: Symbol s => (s -> Bool) -> Parser s s
satisfy test = P p
  where
    p _ _ macc (Symbols src [])
                  = Fail (SourcePos src EOF) Nothing (maybe id id macc)
    p k h macc (Symbols src (Token s pos lx _ : units))
      | test s    = Step (k (h, s) Nothing (Symbols src units))
      | otherwise = Fail (SourcePos src pos) (Just (describe s lx))
                      (maybe id id macc)
    p _ _ _ (Symbols src (Error pos lx _ : _))
                  = LexFail Nothing (SourcePos src pos) lx
    p _ _ _ (Symbols src (Msg msg pos lx _ : _))
                  = LexFail (Just msg) (SourcePos src pos) lx

-- | A 'Parser' that recognises the end of input.
eof :: Symbol s => Parser s ()
eof =  P p
  where
    p k h _ syms@(Symbols _ [])
      = Step (k (h, ()) Nothing syms)
    p _ _ _ (Symbols src (Token s pos lx _ : _))
      = Fail (SourcePos src pos) (Just (describe s lx)) ("end of input" :)
    p _ _ _ (Symbols src (Error pos lx _ : _))
      = LexFail Nothing (SourcePos src pos) lx
    p _ _ _ (Symbols src (Msg msg pos lx _ : _))
      = LexFail (Just msg) (SourcePos src pos) lx

-- | A 'Parser' that produces the 'SourcePos' of the next lexeme in the input
-- (without consuming the associated symbol).
sourcePos :: Parser s SourcePos
sourcePos = P p
  where
    p k h macc syms@(Symbols src [])
      = k (h, SourcePos src EOF) macc syms
    p k h macc syms@(Symbols src (Token _ pos _ _ : _))
      = k (h, SourcePos src pos) macc syms
    p k h macc syms@(Symbols src (Error pos _ _ : _))
      = k (h, SourcePos src pos) macc syms
    p k h macc syms@(Symbols src (Msg _ pos _ _ : _))
      = k (h, SourcePos src pos) macc syms

-- | A 'Parser' that produces the next lexeme in the input (without consuming
-- the associated symbol).
-- Fails if the end of input has been reached.
lexeme :: Parser s String
lexeme = P p
  where
    p k h _ (Symbols src [])
      = Fail (SourcePos src EOF) Nothing ("any symbol" :)
    p k h macc syms@(Symbols _ (Token _ _ lx _ : _)) = k (h, lx) macc syms
    p k h macc syms@(Symbols _ (Error _ lx _ : _ ))  = k (h, lx) macc syms
    p k h macc syms@(Symbols _ (Msg _ _ lx _ : _))   = k (h, lx) macc syms

-- | Labels a 'Parser' with a description of the grammar production it is
-- associated with.
-- Used to produce more informative error messages.
(<!>) :: Parser s a -> String -> Parser s a
P p <!> prod = P q where
  q k h Nothing = p (\h macc' syms -> k h macc' syms) h (Just (prod :))
  q k h macc    = p (\h macc' syms -> k h macc' syms) h macc

-------------------------------------------------------------------------------
-- Derived combinators
-------------------------------------------------------------------------------

infixl 3 `opt`

-- | Produces a 'Parser' that may use any 'Parser' from a given list to
-- parse its input.
choice :: Symbol s => [Parser s a] -> Parser s a
choice []       = empty
choice [parser] = parser
choice parsers  = foldr1 (<|>) parsers

-- | Produces a 'Parser' that tries to parse its input with a given argument
---'Parser',producing a specified default value if the argument 'Parser' fails.
opt :: Symbol s => Parser s a -> a -> Parser s a
opt parser x = parser <|> pure x

-- | Produces a 'Parser' that parses one or more elements chained by a
-- left-associative operator.
chainl :: Symbol s => Parser s (a -> a -> a) -> Parser s a -> Parser s a
chainl op elem = foldl (flip ($)) <$> elem <*> many (flip <$> op <*> elem)

-- | Produces a 'Parser' that parses one or more elements chained by a
-- right-associative operator.
chainr :: Symbol s => Parser s (a -> a -> a) -> Parser s a -> Parser s a
chainr op elem = parser
  where
    parser = elem <**> (flip <$> op <*> parser `opt` id)

-- | Produces a 'Parser' that parses zero or more elements separated by 
-- specified separator.
manySepBy :: Symbol s => Parser s b -> Parser s a -> Parser s [a]
manySepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []

-- | Produces a 'Parser' that parses one or more elements separated by
-- specified separator.
someSepBy :: Symbol s => Parser s b -> Parser s a -> Parser s [a]
someSepBy sep elem = (:) <$> elem <*> many (sep *> elem)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Uses a specified 'Parser' to try and parse 'Symbols'.
-- Produces,if successful, a result and any remaining 'Symbols'.
parse :: Parser s a -> Symbols s -> Feedback (a, Symbols s)
parse (P p) = eval . p (\(_, x) _ syms -> Done (x, syms)) () Nothing

-- | Uses a specified 'Lexer' and 'Parser' to perform syntactic analysis on
-- an input stream.
parse_ :: Lexer s -> Parser s a -> Source -> String -> Feedback a
parse_ lexer parser src = fmap fst . parse parser . lex lexer src

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

-- | Diagnosis of why parsing failed.
data Diagnosis
  = LexicalError (Maybe String) SourcePos String
    -- ^ Indicates that parsing failed because of a lexical error.
    --   Holds an optional error message, the position at which the error 
    --   occurred and the erroneous lexeme.
  | UnexpectedSymbol SourcePos (Maybe String) [String]
    -- ^ Indicates that parsing failed because an expected symbol (or the end
    --   of input) was encountered.
    --   Holds the position at which parsing failed, optionally a description
    --   of the unexpected symbol that was encounterd at that position and a
    --   list of descriptions of the phrases that were actually expected.

instance Printable Diagnosis where pp = ppDiagnosis

-------------------------------------------------------------------------------
-- Pretty printing diagnoses
-------------------------------------------------------------------------------

-- | Pretty prints a 'Diagnosis'.
ppDiagnosis :: Diagnosis -> Doc

ppDiagnosis (LexicalError Nothing srcpos lx)
  = above [ppHeader, text " ", ppUnexpected]
  where
    ppHeader     = wrapped $
                   describeSourcePos srcpos ++ ": Lexical error."
    ppUnexpected = text "? unexpected : " >|< text lx

ppDiagnosis (LexicalError (Just msg) srcpos _)
  = wrapped (describeSourcePos srcpos ++ ": Lexical error: " ++ msg ++ ".")

ppDiagnosis (UnexpectedSymbol srcpos unexp exp)
  = above [ppHeader, text " ", ppUnexpected, ppExpected]
  where
    ppHeader               = wrapped $
                             describeSourcePos srcpos ++ ": Syntax error."
    ppUnexpected           = text "? unexpected : " >|<
                             wrapped (describeUnexpected unexp)
    ppExpected | null exp  = P.empty
               | otherwise = text "? expected   : " >|<
                             wrapped (disjunction exp)

-- | Retrieves a textual description of a 'SourcePos'.
describeSourcePos :: SourcePos -> String
describeSourcePos (SourcePos (File file) (Pos ln col))
                                                 = file ++
                                                   ":line " ++ show ln ++
                                                   ":column " ++ show col
describeSourcePos (SourcePos (File file) EOF)    = file ++
                                                   ":<at end of file>"
describeSourcePos (SourcePos Stdin (Pos ln col)) = "line " ++ show ln ++
                                                   ":column " ++ show col
describeSourcePos (SourcePos Stdin EOF)          = "<at end of input>"

-- | Retrieves a textual description of an unexpected symbol.
describeUnexpected :: Maybe String -> String
describeUnexpected Nothing    = "end of input"
describeUnexpected (Just sym) = sym

-- | Takes a non-empty list of textual items and produces a text that
-- expresses their disjunction: \"<item>, <item>, ..., or <item>\".
disjunction :: [String] -> String
disjunction [x]       = x
disjunction [x, y]    = x ++ " or " ++ y
disjunction [x, y, z] = x ++ ", " ++ y ++ ", or " ++ z
disjunction (x : xs)  = x ++ ", " ++ disjunction xs