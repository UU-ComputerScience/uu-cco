-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Lexing
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A library of lexer combinators that expose their functionality through an
-- 'Applicative' interface.
--
-------------------------------------------------------------------------------

module CCO.Lexing (
    -- * Lexers
    Lexer                                -- abstract, instances: Functor,
                                         -- Applicative, Alternative
  , satisfy                              -- :: Lexer Char
  , message                              -- :: String -> Lexer a

    -- * Derived lexers
  , anyChar                              -- :: Lexer Char
  , anyCharFrom                          -- :: [Char] -> Lexer Char
  , anyCharBut                           -- :: [Char] -> Lexer Char
  , range                                -- :: (Char, Char) -> Lexer Char
  , notInRange                           -- :: (Char, Char) -> Lexer Char
  , char                                 -- :: Lexer Char
  , string                               -- :: Lexer String
  , space                                -- :: Lexer Char
  , lower                                -- :: Lexer Char
  , upper                                -- :: Lexer Char
  , letter                               -- :: Lexer Char
  , alpha                                -- :: Lexer Char
  , alphaNum                             -- :: Lexer Char
  , digit                                -- :: Lexer Char
  , digit_                               -- :: Lexer Int
  , binDigit                             -- :: Lexer Char
  , binDigit_                            -- :: Lexer Int
  , octDigit                             -- :: Lexer Char
  , octDigit_                            -- :: Lexer Int
  , hexDigit                             -- :: Lexer Char
  , hexDigit_                            -- :: Lexer Int

    -- * Ignoring recognised input
  , ignore                               -- :: Lexer a -> Lexer b

    -- * Symbols
  , LexicalUnit (Token, Error, Msg)
  , Symbols (Symbols)

    -- * Lexing
  , lex                                  -- :: Lexer a -> Source -> String ->
                                         --    Symbols a

    -- * Utilities
  , tokens                               -- :: Symbols a -> [(SourcePos, a)]
  , tokens_                              -- :: Symbols a -> [a]
) where

import Prelude hiding (lex)
import CCO.SourcePos (Source, Pos (Pos, EOF), SourcePos (SourcePos))
import Control.Applicative (Applicative (..), Alternative (..), (<$>))
import Data.Char

-------------------------------------------------------------------------------
-- Steps
-------------------------------------------------------------------------------

-- | A @Steps@ represents the tokenisation associated with the recognition of
-- a (part of a) lexeme.
data Steps a = None         -- ^ No tokenisation yet.
             | Return a     -- ^ Produces as a token a value of type @a@.
             | Ignore       -- ^ Produces a value of type @a@ but ignores it as
                            --   far as tokenisation is concerned.
                            --   (Typically used on lexemes that constitute
                            --   comments or whitespace.)
             | Fail String  -- ^ Fail with a specified error message.

instance Functor Steps where
  fmap _ None       = None
  fmap f (Return x) = Return (f x)
  fmap _ Ignore     = Ignore
  fmap _ (Fail msg) = Fail msg

instance Applicative Steps where
  pure x = Return x

  (Fail msg) <*> _          = Fail msg
  _          <*> (Fail msg) = Fail msg
  None       <*> _          = None
  _          <*> None       = None
  Return f   <*> Return x   = Return (f x)
  _          <*> _          = Ignore

instance Alternative Steps where
  empty = None

  steps@(Return _) <|> _                = steps
  steps@Ignore     <|> _                = steps
  _                <|> steps@(Return _) = steps
  _                <|> steps@Ignore     = steps
  None             <|> steps            = steps
  steps            <|> _                = steps

-------------------------------------------------------------------------------
-- Lexers
-------------------------------------------------------------------------------

-- | Type of lexers that produce tokens of type @a@.
data Lexer a = Trie (Steps a) [(Char -> Bool, Lexer (Char -> a))]
               -- ^ A @Lexer@ consists of a @Steps@-component that represents
               --   the tokenisation for the lexeme currently recognised and
               --   a multi-mapping from characters to lexers that represent
               --   the next state of the scanner.

instance Functor Lexer where
  fmap f (Trie steps edges) =
    Trie (fmap f steps) [(p, fmap (f .) next) | (p, next) <- edges]

instance Applicative Lexer where
  pure x = Trie (pure x) []

  Trie None edges <*> lexer = Trie None
    [(p, flip <$> next <*> lexer) | (p, next) <- edges]
  Trie (Fail msg) edges <*> lexer = Trie (Fail msg)
    [(p, flip <$> next <*> lexer) | (p, next) <- edges]
  Trie steps edges <*> lexer@(Trie steps' edges') = Trie (steps <*> steps') $
    [(p, smap ((.) <$> steps) next) | (p, next) <- edges'] ++
    [(p, flip <$> next <*> lexer) | (p, next) <- edges]

instance Alternative Lexer where
  empty = Trie empty []
  Trie steps edges <|> Trie steps' edges' =
    Trie (steps <|> steps') (edges ++ edges')

-- | Maps a tokenisation over a 'Lexer'.  
smap :: Steps (a -> b) -> Lexer a -> Lexer b
smap steps (Trie steps' edges) = Trie (steps <*> steps')
  [(p, smap ((.) <$> steps) next) | (p, next) <- edges] 

-- | A 'Lexer' that recognises any character that satisfies a specified
-- predicate.
satisfy :: (Char -> Bool) -> Lexer Char
satisfy p = Trie None [(p, Trie (Return id) [])]

-- | A 'Lexer' that will produce a specified error message.
message :: String -> Lexer a
message msg = Trie (Fail msg) []

-------------------------------------------------------------------------------
-- Derived lexers
-------------------------------------------------------------------------------

-- | The trivial 'Lexer' that recognises any character.
anyChar :: Lexer Char
anyChar = satisfy (const True)

-- | A 'Lexer' that recognises any character that appears in a given list.
anyCharFrom :: [Char] -> Lexer Char
anyCharFrom cs = satisfy (`elem` cs)

-- | A 'Lexer' that recognises any character that does not appear in a given
-- list.
anyCharBut :: [Char] -> Lexer Char
anyCharBut tabus = satisfy (`notElem` tabus)

-- | A 'Lexer' that recognises any character that appears in a given range.
-- More efficent than @\\(low, up) -> anyCharFrom [low .. up]@.
range :: (Char, Char) -> Lexer Char
range (low, up) = satisfy (\c -> c >= low && c <= up)

-- | A 'Lexer' that recognises any character that does not appear in a given
-- range.
notInRange :: (Char, Char) -> Lexer Char
notInRange (low, up) = satisfy (\c -> c < low || c > up)

-- | A 'Lexer' that recognises a specified character.
char :: Char -> Lexer Char
char c = satisfy (== c)

-- | A 'Lexer' that recognises a specified 'String'.
string :: String -> Lexer String
string []       = pure []
string (c : cs) = (:) <$> char c <*> string cs

-- | A 'Lexer' that recognises a whitespace character
space :: Lexer Char
space = satisfy isSpace

-- | A 'Lexer' that recognises lower-case alphabetic characters.
lower :: Lexer Char
lower = satisfy isLower

-- | A 'Lexer' that recognises upper-case or title-case alphabetic characters.
upper :: Lexer Char
upper = satisfy isUpper

-- | A 'Lexer' that recognises alphabetic characters. Equivalent to 'alpha'.
letter :: Lexer Char
letter = alpha

-- | A 'Lexer' that recognises alphabetic characters. Equivalet to 'letter'.
alpha :: Lexer Char
alpha = satisfy isAlpha

-- | A 'Lexer' that recognises alphabetic or numeric characters.
alphaNum :: Lexer Char
alphaNum = satisfy isAlphaNum

-- | A 'Lexer' that recognises a digit.
digit :: Lexer Char
digit = satisfy isDigit

-- | A 'Lexer' that recognises a digit and tokenises it as an 'Int'.
digit_ :: Lexer Int
digit_ = (\c -> ord c - ordZero) <$> digit

-- | A 'Lexer' that recognises a binary digit.
binDigit :: Lexer Char
binDigit = range ('0', '1')

-- | A 'Lexer' that recognises a binary digit and tokenises it as an 'Int'.
binDigit_ :: Lexer Int
binDigit_ = (\c -> ord c - ordZero) <$> binDigit

-- | A 'Lexer' that recognises an octal digit.
octDigit :: Lexer Char
octDigit = satisfy isOctDigit

-- | A 'Lexer' that recognises an octal digit and tokenises it as an 'Int'.
octDigit_ :: Lexer Int
octDigit_ = (\c -> ord c - ordZero) <$> octDigit

-- | A 'Lexer that recognises a hexedecimal digit.
hexDigit :: Lexer Char
hexDigit = satisfy isHexDigit

-- | A 'Lexer' that recognises a hexadecimal digit and tokenises it as an
-- 'Int'.
hexDigit_ :: Lexer Int
hexDigit_ = (\c -> ord c - ordZero) <$> digit <|>
            (\c -> 10 + (ord c - ordLowerA)) <$> range ('a', 'f') <|>
            (\c -> 10 + (ord c - ordUpperA)) <$> range ('A', 'F')

-------------------------------------------------------------------------------
-- Ignoring recognised input
-------------------------------------------------------------------------------

-- | Produces a 'Lexer' that recognises the same inputs as a given underlying
-- 'Lexer', but that does not result in any tokenisation.
--
-- The input recognised by a 'Lexer' constructed with @ignore@ is simply 
-- ignored when the 'Lexer' is used to turn a stream of characters into a
-- stream of 'LexicalUnit's.
-- 
-- Mainly used to suppress the generation of tokens for lexemes that constitute-- lexical units like comments and whitespace.

ignore :: Lexer a -> Lexer b
ignore = smap Ignore

-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

-- | The type of lexical units.
data LexicalUnit a
   = Token a Pos String String     -- ^ A tokenised lexeme: its token, its
                                   --   position, the characters it consists
                                   --   of, and its its trailing characters in
                                   --   the input stream.
   | Error Pos String String       -- ^ An invalid lexeme: its position, the
                                   --   characters it consists of, and its
                                   --   trailing characters in the input
                                   --   stream.
   | Msg String Pos String String  -- ^ An invalid lexeme, labeled by a custom
                                   --   error message: the message, its
                                   --   position, the characters it consists
                                   --   of, and its trailing characters in the
                                   --   input stream.

-- | The type of streams of symbols described by tokens of type 'a'.
data Symbols a = Symbols Source [LexicalUnit a]

-------------------------------------------------------------------------------
-- Lexing
-------------------------------------------------------------------------------

-- | A @Scan@: an accumulator for the input recognised, a tokenisation for the
-- input recognised, the current position, and the remaining input.
type Scan a = (String -> String, Steps a, Pos, String)

-- | Scanner: takes a 'Lexer', an input position, and an input stream and then
-- produces a 'Scan' for the next lexeme.
scan :: Lexer a -> Pos -> String -> Scan a
scan (Trie steps edges) pos input = (id, steps, pos, input) `sor` consume input
  where
    consume []       = (id, None, pos, input)
    consume (c : cs) =
      let lexer                       = choice [next | (p, next) <- edges, p c]
          (acc, steps', pos', input') =
            scan (lexer <*> pure c) (incrPos c pos) cs
      in  ((c :) . acc, steps', pos', input')

-- | Picks the \"best\" of two 'Scan's, favouring the second over the first,
-- except when the second does not allow any tokenisation.
sor :: Scan a -> Scan a -> Scan a
sor scn (_, None, _, _) = scn
sor _ scn               = scn

-- | Runs a lexer on a specified input stream.
lex :: Lexer a -> Source -> String ->  Symbols a
lex lexer src = let initpos = initialPos
                in  Symbols src . tokenise initpos id initpos
  where
    tokenise errpos prefix pos input =
      let sentinel = case prefix "" of
                       []     -> id 
                       lexeme -> (Error errpos lexeme input :)
      in  case scan lexer pos input of
            (_, None, _, [])              -> sentinel []
            (acc, None, pos', c : input') -> tokenise errpos (prefix . (c :))
                                               (incrPos c pos') input'
            (acc, Return x, pos', input') -> sentinel $
                                               Token x pos (acc "") input' :
                                               tokenise pos' id pos' input'
            (acc, Fail msg, pos', input') -> Msg msg errpos (acc "") input' :
                                               tokenise pos' id pos' input'
            (_, _, pos', input')          -> sentinel
                                               (tokenise pos' id pos' input')

-------------------------------------------------------------------------------
-- Source positions
-------------------------------------------------------------------------------

-- | Retrieves the first position in an input stream.
initialPos :: Pos
initialPos = Pos 1 1

-- | Increments a 'Pos' based on the character consumed.
-- If a newline character is consumed, the line number is incremented and the
-- column number is reset to 1; for any other character, the line number is
-- kept unchanged and the column number is incremented by 1.
-- Fails if an attempt is made to increment an 'EOF'-value.
incrPos :: Char -> Pos -> Pos
incrPos '\n' (Pos line column) = Pos (line + 1) 1
incrPos _    (Pos line column) = Pos line (column + 1)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | The 'ord' of '\'0\''.
ordZero :: Int
ordZero = ord '0'

-- | The 'ord' of '\'a\''.
ordLowerA :: Int
ordLowerA = ord 'a'

-- | The 'ord' of '\'A\''.
ordUpperA :: Int
ordUpperA = ord 'A'

-- | Combines multiple 'Alternative's by means of '<|>'.
choice :: Alternative f => [f a] -> f a
choice []  = empty
choice [x] = x
choice xs  = foldr1 (<|>) xs

-- | Retrieves all tokens together with their source positions from a list of
-- 'Symbols'.
tokens :: Symbols a -> [(SourcePos, a)]
tokens (Symbols src units) =
  [(SourcePos src pos, x) | Token x pos _ _ <- units]

-- | Retrieves all tokens from a list of 'Symbols'.
tokens_ :: Symbols a -> [a]
tokens_ (Symbols _ units) = [x | Token x _ _ _ <- units]