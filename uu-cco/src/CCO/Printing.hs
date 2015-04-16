-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Printing
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A combinator library for pretty printing.
--
-- Inspired by
--
-- * S. Doaitse Swierstra, Pablo R. Azero Alcocer, and Joao Saraiva.
--   Designing and implementing combinator languages.
--   In S. Doaitse Swierstra and Pedro Rangel Henriques, and Jose Nuno
--   Oliveira, editors, /Advanced Functional Programming, Third International/
--   /School, Braga, Portugal, September 12-19, 1998, Revised Lectures/, volume
--   1608 of /Lecture Notes in Computer Science/, pages 150-206.
--   Springer-Verlag, 1999.
--
-------------------------------------------------------------------------------

module CCO.Printing (
    -- * Abstract document type
    Doc                   -- abstract
  , isEmpty               -- :: Doc -> Bool

    -- * Primitive document constructors
  , empty                 -- :: Doc
  , text                  -- :: String -> Doc
  , wrapped               -- :: String -> Doc

    -- * Elementary document combinators
  , indent                -- :: Int -> Doc -> Doc
  , (>-<)                 -- :: Doc -> Doc -> Doc
  , above                 -- :: [Doc] -> Doc
  , (>|<)                 -- :: Doc -> Doc -> Doc
  , besides               -- :: [Doc] -> Doc

    -- * Parallelisation
  , (>//<)                -- :: Doc -> Doc -> Doc
  , split                 -- :: [Doc] -> Doc
  , join                  -- :: Doc -> Doc
  , (>^<)                 -- :: Doc -> Doc -> Doc
  , choose                -- :: [Doc] -> Doc

    -- * Punctuation
  , space                 -- :: Doc
  , period                -- :: Doc
  , comma                 -- :: Doc
  , semicolon             -- :: Doc
  , colon                 -- :: Doc
  , sepBy                 -- :: [Doc] -> Doc
  , (>#<)                 -- :: Doc -> Doc -> Doc
  , lparen, rparen        -- :: Doc
  , lbracket, rbracket    -- :: Doc
  , lbrace, rbrace        -- :: Doc
  , langle, rangle        -- :: Doc
  , enclose               -- :: Doc -> Doc -> Doc -> Doc
  , parens                -- :: Doc -> Doc
  , brackets              -- :: Doc -> Doc
  , braces                -- :: Doc -> Doc
  , angles                -- :: Doc -> Doc

    -- * Colours
  , black                 -- :: Doc -> Doc
  , red                   -- :: Doc -> Doc
  , green                 -- :: Doc -> Doc
  , blue                  -- :: Doc -> Doc
  , yellow                -- :: Doc -> Doc
  , magenta               -- :: Doc -> Doc
  , cyan                  -- :: Doc -> Doc
  , white                 -- :: Doc -> Doc

    -- * Rendering
  , render                -- :: Int -> Doc -> Maybe String
  , render_               -- :: Int -> Doc -> String
  , renderHeight          -- :: Int -> Doc -> Maybe (String, Int)
  , renderHeight_         -- :: Int -> Doc -> (String, Int)
  , renderIO              -- :: Int -> Doc -> Maybe (IO ())
  , renderIO_             -- :: Int -> Doc -> IO ()
  , renderIOHeight        -- :: Int -> Doc -> Maybe (IO (), Int)
  , renderIOHeight_       -- :: Int -> Doc -> (IO (), Int)

    -- * The class Printable
  , Printable (..)    

    -- * Printing showables
  , showable              -- :: Show a => a -> Doc
  ) where

import CCO.Printing.Colour (Colour (..))
import CCO.Printing.Doc (Doc (..), isEmpty)
import CCO.Printing.Printer (Printer (height), printToString, printToIO)
import qualified CCO.Printing.Rendering as R (render)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)

-------------------------------------------------------------------------------
-- Primitive document constructors
-------------------------------------------------------------------------------

-- | The empty document.
-- Left and right unit of '>-<' and '>|<'.
empty :: Doc
empty =  Empty

-- | @text txt@ produces a document containing the text @txt@.
text :: String -> Doc
text = foldr Above Empty . map Text . lines

-- | @wrapped txt@ produces a document containing the text @txt@, possibly
-- wrapping its contents to fit the available space
wrapped :: String -> Doc
wrapped = foldr Above Empty . map Wrapped . lines

-------------------------------------------------------------------------------
-- Document combinators
-------------------------------------------------------------------------------

infixr 3 >|<
infixr 2 >-<

-- | Indents a document by a given amount of space.
indent :: Int -> Doc -> Doc
indent n = Indent n

-- | \"Above\": puts one document on top of another.
(>-<) :: Doc -> Doc -> Doc
(>-<) = Above

-- | Stacks multiple documents: @above = foldr (>-<) empty@.
above :: [Doc] -> Doc
above = foldr (>-<) empty

-- | \"Besides\": puts two documents next to eachother by \"dovetailing\" them.
(>|<) :: Doc -> Doc -> Doc
(>|<) = Besides

-- | Queues multiple documents: @besides = foldr (>|<) empty@.
besides :: [Doc] -> Doc
besides = foldr (>|<) empty

-------------------------------------------------------------------------------
-- Parallelisation
-------------------------------------------------------------------------------

infixr 1 >//<, >^<

-- | \"Split\": introduces two alternative (\"parallel\") formattings.
(>//<) :: Doc -> Doc -> Doc
(>//<) = Split

-- | Introduces multiple alternative formattings:
-- @split = foldr (>\/\/<) empty@.
split :: [Doc] -> Doc
split = foldr (>//<) empty

-- | Selects the most space-efficient of all alternative formattings for a
-- document.
join :: Doc -> Doc
join = Join

-- | Immediate choice: @l >^\< r = join (l >\/\/< r)@.
(>^<) :: Doc -> Doc -> Doc
l >^< r = join (l >//< r)

-- | Immediate choice: @choose = foldr (>^<) empty@.
choose :: [Doc] -> Doc
choose = foldr (>^<) empty

-------------------------------------------------------------------------------
-- Punctuation
-------------------------------------------------------------------------------

infixr 3 >#<

-- | A space character: @space = text \" \"@.
space :: Doc
space =  text " "

-- | A full stop: @period = text \".\"@.
period :: Doc
period = text "."

-- | A comma: @comma = text \",\"@.
comma :: Doc
comma = text ","

-- | A semicolon: @semicolon = text \";\"@.
semicolon :: Doc
semicolon = text ";"

-- | A colon: @colon = text \":\"@.
colon :: Doc
colon = text ":"

-- | Inserts a delimiter between all adjacent nonempty documents in a list.
sepBy :: [Doc] -> Doc -> Doc
sepBy []                     _   = empty
sepBy [doc]                  _   = doc
sepBy (l : docs@(r : docs')) sep
  | isEmpty l                    = sepBy docs sep
  | isEmpty r                    = sepBy (l : docs') sep
  | otherwise                    = sepBy ((l >|< sep >|< r) : docs') sep

-- | Inserts a space between two documents.
-- If one of the documents is empty, the other one is returned:
-- @l >#< r = [l, r] \`sepBy\` space@. 
(>#<) :: Doc -> Doc -> Doc
l >#< r = [l, r] `sepBy` space

-- | Parentheses:
--
-- > lparen = text "("
-- > rparen = text ")"

lparen, rparen :: Doc
lparen = text "("
rparen = text ")"

-- | Square brackets:
--
-- > lbracket = text "["
-- > rbracket = text "]"

lbracket, rbracket :: Doc
lbracket = text "["
rbracket = text "]"

-- | Curly braces:
--
-- > lbrace = text "{"
-- > rbrace = text "}"

lbrace, rbrace :: Doc
lbrace = text "{"
rbrace = text "}"

-- | Angle brackets:
--
-- > langle = text "<"
-- > rangle = text ">"

langle, rangle :: Doc
langle = text "<"
rangle = text ">"

-- | Encloses a document in brackets:
-- @enclose l r d = l >|\< d >|\< r@.
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l >|< d >|< r

-- | Encloses a document in parentheses: @parens = enclose lparen rparen@.
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Encloses a document in square brackets:
-- @brackets = enclose lbracket rbracket@.
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | Encloses a document in curly braces: @braces = enclose lbrace rbrace@.
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Encloses a document in angle brackets: @angles = enclose langle rangle@.
angles :: Doc -> Doc
angles = enclose langle rangle

-------------------------------------------------------------------------------
-- Colours
-------------------------------------------------------------------------------

-- | Sets the foreground colour of a document to black.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

black :: Doc -> Doc
black = Colour Black

-- | Sets the foreground colour of a document to red.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

red :: Doc -> Doc
red = Colour Red

-- | Sets the foreground colour of a document to green.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

green :: Doc -> Doc
green = Colour Green

-- | Sets the foreground colour of a document to blue.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

blue :: Doc -> Doc
blue = Colour Blue

-- | Sets the foreground colour of a document to yellow.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

yellow :: Doc -> Doc
yellow = Colour Yellow

-- | Sets the foreground colour of a document to magenta.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

magenta :: Doc -> Doc
magenta = Colour Magenta

-- | Sets the foreground colour of a document to cyan.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

cyan :: Doc -> Doc
cyan = Colour Cyan

-- | Sets the foreground colour of a document to white.
--
-- (Note: colours are only taken into account when a document is rendered by
-- means of 'renderIO' or 'renderIO_'.
-- They are ignored if 'render' or 'render_' are used.)

white :: Doc -> Doc
white = Colour White

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

-- | Tries to render a document with a specified amount of horizontal space and
-- to invoke the supplied function on the resulting printer.
renderWith :: Printer a => (a -> b) -> Int -> Doc -> Maybe b
renderWith f wmax doc = fmap f (R.render wmax doc)

-- | Tries to render a document with a specified amount of horizontal space and
-- to invoke the supplied function on the resulting printer.
-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
renderWith_ :: Printer a => (a -> b) -> Int -> Doc -> b
renderWith_ f wmax doc = head $ catMaybes $ fmap (\w -> renderWith f w doc) ws
  where
    ws = iterate (\w -> w + 1 `max` (w `div` 10)) wmax

-- | Tries to render a document with a specified amount of horizontal space and
-- to invoke the supplied function on the resulting printer.
-- The result of the function is tupled with the number of new lines claimed by
-- the rendering.
renderHeightWith :: Printer a => (a -> b) -> Int -> Doc -> Maybe (b, Int)
renderHeightWith f wmax doc = fmap (f &&& height) (R.render wmax doc)

-- | Tries to render a document with a specified amount of horizontal space and
-- to invoke the supplied function on the resulting printer.
-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
-- The resulting rendering is tupled with the number of new lines claimed by
-- the rendering.
renderHeightWith_ :: Printer a => (a -> b) -> Int -> Doc -> (b, Int)
renderHeightWith_ f wmax doc
  = head $ catMaybes $ fmap (\w -> renderHeightWith f w doc) ws
  where
    ws = iterate (\w -> w + 1 `max` (w `div` 10)) wmax

-- | Tries to render a document within a specified amount of horizontal space.
render :: Int -> Doc -> Maybe String
render = renderWith printToString

-- | Tries to render a document within a specified amount of horizontal space.
-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
render_ :: Int -> Doc -> String
render_ = renderWith_ printToString

-- | Tries to render a document within a specified amount of horizontal space.
-- A resulting rendering is tupled with the number of new lines claimed by the
-- rendering.
renderHeight :: Int -> Doc -> Maybe (String, Int)
renderHeight = renderHeightWith printToString

-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
-- The resulting rendering is tupled with the number of new lines claimed by
-- the rendering.
renderHeight_ :: Int -> Doc -> (String, Int)
renderHeight_ = renderHeightWith_ printToString

-- | Tries to render a document within a specified amount of horizontal space
-- and to print it to the standard output channel.
renderIO :: Int -> Doc -> Maybe (IO ())
renderIO = renderWith printToIO

-- | Tries to render a document within a specified amount of horizontal space
-- and to print it to the standard output channel.
-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
renderIO_ :: Int -> Doc -> IO ()
renderIO_ = renderWith_ printToIO

-- | Tries to render a document within a specified amount of horizontal space
-- and to print it to the standard output channel.
-- A resulting rendering is tupled with the number of new lines claimed by the
-- rendering.
renderIOHeight :: Int -> Doc -> Maybe (IO (), Int)
renderIOHeight = renderHeightWith printToIO

-- | Tries to render a document within a specified amount of horizontal space
-- and to print it to the standard output channel.
-- If the document cannot be rendered within the given amount of space, the
-- amount of space is, until the document fits, repeatedly enlarged by 10
-- percent.
-- The resulting rendering is tupled with the number of new lines claimed by
-- the rendering.
renderIOHeight_ :: Int -> Doc -> (IO (), Int)
renderIOHeight_ = renderHeightWith_ printToIO

-------------------------------------------------------------------------------
-- The class Printable
-------------------------------------------------------------------------------

-- | The class @Printable@.
-- Instances of @Printable@ provide a pretty printer for their values.
--
-- A minimal complete definition must supply the method @pp@.

class Printable a where
  -- | Retrieves a pretty-printable document for a value.
  pp :: a -> Doc

instance Printable Char    where pp = showable
instance Printable Int     where pp = showable
instance Printable Integer where pp = showable
instance Printable Float   where pp = showable
instance Printable Double  where pp = showable

-------------------------------------------------------------------------------
-- Printing showables
-------------------------------------------------------------------------------

-- | Prints a 'Show'able value: @showable = text . show@.
showable :: Show a => a -> Doc
showable = text . show
