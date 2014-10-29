-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Printing.Rendering
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Facilities for rendering pretty-printable documents.
--
-------------------------------------------------------------------------------

module CCO.Printing.Rendering (
    render    -- :: Printer a => Int -> Doc -> Maybe a
  ) where

import CCO.Printing.Colour  (Colour)
import CCO.Printing.Doc     (Doc (..))
import CCO.Printing.Printer (Printer (..))
import Data.Maybe           (listToMaybe)
import Data.Monoid          (Monoid (..))
import Data.List            (intersperse)

-- This module is at the heart of the pretty-printing library.

-- Rendering involves a hierarchy of data structures, that share a common
-- interface ('PP').
-- At the top of the hierarchy, we have 'Formatters' that, given a amount of
-- available horizontal space, spawn a list of all possible 'Format's, from
-- which concrete printers can be obtained.

-- Across the hierarchy, empty documents are expected to not appear as operands
-- to \"above\" or \"besides\" operations.
-- (The hence necessary normalisation is performed within the function
-- 'fromDoc'.)

-------------------------------------------------------------------------------
-- The PP class
-------------------------------------------------------------------------------

-- | The common interface of the data structures in the rendering hierarchy.
--
-- A minimal complete definition must supply all six methods, i.e., @ppEmpty@,
-- @ppText@, @ppIndent@, @ppAbove@, @ppBesides@, and @ppColour@.

class PP a where
  -- | Involved in rendering the empty document.
  ppEmpty   :: a
  -- | Involved in rendering text.
  ppText    :: String -> a
  -- ^ Involved in rendering an indented document.
  ppIndent  :: Int -> a -> a
  -- ^ Involved in rendering two stacked up documents.
  ppAbove   :: a -> a -> a
  -- ^ Involved in rendering two \"dovetailed\" documents.
  ppBesides :: a -> a -> a
  -- ^ Involved in rendering coloured output.
  ppColour  :: Colour -> a -> a

-------------------------------------------------------------------------------
-- Concrete renderings
-------------------------------------------------------------------------------

-- | The most low-level data structure in the rendering hierarchy.
--
-- A @Rendering@ holds a list of printers: one for each printed line within the
-- rendered document.

newtype Rendering a = R [a]

instance Printer a => PP (Rendering a) where
  ppEmpty                           = R []
  ppText txt                        = R [ptext txt]
  ppIndent n (R ps)                 = R (map (ws n `mappend`) ps)
  ppAbove (R psl) (R psr)           = R (psl ++ psr)

  -- \"dovetailing\" involves concatenating
  -- * all but the last of the lines in the left-hand side rendering;
  -- * the line obtained by glueing together the last and the first line of
  --   the left- and right-hand side renderings, respectively; and
  -- * the lines obtained by prefixing all but the first of the lines in the
  --   right-hand side rendering with an amount of whitespace that matches the
  --   width of the last line in the left-hand side rendering
  ppBesides (R []) rr               = rr
  ppBesides rl (R [])               = rl
  ppBesides (R psl) (R (pr : psr')) = let (psl', pl) = unSnoc psl
                                          prefix     = ws (width pl)
                                      in  R $ psl' ++ [pl `mappend` pr] ++
                                                map (prefix `mappend`) psr'

  ppColour _ r@(R [])     = r
  ppColour c (R [p])      = R [beginColour c `mappend` p `mappend` endColour]
  ppColour c (R (p : ps)) = let (ps', p') = unSnoc ps
                            in  R $ beginColour c `mappend` p :
                                      ps' ++ [p' `mappend` endColour]

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Provides an abstraction of a 'Rendering': the total height, total width,
-- and the width of the last line of a rendering.
data Dim = D !Int !Int !Int

instance PP Dim where
  ppEmpty                            = D 0 0 0
  ppText txt                         = D 1 w w where w = length txt
  ppIndent n (D h w l)               = D h (w + n) (l + n)
  ppAbove (D hl wl _) (D hr wr lr)   = D (hl + hr) (wl `max` wr) lr
  ppBesides (D hl _ ll) (D hr wr lr) = D (hl + hr - 1) (ll + wr) (ll + lr)
  ppColour _ d                       = d

-------------------------------------------------------------------------------
-- Formats
-------------------------------------------------------------------------------

-- | A concrete 'Rendering' together with its 'Dim'-abstraction.
data Format a = Rendering a :^ Dim

instance Printer a => PP (Format a) where
  ppEmpty                         = ppEmpty         :^ ppEmpty
  ppText txt                      = ppText txt      :^ ppText txt
  ppIndent n (r :^ d)             = ppIndent n r    :^ ppIndent n d
  ppAbove (rl :^ dl) (rr :^ dr)   = ppAbove rl rr   :^ ppAbove dl dr
  ppBesides (rl :^ dl) (rr :^ dr) = ppBesides rl rr :^ ppBesides dl dr
  ppColour c (r :^ d)             = ppColour c r    :^ ppColour c d

-------------------------------------------------------------------------------
-- Formatters
-------------------------------------------------------------------------------

-- | Represents the minimal amount of horizontal space needed to render a
-- document, expressed in terms of the total width and the width of the last
-- line in the most efficient rendering.
type Frame = (Int, Int)

-- | The top-level data structure in the rendering hierarchy: it provides the
-- minimal amount of horizontal space needed to render the document (expressed
-- in terms of a 'Frame') together with a function that spawns all possible
-- formats for a document that fit within a specifed amount of horizontal
-- space (expressed in terms of the total width of a rendering).
--
-- @Formatter@s maintain the invariant that the lists of possible formats are
-- sorted on ascending heights and descending widths /simultaneously/.

data Formatter a = Frame :< (Int -> [Format a]) 

instance Printer a => PP (Formatter a) where
  ppEmpty    = (0, 0) :< \_ -> [ppEmpty]
  ppText txt = let w = length txt
               in  (w, w) :< \wmax -> if w <= wmax then [ppText txt] else []

  ppIndent n ((wmin, lmin) :< spwn)
    = (wmin + n, lmin + n) :< \wmax ->
        [ppIndent n fmt | fmt <- spwn (wmax - n)]

  -- * if we stack to documents, we have to be careful not to generate too many
  --   formats;
  -- * more specifically, we do not have to take the cross product of the lists
  --   of formats for the upper document and the lower document
  -- * rather, if we move through the lists in order of descending widths, it
  --   is sufficient to combine each format with at most one format of lesser
  --   or equal width: combining it with other formats of lesser width cannot
  --   yield narrower formats, for the width of the format itself will
  --   determine the combined width with those other formats;
  -- * therefore, we can process the two lists of formats in a left-to-right
  --   fashion, as implemented by the auxiliary function 'stack'
  
  ppAbove ((wminl, _) :< spwnl) ((wminr, lminr) :< spwnr)
    = (wminl `max` wminr, lminr) :< \wmax -> 
        if   wminl <= wmax && wminr <= wmax
        then stack (spwnl wmax) (spwnr wmax)
        else []

  -- * a straightforward approach to \"dovetailing\" would be to first spawn
  --   all formats that fit the available horizontal space for the left-hand 
  --   document and then, /for each of these formats/, determine the amount of
  --   space left and spawn all fiting formats for the right-hand document;
  -- * however, this can be rather costly if there are many fitting formats for
  --   the left-hand document;
  -- * therefore, we adopt an alternative technique: we spawn all left-hand
  --   formats and use the lower bound on the widths of their last lines
  --   (provided by the 'Frame' component of the left-hand formatter) as a
  --   conservative estimate on the amount of horizontal space available for
  --   the right-hand formats;
  -- * this way, we may end up with combined formats that are too wide
  -- * so, after combining the formats, the ones that occupy too much space are
  --   cut off
  ppBesides ((_, lminl) :< spwnl) ((wminr, lminr) :< spwnr)
    = (lminl + wminr, lminl + lminr) :< \wmax ->
        let fmtsl = spwnl wmax
            fmtsr = spwnr (wmax - lminl)
        in  if   lminl + wminr <= wmax
            then (cut wmax . foldr merge [])
                   [[ppBesides fmtl fmtr | fmtr <- fmtsr] | fmtl <- fmtsl]
            else []

  ppColour c (frm :< spwn) = (frm :< \wmax -> map (ppColour c) (spwn wmax))
        
-- | Drops all 'Format's that do not fit within a specified amount of
-- horizontal space (expressed as the total width of a rendering).
cut :: Int -> [Format a] -> [Format a]
cut wmax = dropWhile tooWide    -- we can use 'dropWhile', for we may assume 
                                -- that the formats are sorted on descending
                                -- widths
  where
    tooWide (_ :^ D _ w _) = w > wmax

-- | A (proper) partial order on formats, according to which the lists
-- produced by 'Formatter's are sorted.
(<=.) :: Format a -> Format a -> Bool
(_ :^ D hl wl _) <=. (_ :^ D hr wr _) = hl <= hr && wl >= wr

-- | Merges two lists of 'Format's preserving the '(<=.)'-ordering of their 
-- elements.
--
-- Because '(<=.)' is a proper partial order, elements from the right-hand 
-- list may be dropped if they cannot be merged into the left-hand side list.

merge :: [Format a] -> [Format a] -> [Format a]
merge [] fmtsr                                    = fmtsr
merge fmtsl []                                    = fmtsl
merge fmtsl@(fmtl : fmtsl') fmtsr@(fmtr : fmtsr')
  | fmtl <=. fmtr                                 = fmtl : merge fmtsl' fmtsr
  | fmtr <=. fmtl                                 = fmtr : merge fmtsl  fmtsr'
  | otherwise                                     = fmtl : merge fmtsl' fmtsr'

-- | Takes two sorted lists of formats and combines them to form a list that
-- containts the most efficient formats obtained by stacking formats from the
-- first list on top of formats from the second list.
--
-- (Auxiliary to the implementation of 'ppAbove' for 'Formatter'.)

stack :: Printer a => [Format a] -> [Format a] -> [Format a]
stack [] fmtsr                                    = []
stack fmtsl []                                    = []
stack fmtsl@(fmtl : fmtsl') fmtsr@(fmtr : fmtsr') = case (fmtl, fmtr) of
  (_ :^ D _ wl _, _ :^ D _ wr _)
    | wl <= wr  -> ppAbove fmtl fmtr : stack fmtsl fmtsr'
    | otherwise -> ppAbove fmtl fmtr : stack fmtsl' fmtsr

-- | Takes a single-line text and produces a @Formatter@ for 
-- its \'paragraph fill'.
ppWrapped :: Printer a => String -> Formatter a
ppWrapped txt = frm :< \wmax -> if null wrds then [ppText ""] else
                                let ws = [wmax, wmax - minw .. minw]
                                in  [ppFill w wrds | w <- ws]
  where
    wrds                      = [(txt', length txt') | txt' <- words txt]
    lengths                   = map snd wrds
    frm@(minw, _) | null wrds = (0, 0)
                  | otherwise = (maximum lengths, last lengths)

-- | Takes a list of words and their lengths and produces the 'Format' for its
-- \"paragraph\" fill with respect to a given paragraph width.
-- Words on the same line are separated by a single whitespace character.
ppFill :: Printer a => Int -> [(String, Int)] -> Format a
ppFill _    []   = ppText ""
ppFill wmax wrds = foldr1 ppAbove . map (ppText . unwords) . fill (-1) id $
                   wrds
  where
    fill _ acc [] = [acc []]
    fill n acc wrds@((txt, len) : wrds')
      | n + len <= wmax - 1 = fill (n + len + 1) (acc . (txt :)) wrds'
      | otherwise           = acc [] : fill (- 1) id wrds
    
-- | Parallelisation: combines two alternative formatters into a single
-- formatter.
ppSplit :: Formatter a -> Formatter a -> Formatter a
ppSplit ((wminl, lminl) :< spwnl) ((wminr, lminr) :< spwnr)
  = (wminl `min` wminr, lminl `min` lminr) :< \wmax -> case () of
      _ | wminl <= wmax && wminr < wmax -> merge (spwnl wmax) (spwnr wmax)
        | wminl <= wmax                 -> spwnl wmax
        | wminr <= wmax                 -> spwnr wmax
        | otherwise                     -> []

-- | Joins all parallel formats into a single format.
ppJoin :: Formatter a -> Formatter a
ppJoin (frm :< spwn) = frm :< \wmax -> case spwn wmax of
  []      -> []
  fmt : _ -> [fmt]

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

-- | Retrieves a concrete printer for a 'Format'.
toPrinter :: Printer a => Format a -> a
toPrinter (R ps :^ _) = mconcat (intersperse newLine ps)

-- | Produces a formatter for a 'Doc'.

fromDoc :: Printer a => Doc -> Formatter a
fromDoc Empty             = ppEmpty
fromDoc (Text txt)        = ppText txt
fromDoc (Wrapped txt)     = ppWrapped txt
fromDoc (Indent n d)      = ppIndent n (fromDoc d)
fromDoc (Above Empty r)   = fromDoc r
fromDoc (Above l Empty)   = fromDoc l
fromDoc (Above l r)       = ppAbove (fromDoc l) (fromDoc r)
fromDoc (Besides Empty r) = fromDoc r
fromDoc (Besides l Empty) = fromDoc l
fromDoc (Besides l r)     = ppBesides (fromDoc l) (fromDoc r)
fromDoc (Split l r)       = ppSplit (fromDoc l) (fromDoc r)
fromDoc (Join d)          = ppJoin (fromDoc d)
fromDoc (Colour _ Empty)  = ppEmpty
fromDoc (Colour c d)      = ppColour c (fromDoc d)

-- | Produces a printer for the most efficient rendering of a document within
-- a specified amount of horizontal space.
-- Returns 'Nothing' if the document cannot be rendered within the given amount
-- of space.
render :: Printer a => Int -> Doc -> Maybe a
render wmax doc = listToMaybe $ fmap toPrinter $ spwn wmax
  where
    _ :< spwn = fromDoc doc

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Breaks up a list into its 'init' and 'last' parts.
-- Undefined for empty lists.
unSnoc :: [a] -> ([a], a)
unSnoc = unsnc id
  where
    unsnc acc [x]      = (acc [], x)
    unsnc acc (x : xs) = unsnc (acc . (x :)) xs