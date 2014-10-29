-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.Instances
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Tree' instances for types from the Prelude.
--
-------------------------------------------------------------------------------

module CCO.Tree.Instances () where

import CCO.Feedback           (Message (Error), errorMessage)
import CCO.Printing
import CCO.Tree.ATerm         (ATerm (..), Con)
import CCO.Tree.Base          (Tree (..))
import CCO.Tree.Parser
import Control.Applicative    (Applicative (pure, (<*>)), (<$>))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Tree Bool where
  fromTree False = App "False" []
  fromTree True  = App "True"  []

  toTree = parseTree [ app "False" (pure False)
                     , app "True"  (pure True)
                     ]

instance Tree a => Tree (Maybe a) where
  fromTree Nothing  = App "Nothing" []
  fromTree (Just x) = App "Just"    [fromTree x]

  toTree = parseTree [ app "Nothing" (pure Nothing)
                     , app "Just"    (Just <$> arg)
                     ]

instance (Tree a, Tree b) => Tree (Either a b) where
  fromTree (Left x)  = App "Left"  [fromTree x]
  fromTree (Right y) = App "Right" [fromTree y]

  toTree = parseTree [ app "Left"  (Left <$> arg)
                     , app "Right" (Right <$> arg)
                     ]

instance Tree Ordering where
  fromTree LT = App "LT" []
  fromTree EQ = App "EQ" []
  fromTree GT = App "GT" []

  toTree = parseTree [ app "LT" (pure LT)
                     , app "EQ" (pure EQ)
                     , app "GT" (pure GT)
                     ]

instance Tree Char where
  fromTree c = String [c]

  toTree aterm = do
    s <- parseTree [string] aterm
    case s of
      [c] -> return c
      cs  -> errorMessage (ppNonsingletonStringError aterm cs)

  fromTrees s = String s
  toTrees     = parseTree [string]

instance Tree Int where
  fromTree n = Integer (toInteger n)
  toTree     = parseTree [fromInteger <$> integer]

instance Tree Integer where
  fromTree n = Integer n
  toTree     = parseTree [integer]

instance Tree Float where
  fromTree r = Float (realToFrac r)
  toTree     = parseTree [realToFrac <$> float]

instance Tree Double where
  fromTree r = Float r
  toTree     = parseTree [float]

instance Tree a => Tree [a] where
  fromTree = fromTrees
  toTree   = toTrees

instance (Tree a, Tree b) => Tree (a, b) where
  fromTree (x, y) = Tuple [fromTree x, fromTree y]
  toTree          = parseTree [tuple ((,) <$> arg <*> arg)]

instance (Tree a, Tree b, Tree c) => Tree (a, b, c) where
  fromTree (x, y, z) = Tuple [fromTree x, fromTree y, fromTree z]
  toTree             = parseTree [tuple ((,,) <$> arg <*> arg <*> arg)]

instance (Tree a, Tree b, Tree c, Tree d) => Tree (a, b, c, d) where
  fromTree (w, x, y, z) =
    Tuple [fromTree w, fromTree x, fromTree y, fromTree z]

  toTree = parseTree [tuple ((,,,) <$> arg <*> arg <*> arg <*> arg)]

instance (Tree a, Tree b, Tree c, Tree d, Tree e) => Tree (a, b, c, d, e) where
  fromTree (v, w, x, y, z) =
    Tuple [fromTree v, fromTree w, fromTree x, fromTree y, fromTree z]

  toTree = parseTree [tuple ((,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg)]

instance (Tree a, Tree b, Tree c, Tree d, Tree e, Tree f) =>
         Tree (a, b, c, d, e, f) where
  fromTree (u, v, w, x, y, z) = Tuple [ fromTree u, fromTree v, fromTree w
                                      , fromTree x, fromTree y, fromTree z ]

  toTree =
    parseTree [tuple ((,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg)]

instance (Tree a, Tree b, Tree c, Tree d, Tree e, Tree f, Tree g) =>
         Tree (a, b, c, d, e, f, g) where
  fromTree (t, u, v, w, x, y, z) = Tuple [ fromTree t, fromTree u, fromTree v
                                         , fromTree w, fromTree x, fromTree y
                                         , fromTree z ]

  toTree = parseTree
    [tuple ((,,,,,,) <$> arg <*> arg <*> arg <*> arg <*> arg <*> arg <*> arg)]

instance Tree () where
  fromTree _ = Tuple []
  toTree     = parseTree [tuple (pure ())]

-------------------------------------------------------------------------------
-- Pretty printing error messages
-------------------------------------------------------------------------------

-- | Takes a nonsingleton 'String' and produces an error message indicating
-- that a singleton 'String' in a given 'ATerm' was expected.
ppNonsingletonStringError :: ATerm -> String -> Doc
ppNonsingletonStringError aterm s
  = above [ppHeader, ppUnexpected, ppExpected, ppTerm]
  where
    ppHeader     = wrapped ("Error in ATerm.")
    ppUnexpected = text "*** Unexpected : " >|< wrapped (describeString s)
    ppExpected   = text "*** Expected   : " >|< wrapped
                                                  "singleton string literal"
    ppTerm       = text "*** In term    : " >|< pp aterm

-- | Retrieves a textual description of a 'String'.
describeString :: String -> String
describeString "" = "empty string literal"
describeString _  = "nonsingleton string literal"