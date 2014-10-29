-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.Parser.Validation
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a validator that matches ATerms against schematic descriptions of
-- their structure.
--
-------------------------------------------------------------------------------

module CCO.Tree.Parser.Validation (
    -- * Schemes
    Scheme (..)

    -- * Validation
  , validate
) where

import CCO.Feedback    (Feedback, Message (Error), message)
import CCO.Printing    (Doc, above, text, wrapped, (>|<), Printable (pp))
import CCO.Tree.ATerm  (Con, ATerm (..))
import Data.List       (nub, sort)

-------------------------------------------------------------------------------
-- Schemes
-------------------------------------------------------------------------------

-- | Describes the top-level shape of an 'ATerm' node.
data Scheme = IntegerTerm        -- ^ Integer literal.
            | FloatTerm          -- ^ Floating-point literal.
            | StringTerm         -- ^ String literal.
            | AppTerm Con Int    -- ^ Application of a constructor to a
                                 --   specified number of arguments.
            | TupleTerm Int      -- ^ Tuple of a specified arity.
            | ListTerm           -- ^ List.
            deriving (Eq)

-- | Retrieves the scheme of an 'ATerm'.
schemeOf :: ATerm -> Scheme
schemeOf (Integer _)    = IntegerTerm
schemeOf (Float _)      = FloatTerm
schemeOf (String _)     = StringTerm
schemeOf (App conid as) = AppTerm conid (length as)
schemeOf (Tuple as)     = TupleTerm (length as)
schemeOf (List _)       = ListTerm
schemeOf (Ann a _)      = schemeOf a

-- | An 'ATerm' and its 'Scheme'.
data Focus = ATerm ::: Scheme

-- | Retrieves the 'Focus' for an 'ATerm'.
focusOn :: ATerm -> Focus
focusOn aterm = aterm ::: schemeOf aterm

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Validates an 'ATerm' against a family of 'Scheme's and produces the
-- a matching 'Scheme' if one is found.
-- If no matching 'Scheme' was found, an appropriate error message is issued.
validate :: ATerm -> [Scheme] -> Feedback Scheme
validate aterm schemes
  | scheme `elem` schemes = return scheme
  | otherwise             = do message (Error (pp (diagnose focus schemes)))
                               return scheme
  where
    focus@(_ ::: scheme) = focusOn aterm

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

-- | Diagnosis of  why an 'ATerm' cannot be parsed.
data Diagnosis = NoScheme ATerm  
                 -- ^ There was no 'Scheme' to match the 'ATerm' against.

               | ArityMismatch ATerm Con Int [Int]
                 -- ^ The arity of an application of a constructor does not
                 --   match against the arities that were expected for an
                 --   application of that constructor.

               | SchemeMismatch ATerm Scheme [Scheme]
                 -- ^ The 'Scheme' of the 'ATerm' does not match against any of
                 --   the expected 'Scheme's.

instance Printable Diagnosis where pp = ppDiagnosis

-- | Takes a 'Focus' and a familiy of 'Scheme's that do not match with it, and
-- produces a 'Diagnosis'.
diagnose :: Focus -> [Scheme] -> Diagnosis

diagnose (aterm ::: _) [] = NoScheme aterm

diagnose (aterm ::: scheme@(AppTerm conid arity)) expected
  | null expectedArities = SchemeMismatch aterm scheme expected
  | otherwise            = ArityMismatch aterm conid arity expectedArities
  where
    expectedArities = 
      [ expectedArity |
          AppTerm conid' expectedArity <- expected, conid' == conid ]

diagnose (aterm ::: scheme) expected = SchemeMismatch aterm scheme expected

-------------------------------------------------------------------------------
-- Pretty-printing diagnoses
-------------------------------------------------------------------------------

-- | Pretty prints an 'Diagnosis'.
ppDiagnosis :: Diagnosis -> Doc

ppDiagnosis (NoScheme aterm) = above [ppHeader, text " ", ppTerm]
  where
    ppHeader = wrapped "Error in ATerm."
    ppTerm   = text "? in term : " >|< pp aterm

ppDiagnosis (ArityMismatch aterm conid arity expectedArities)
  = above [ppHeader, text " ", ppTerm]
  where
    ppHeader = wrapped $ "Error in ATerm: " ++ conid ++ " " ++
                         describeExpectedArities expectedArities ++
                         ", but " ++ describeGivenArity arity ++ "."
    ppTerm   = text "? in term : " >|< pp aterm

ppDiagnosis (SchemeMismatch aterm scheme expected)
  = above [ppHeader, text " ", ppUnexpected, ppExpected, ppTerm]
  where
    ppHeader     = wrapped $ "Error in ATerm."
    ppUnexpected = text "? unexpected : " >|< wrapped (describeScheme scheme)
    ppExpected   = text "? expected   : " >|< (wrapped . disjunction)
                                                (map describeScheme expected)
    ppTerm       = text "? in term    : " >|< pp aterm

-- | Retrieves a textual description of a 'Scheme'.
describeScheme :: Scheme -> String
describeScheme IntegerTerm       = "integer literal"
describeScheme FloatTerm         = "floating-point literal"
describeScheme StringTerm        = "string literal"
describeScheme (AppTerm conid _) = conid
describeScheme (TupleTerm n)     = show n ++ "-tuple"
describeScheme ListTerm          = "list"

-- | Retrieves a textual description of an arity given to a constructor
-- application.
-- Produces a phrase like \"<arity> were given\".
describeGivenArity :: Int -> String
describeGivenArity 0     = "none were given"
describeGivenArity 1     = "1 was given"
describeGivenArity arity = show arity ++ " were given"

-- | Retrieves a textual description of the arities expected for a constructor
-- application.
-- Produces a phrase like \"takes <arity> arguments\".
describeExpectedArities :: [Int] -> String
describeExpectedArities = descr . sort . nub
  where
    descr [0]     = "takes no arguments"
    descr [0, 1]  = "takes 0 or 1 argument"
    descr arities = "takes " ++ disjunction (map show arities) ++ " arguments"

-- | Takes a nonempty list of textual items and produces a text that 
-- expresses their disjunction: \"<item>, <item>, ..., or <item>\".
disjunction :: [String] -> String
disjunction [x]       = x
disjunction [x, y]    = x ++ " or " ++ y
disjunction [x, y, z] = x ++ ", " ++ y ++ ", or " ++ z
disjunction (x : xs)  = x ++ ", " ++ disjunction xs