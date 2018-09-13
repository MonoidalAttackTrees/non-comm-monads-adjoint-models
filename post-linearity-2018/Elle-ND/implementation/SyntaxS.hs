{-# LANGUAGE TemplateHaskell,       FlexibleInstances,
            MultiParamTypeClasses, FlexibleContexts, 
            UndecidableInstances                      #-}
--------------------------------------------------------------------------------------
-- This file contains the syntax and syntax manipulators of the non linear of Elle. --
--------------------------------------------------------------------------------------

module SyntaxS (module Unbound.LocallyNameless,
               module Unbound.LocallyNameless.Alpha,
               Type_S(..),
               Term_S(..),
               TmName,
               replace_S,
               replaceTest_S,
               TypeException(..),
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless hiding (fv)
import Unbound.LocallyNameless.Alpha
import Errors

import Syntax

data Type_S = Unit_S
            | Tensor_S Type_S Type_S
            | Imp_S Type_S Type_S
            | ImpL_S Type_S Type_S
            | ImpR_S Type_S Type_S
            deriving (Show, Eq)
--            | F_S Type_T

type TmName = Name Term_S

--          | LetU_T Term_T Term_T
--          | LetT_T Term_T Type_T (Bind TmName (Bind TmName Term_T))
--          | Tens_T Term_T Term_T
--          | Lam_T Type_T (Bind TmName Term_T)
--          | App_T Term_T Term_T

data Term_S = Triv_S
            | Var_S TmName


$(derive [''Term_S, ''Type_S])
instance Alpha Term_S
instance Alpha Type_S

instance Subst Term_S Type_S
instance Subst Term_S Term_S where
  isvar (Var_S x) = Just (SubstName x)
  isvar _         = Nothing

replace_S :: Name Term_S -> Term_S -> Term_S -> Term_S
replace_S nm t t' = subst nm t t'

replaceTest_S :: Fresh m => Name Term_S -> Term_S -> Term_S -> m Term_S
replaceTest_S n t t' = return $ replace_S n t t'

-- NameToString
n2s :: Name a -> String
n2s = name2String
