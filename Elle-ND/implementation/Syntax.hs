{-# LANGUAGE TemplateHaskell,       FlexibleInstances,
            MultiParamTypeClasses, FlexibleContexts, 
            UndecidableInstances                      #-}
--------------------------------------------------------------------------------------
-- This file contains the syntax and syntax manipulators of the non linear of Elle. --
--------------------------------------------------------------------------------------

module Syntax (module Unbound.LocallyNameless,
               module Unbound.LocallyNameless.Alpha,
               Type_T(..),
               Type_S(..),
               Term_T(..),
               Term_S(..),
               TmName_T,
               TmName_S,
               replace_T,
               replaceTest_T,
               varT2varS,
               varS2varT,
               TypeException(..),
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless hiding (fv)
import Unbound.LocallyNameless.Alpha
import Errors

data Type_T = Unit_T
          | Tensor_T Type_T Type_T
          | Imp_T Type_T Type_T
          | Gty_T Type_S
          deriving (Show, Eq)

data Type_S = Unit_S
            | Tensor_S Type_S Type_S
            | Imp_S Type_S Type_S
            | ImpL_S Type_S Type_S
            | ImpR_S Type_S Type_S
            | Fty_S Type_T
            deriving (Show, Eq)


-- Name:
-- an abstract type for representing names, provided by Unbound
-- Names are indexed by the sorts of things to which they can refer,
-- or more precisely, the sorts of things which can be substituted for them
type TmName_T = Name Term_T

data Term_T = Triv_T
          | Var_T TmName_T     -- Variable is simply a name for some Term
          -- LetU_T t1 t2
          -- let t1 : Unit be triv in t2
          | LetU_T Term_T Term_T
          -- Bind p b:
          -- a pair consisting of a pattern p and a body b
          -- The pattern may bind names which occur in the body
          -- LetT_T t1 (X * Y) (Bind x (Bind y t2))
          -- let t1 : X * Y be x * y in t2
          | LetT_T Term_T Type_T (Bind TmName_T (Bind TmName_T Term_T))
          -- LetG_T t (Gt_T ty) (Bind x s)
          -- let t : (Gt_T ty) be x in s
          -- ?
          | LetG_T Term_T Type_T (Bind TmName_T Term_T)
          | Tens_T Term_T Term_T
          | Lam_T Type_T (Bind TmName_T Term_T)
          | App_T Term_T Term_T
          | Gt_T Term_S
          deriving (Show)

type TmName_S = Name Term_S

data Term_S = Triv_S
            | Var_S TmName_S
            | LetU_S Term_S Term_S
            | LetT_S Term_S Type_S (Bind TmName_S (Bind TmName_S Term_S))
            -- ?
            | LetF_S Term_S Type_S (Bind TmName_S Term_S)
            | Tens_S Term_S Term_S
            | LamL_S Type_S (Bind TmName_S Term_S)
            | LamR_S Type_S (Bind TmName_S Term_S)
            | AppL_S Term_S Term_S
            | AppR_S Term_S Term_S
            | Ex_S Term_S Term_S (Bind TmName_S (Bind TmName_S Term_S))
            | Ft_S Term_T
            | Derelict_S Term_T     -- Term_T must be Gt_T Term_S
            deriving (Show)


-- automatically derive a bunch of behind-the-scenes, boilerplate instances for Term and Type
$(derive [''Term_T, ''Type_T, ''Term_S, ''Type_S])
-- make Term an instance of Alpha,
-- which provides most of the methods we will need
-- for working with the variables and binders within `Term`s
instance Alpha Term_T
instance Alpha Type_T
instance Alpha Term_S
instance Alpha Type_S

-- an instance for "Subst b a" means:
-- we can use the "subst" function
-- to substitute things of type b for Names occurring in things of type a
instance Subst Term_T Type_T
instance Subst Term_T Term_T where
  isvar (Var_T x) = Just (SubstName x)
  isvar _         = Nothing

instance Subst Term_T Type_S
instance Subst Term_S Type_T
instance Subst Term_T Term_S
instance Subst Term_S Term_T

instance Subst Term_S Type_S
instance Subst Term_S Term_S where
  isvar (Var_S x) = Just (SubstName x)
  isvar _         = Nothing

replace_T :: Name Term_T -> Term_T -> Term_T -> Term_T
replace_T nm t t' = subst nm t t'

replaceTest_T :: Fresh m => Name Term_T -> Term_T -> Term_T -> m Term_T
replaceTest_T n t t' = return $ replace_T n t t'

replace_S :: Name Term_S -> Term_S -> Term_S -> Term_S
replace_S nm t t' = subst nm t t'

replaceTest_S :: Fresh m => Name Term_S -> Term_S -> Term_S -> m Term_S
replaceTest_S n t t' = return $ replace_S n t t'

-- NameToString
n2s :: Name a -> String
n2s = name2String

-- Variables for T to/from variables for S
varT2varS :: TmName_T -> TmName_S
varT2varS tmT = translate tmT

varS2varT :: TmName_S -> TmName_T
varS2varT tmS = translate tmS
