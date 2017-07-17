{-# LANGUAGE TemplateHaskell,       FlexibleInstances,
            MultiParamTypeClasses, FlexibleContexts, 
            UndecidableInstances                      #-}
--------------------------------------------------------------------------------------
-- This file contains the syntax and syntax manipulators of the non linear of Elle. --
--------------------------------------------------------------------------------------

module Syntax (module Unbound.LocallyNameless,
               module Unbound.LocallyNameless.Alpha,
               Type(..),
               Term(..),
               TmName,
               replace,
               replaceTest,
               TypeException(..),
               n2s) where

import Prelude
import Data.List
import Unbound.LocallyNameless hiding (fv)
import Unbound.LocallyNameless.Alpha
import Errors

data Type = Unit
          | Tensor Type Type
          | Imp Type Type
          | ImpL Type Type
          | ImpR Type Type
          deriving (Show, Eq)


-- Name:
-- an abstract type for representing names, provided by Unbound
-- Names are indexed by the sorts of things to which they can refer,
-- or more precisely, the sorts of things which can be substituted for them
type TmName = Name Term

data Term = Triv
          | Var TmName     -- Variable is simply a name for some Term
          -- LetU t1 t2
          -- let t1 : Unit be triv in t2
          | LetU Term Term
          -- Bind p b:
          -- a pair consisting of a pattern p and a body b
          -- The pattern may bind names which occur in the body
          -- LetT t1 (X * Y) (Bind x (Bind y t2))
          -- let t1 : X * Y be x * y in t2
          --
          | LetT Term Type (Bind TmName (Bind TmName Term))
          | Tens Term Term
          | Lam Type (Bind TmName Term)
          | App Term Term
          -- Ex t1 t2 (Bind x1 (Bind x2 t3))
          -- ex x1, x2 with t1, t2 in t3
          | Ex Term Term (Bind TmName (Bind TmName Term))
          -- ContrR t1 t2 (Bind x t3)
          -- contrR x as t1, t2 in t3
          | ContrR Term Term (Bind TmName Term)
          -- ContrL t1 t2 (Bind x t3)
          -- contrL x as t1, t2 in t3
          | ContrL Term Term (Bind TmName Term)
          -- Weak (Bind x t)
          -- weak x in t
          | Weak (Bind TmName Term)
          deriving (Show)


-- automatically derive a bunch of behind-the-scenes, boilerplate instances for Term and Type
$(derive [''Term,''Type])
-- make Term an instance of Alpha,
-- which provides most of the methods we will need
-- for working with the variables and binders within `Term`s
instance Alpha Term
instance Alpha Type

-- an instance for "Subst b a" means:
-- we can use the "subst" function
-- to substitute things of type b for Names occurring in things of type a
instance Subst Term Type
instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

replace :: Name Term -> Term -> Term -> Term
replace nm t t' = subst nm t t'

replaceTest :: Fresh m => Name Term -> Term -> Term -> m Term
replaceTest n t t' = return $ replace n t t'

-- NameToString
n2s :: Name a -> String
n2s = name2String
