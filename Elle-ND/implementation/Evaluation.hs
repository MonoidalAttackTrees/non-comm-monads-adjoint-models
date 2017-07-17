{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------
-- One-step evaluation functions for the terms                       --
-----------------------------------------------------------------------
module Evaluation where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax
import TypeCheck

eval :: Fresh m => Ctx -> Term -> ExceptT TypeException m Term
eval _ Triv = return Triv
eval g (LetU t1 t2) = do
  ctx1 <- extractCtx g t1
  ty <- typeCheck ctx1 t1
  case ty of
    Unit -> return t2
    _ -> throwError InvalidTypeError
eval g (LetT (Tens t1 t2) (Tensor ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, t) <- unbind b'
  ctx1 <- extractCtx g t1
  ctx2 <- extractCtx g t2
  ty1' <- typeCheck ctx1 t1
  ty2' <- typeCheck ctx2 t2
  if ((ty1', ty2') == (ty1, ty2))
    then return (subst tm2 t2 (subst tm1 t1 t))
    else throwError InvalidTypeError
eval g (App (Lam ty b) t) = do
  (tm, t') <- unbind b
  ctx <- extractCtx g t
  ty' <- typeCheck ctx t
  if (ty' == ty)
    then return (subst tm t t')
    else throwError InvalidTypeError
eval g (App t1 t2) = do
  ctx1 <- extractCtx g t1
  ctx2 <- extractCtx g t2
  t1' <- eval ctx1 t1
  t2' <- eval ctx2 t2
  return (App t1' t2')
  
