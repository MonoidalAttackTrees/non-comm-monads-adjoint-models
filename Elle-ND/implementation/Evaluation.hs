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

eval_T :: Fresh m => Ctx_T -> Term_T -> ExceptT TypeException m Term_T
eval_T _ Triv_T = return Triv_T
eval_T g (LetU_T t1 t2) = do
  ctx1 <- extractCtx_T g t1
  ty <- typeCheck_T ctx1 t1
  case ty of
    Unit_T -> return t2
    _ -> throwError InvalidTypeError
eval_T g (LetT_T (Tens_T t1 t2) (Tensor_T ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, t) <- unbind b'
  ctx1 <- extractCtx_T g t1
  ctx2 <- extractCtx_T g t2
  ty1' <- typeCheck_T ctx1 t1
  ty2' <- typeCheck_T ctx2 t2
  if ((ty1', ty2') == (ty1, ty2))
    then return (subst tm2 t2 (subst tm1 t1 t))
    else throwError InvalidTypeError
eval_T g (App_T (Lam_T ty b) t) = do
  (tm, t') <- unbind b
  ctx <- extractCtx_T g t
  ty' <- typeCheck_T ctx t
  if (ty' == ty)
    then return (subst tm t t')
    else throwError InvalidTypeError
eval_T g (App_T t1 t2) = do
  ctx1 <- extractCtx_T g t1
  ctx2 <- extractCtx_T g t2
  t1' <- eval_T ctx1 t1
  t2' <- eval_T ctx2 t2
  return (App_T t1' t2')



eval_S :: Fresh m => Ctx_T -> Ctx_S -> Term_S -> ExceptT TypeException m Term_S
eval_S _ _ Triv_S = return Triv_S
eval_S g d (LetU_S s1 s2) = do
  d' <- extractCtx_S d s1
  ty <- typeCheck_S g d' s1
  case ty of
    Unit_S -> return s2
    _      -> throwError InvalidTypeError
eval_S g d (LetT_S (Tens_S s1 s2) (Tensor_S ty1 ty2) b) = do
  (x1, b') <- unbind b
  (x2, s) <- unbind b'
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  ty1' <- typeCheck_S g ctx1 s1
  ty2' <- typeCheck_S g ctx2 s2
  if ((ty1', ty2') == (ty1, ty2))
    then return $ subst x2 s2 (subst x1 s1 s)
    else throwError InvalidTypeError
eval_S g d (AppL_S (LamL_S ty b) s) = do
  (x, s') <- unbind b
  ctx <- extractCtx_S d s
  ty' <- typeCheck_S g ctx s
  if (ty' == ty)
    then return $ subst x s s'
    else throwError InvalidTypeError
eval_S g d (AppR_S (LamR_S ty b) s) = do
  (x, s') <- unbind b
  ctx <- extractCtx_S d s
  ty' <- typeCheck_S g ctx s
  if (ty' == ty)
    then return $ subst x s s'
    else throwError InvalidTypeError
eval_S g d (AppL_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  s1' <- eval_S g ctx1 s1
  s2' <- eval_S g ctx2 s2
  return (AppL_S s1' s2')
eval_S g d (AppR_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  s1' <- eval_S g ctx1 s1
  s2' <- eval_S g ctx2 s2
  return (AppL_S s1' s2')
eval_S g d (Derelict_S s) = do
  case s of
    Gt_T s' -> return s'
    _       -> throwError InvalidArgError










  
