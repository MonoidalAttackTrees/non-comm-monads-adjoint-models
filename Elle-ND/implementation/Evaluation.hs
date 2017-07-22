{-# LANGUAGE FlexibleContexts #-}
module Evaluation where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax

-----------------------------------------------------------------------
-- Checks if a T term is a value, i.e. cannot be further evaluated   --
-----------------------------------------------------------------------
isVal_T :: Fresh m => Term_T -> m Bool
isVal_T Triv_T = return True
isVal_T (Var_T x) = return True
isVal_T (Tens_T t1 t2) = do
  t1' <- isVal_T t1
  case t1' of
    True  -> isVal_T t2
    False -> return False
isVal_T (Lam_T ty b) = do
  (x, t) <- unbind b
  isVal_T t
isVal_T (Gt_T s) = isVal_S s
isVal_T _ = return False

-----------------------------------------------------------------------
-- Checks if a S term is a value, i.e. cannot be further evaluated   --
-----------------------------------------------------------------------
isVal_S :: Fresh m => Term_S -> m Bool
isVal_S Triv_S = return True
isVal_S (Var_S x) = return True
isVal_S (Tens_S s1 s2) = do
  s1' <- isVal_S s1
  case s1' of
    True  -> isVal_S s2
    False -> return False
isVal_S (LamL_S ty b) = do
  (x, s) <- unbind b
  isVal_S s
isVal_S (LamR_S ty b) = do
  (x, s) <- unbind b
  isVal_S s
isVal_S (Ft_S t) = isVal_T t
isVal_S _ = return False

-----------------------------------------------------------------------
-- One-step evaluation functions for the T terms                     --
-----------------------------------------------------------------------
eval_T :: Fresh m => Term_T -> m Term_T
eval_T (LetU_T t1 t2) = return t2
eval_T (LetT_T (Tens_T t1 t2) (Tensor_T ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, t) <- unbind b'
  return $ subst tm2 t2 (subst tm1 t1 t)
eval_T (App_T (Lam_T ty b) t) = do
  (tm, t') <- unbind b
  return $ subst tm t t'
eval_T (App_T t1 t2) = do
  t1' <- eval_T t1
  t2' <- eval_T t2
  return (App_T t1' t2')
eval_T t = return t

-----------------------------------------------------------------------
-- One-step evaluation functions for the S terms                     --
-----------------------------------------------------------------------
eval_S :: Fresh m => Term_S -> m Term_S
eval_S (LetU_S s1 s2) = return s2
eval_S (LetT_S (Tens_S s1 s2) (Tensor_S ty1 ty2) b) = do
  (x1, b') <- unbind b
  (x2, s) <- unbind b'
  return $ subst x2 s2 (subst x1 s1 s)
eval_S (AppL_S (LamL_S ty b) s) = do
  (x, s') <- unbind b
  return $ subst x s s'
eval_S (AppR_S (LamR_S ty b) s) = do
  (x, s') <- unbind b
  return $ subst x s s'
eval_S (AppL_S s1 s2) = do
  s1' <- eval_S s1
  s2' <- eval_S s2
  return (AppL_S s1' s2')
eval_S (AppR_S s1 s2) = do
  s1' <- eval_S s1
  s2' <- eval_S s2
  return (AppL_S s1' s2')
eval_S (Derelict_S (Gt_T s)) = return s
eval_S s = return s

-----------------------------------------------------------------------
-- Recursive Evaluation for T                                        --
-----------------------------------------------------------------------
evalRec_T :: Fresh m => Term_T -> m Term_T
evalRec_T t = do
  t' <- eval_T t
  val <- isVal_T t'
  if val
    then return t'
    else evalRec_T t'

-----------------------------------------------------------------------
-- Recursive Evaluation for S                                        --
-----------------------------------------------------------------------
evalRec_S :: Fresh m => Term_S -> m Term_S
evalRec_S s = do
  s' <- eval_S s
  val <- isVal_S s'
  if val
    then return s'
    else evalRec_S s'







  
