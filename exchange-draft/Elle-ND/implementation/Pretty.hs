{-# LANGUAGE FlexibleContexts #-}

------------------------------------------------------------------------
-- This file contains the ILL-Impl pretty printers.                   --
------------------------------------------------------------------------

module Pretty where

import Data.List
import Data.Char
import Text.Parsec
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Alpha
import Control.Monad.Except

import Syntax
import Parser



------------------------------------------------------------------------
-- Converts a type into a string                                      --
------------------------------------------------------------------------
prettyType_T :: Fresh m => Type_T -> m String
prettyType_T Unit_T = return "Unit"
prettyType_T (Tensor_T a b) = do
  a' <- prettyType_T a
  b' <- prettyType_T b
  return (a' ++ "(x)" ++ b')
prettyType_T (Imp_T a b) = do
  a' <- prettyType_T a
  b' <- prettyType_T b
  return ("(" ++ a' ++ ")" ++ "-o" ++ b')

------------------------------------------------------------------------
-- Converts a term into a string                                      --
------------------------------------------------------------------------
prettyTerm_T :: Fresh m => Term_T -> m String
prettyTerm_T Triv_T = return "Triv"
prettyTerm_T (Var_T x) = return.n2s $ x
prettyTerm_T (LetU_T t1 t2) = do
  t1' <- prettyTerm_T t1
  t2' <- prettyTerm_T t2
  return $ "let " ++ t1' ++ " : Unit be Triv in " ++ t2'
prettyTerm_T (LetT_T t1 ty b) = do
  t1' <- prettyTerm_T t1
  ty' <- prettyType_T ty
  (x, b') <- unbind b
  (y, t2) <- unbind b'
  let x' = n2s x
  let y' = n2s y
  t2' <- prettyTerm_T t2
  return $ "let " ++ x' ++ " (x) " ++ y' ++ " : " ++ ty' ++ " be " ++ t1' ++ " in " ++ t2'
prettyTerm_T (Tens_T t1 t2) = do
  t1' <- prettyTerm_T t1
  t2' <- prettyTerm_T t2
  return $ "(tens " ++ t1' ++ ", " ++ t2' ++ ")"
prettyTerm_T (Lam_T ty b) = do
  ty' <- prettyType_T ty
  (x, t) <- unbind b
  t' <- prettyTerm_T t
  return $ "\\(" ++ (n2s x) ++ ":" ++ ty' ++ ")." ++ t'
prettyTerm_T (App_T t1 t2) = do
  t1' <- prettyTerm_T t1
  t2' <- prettyTerm_T t2
  case t1 of
    Triv_T      -> case t2 of 
                     Triv_T -> return $ t1' ++ " " ++ t2'
                     _      -> return $ t1' ++ " (" ++ t2' ++ ")"
    Var_T _     -> case t2 of
                     Triv_T  -> return $ t1' ++ " " ++ t2'
                     Var_T _ -> return $ t1' ++ " " ++ t2'
                     _       -> return $ t1' ++ " (" ++ t2' ++ ")"
    (App_T _ _) -> case t2 of
                     Triv_T -> return $ t1' ++ " " ++ t2'
                     _      -> return $ t1' ++ " (" ++ t2' ++ ")"
    _           -> case t2 of
                     Triv_T -> return $ "(" ++ t1' ++ ") " ++ t2'
                     _      -> return $ "(" ++ t1' ++ ") " ++ " (" ++ t2' ++ ")"

------------------------------------------------------------------------
-- Testing functions                                                  --
------------------------------------------------------------------------
testPretty_T parser pretty_T s = do
  let o = parse parser "" s in
    case o of
      Left e -> error $ show e
      Right r -> runFreshM (pretty_T r)

testPrettyType_T :: String -> String
testPrettyType_T = testPretty_T typeParser_T prettyType_T

testPrettyTerm_T :: String -> String
testPrettyTerm_T = testPretty_T termParser_T prettyTerm_T

runPrettyType_T :: Type_T -> String
runPrettyType_T = runFreshM.prettyType_T

runPrettyTerm_T :: Term_T -> String
runPrettyTerm_T = runFreshM.prettyTerm_T

