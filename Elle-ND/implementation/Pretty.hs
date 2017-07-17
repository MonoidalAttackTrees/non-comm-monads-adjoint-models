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
prettyType :: Fresh m => Type -> m String
prettyType Unit = return "Unit"
prettyType (Tensor a b) = do
  a' <- prettyType a
  b' <- prettyType b
  return (a' ++ "(x)" ++ b')
prettyType (Imp a b) = do
  a' <- prettyType a
  b' <- prettyType b
  return ("(" ++ a' ++ ")" ++ "-o" ++ b')

------------------------------------------------------------------------
-- Converts a term into a string                                      --
------------------------------------------------------------------------
prettyTerm :: Fresh m => Term -> m String
prettyTerm Triv = return "Triv"
prettyTerm (Var x) = return.n2s $ x
prettyTerm (LetU t1 t2) = do
  t1' <- prettyTerm t1
  t2' <- prettyTerm t2
  return $ "let " ++ t1' ++ " : Unit be Triv in " ++ t2'
prettyTerm (LetT t1 ty b) = do
  t1' <- prettyTerm t1
  ty' <- prettyType ty
  (x, b') <- unbind b
  (y, t2) <- unbind b'
  let x' = n2s x
  let y' = n2s y
  t2' <- prettyTerm t2
  return $ "let " ++ x' ++ " (x) " ++ y' ++ " : " ++ ty' ++ " be " ++ t1' ++ " in " ++ t2'
prettyTerm (Tens t1 t2) = do
  t1' <- prettyTerm t1
  t2' <- prettyTerm t2
  return $ "(tens " ++ t1' ++ ", " ++ t2' ++ ")"
prettyTerm (Lam ty b) = do
  ty' <- prettyType ty
  (x, t) <- unbind b
  t' <- prettyTerm t
  return $ "\\(" ++ (n2s x) ++ ":" ++ ty' ++ ")." ++ t'
prettyTerm (App t1 t2) = do
  t1' <- prettyTerm t1
  t2' <- prettyTerm t2
  case t1 of
    Triv      -> case t2 of 
                    Triv -> return $ t1' ++ " " ++ t2'
                    _    -> return $ t1' ++ " (" ++ t2' ++ ")"
    Var _     -> case t2 of
                   Triv  -> return $ t1' ++ " " ++ t2'
                   Var _ -> return $ t1' ++ " " ++ t2'
                   _     -> return $ t1' ++ " (" ++ t2' ++ ")"
    (App _ _) -> case t2 of
                   Triv -> return $ t1' ++ " " ++ t2'
                   _    -> return $ t1' ++ " (" ++ t2' ++ ")"
    _         -> case t2 of
                   Triv -> return $ "(" ++ t1' ++ ") " ++ t2'
                   _    -> return $ "(" ++ t1' ++ ") " ++ " (" ++ t2' ++ ")"

------------------------------------------------------------------------
-- Testing functions                                                  --
------------------------------------------------------------------------
testPretty parser pretty s = do
  let o = parse parser "" s in
    case o of
      Left e -> error $ show e
      Right r -> runFreshM (pretty r)

testPrettyType :: String -> String
testPrettyType = testPretty typeParser prettyType

testPrettyTerm :: String -> String
testPrettyTerm = testPretty termParser prettyTerm

runPrettyType :: Type -> String
runPrettyType = runFreshM.prettyType

runPrettyTerm :: Term -> String
runPrettyTerm = runFreshM.prettyTerm

