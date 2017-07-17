{-# LANGUAGE FlexibleContexts #-}

---------------------------------------------------------
-- Type checking the terms defined in Syntax.hs        --
---------------------------------------------------------

module TypeCheck where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax

-----------------------------------------------------------------------
-- Typing contexts                                                   --
-----------------------------------------------------------------------
type Ctx = [(TmName, Type)]

emptyCtx :: Ctx
emptyCtx = []

extCtx :: Ctx -> TmName -> Type -> Ctx
extCtx ctx nm ty = (nm, ty) : ctx

infixr 5 |:|
(|:|) :: (TmName, Type) -> Ctx -> Ctx
(x,t) |:| ctx = extCtx ctx x t

-----------------------------------------------------------------------
-- Free Variable Collection                                          --
-----------------------------------------------------------------------
-- The Fresh type class:
-- governs monads which can generate new globally unique Names based on a given Name
fv :: Fresh m => Term -> m [TmName]
fv Triv = return []
fv (Var t) = return [t]
fv (LetU t1 t2) = do
  t1' <- fv t1
  t2' <- fv t2
  return (t1' ++ t2')
-- unbind:
-- always generates a new globally fresh name
-- no matter what other names are or aren't in scope
-- (doesn't look pretty when printing)
fv (LetT t ty b) = do
  (tm, b') <- unbind b
  (tm', t') <- unbind b'
  t1 <- fv t
  t2 <- fv t'
  return (t1 ++ (t2 \\ [tm, tm']))
fv (Tens t1 t2) = do
  t1' <- fv t1
  t2' <- fv t2
  return (t1' ++ t2')
fv (Lam ty b) = do
  (tm, t) <- unbind b
  t' <- fv t
  return (t' \\ [tm])
fv (App t1 t2) = do
  t1' <- fv t1
  t2' <- fv t2
  return (t1' ++ t2')

-----------------------------------------------------------------------
-- Takes a list of free variables and a context,                     --
-- returns the subcontext that contains those free variables         --
-----------------------------------------------------------------------
subCtx :: [TmName] -> Ctx -> Maybe Ctx
subCtx _ [] = Just emptyCtx
-- (>>=) :: m a -> (a -> m b) -> m b
-- e.g. For the list monad, replace m by []
-- [1,2,3,4] >>= \ x -> [x - 1, x + 1]
-- we get [0,2,1,3,2,4,3,5]
subCtx (tm:tms) ctx = do
  ty <- lookup tm ctx
  (subCtx tms ctx) >>= (\ctx' -> return ((tm,ty):ctx'))

-----------------------------------------------------------------------
-- Takes a context and a term                                        --
-- returns the subcontext that contains those free variables in term --
-----------------------------------------------------------------------
extractCtx :: Fresh m => Ctx -> Term -> ExceptT TypeException m Ctx
extractCtx ctx tm = do
  tm' <- fv tm
  let mctx' = subCtx tm' ctx in case mctx' of
    Just ctx' -> return ctx'
    Nothing -> throwError InvalidContextError

-----------------------------------------------------------------------
-- Type checking                                                     --
-- Only for the non-linear part (T in Elle)                          --
-----------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Term -> ExceptT TypeException m Type
typeCheck _ Triv = return Unit
typeCheck g (Var tm) = do
  ctx <- extractCtx g (Var tm)
  case (lookup tm ctx) of
    Nothing -> throwError VarException
    Just ty -> return ty
typeCheck g (LetU t1 t2) = do
  ctx1 <- extractCtx g t1
  ctx2 <- extractCtx g t2
  ty1 <- typeCheck ctx1 t1
  ty2 <- typeCheck ctx2 t2
  if (ty1 == Unit)
    then return ty2
    else throwError InvalidTypeError
typeCheck g (LetT t (Tensor ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, t') <- unbind b'
  ctx <- extractCtx g t
  ctx' <- extractCtx g t'
  ty <- typeCheck ctx t
  ty' <- typeCheck ((tm1, ty1) |:| (tm2, ty2) |:| ctx') t'
  case ty of
    (Tensor ty1' ty2') -> if ((ty1', ty2') == (ty1, ty2))
                          then return ty'
                          else throwError InvalidTypeError
    _ -> throwError InvalidTypeError
typeCheck g (Tens t1 t2) = do
  ctx1 <- extractCtx g t1
  ctx2 <- extractCtx g t2
  ty1 <- typeCheck ctx1 t1
  ty2 <- typeCheck ctx2 t2
  return (Tensor ty1 ty2)
typeCheck g (Lam ty b) = do
  (tm, t) <- unbind b
  ty' <- typeCheck ((tm, ty) |:| g) t
  return (Imp ty ty')
typeCheck g (App t1 t2) = do
  ctx1 <- extractCtx g t1
  ctx2 <- extractCtx g t2
  ty1 <- typeCheck ctx1 t1
  ty2 <- typeCheck ctx2 t2
  case ty1 of
    (Imp ty ty') -> if (ty2 == ty)
                    then return ty'
                    else throwError AppSrcError
    _ -> throwError AppSrcError

  













