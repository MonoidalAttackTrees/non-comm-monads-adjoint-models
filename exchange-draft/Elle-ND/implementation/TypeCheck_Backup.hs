{-# LANGUAGE FlexibleContexts #-}

---------------------------------------------------------
-- Type checking the terms defined in Syntax.hs        --
---------------------------------------------------------

module TypeCheck_Backup where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax

-----------------------------------------------------------------------
-- Typing contexts for T terms                                       --
-----------------------------------------------------------------------
type Ctx_T = [(TmName_T, Type_T)]

emptyCtx_T :: Ctx_T
emptyCtx_T = []

extCtx_T :: Ctx_T -> TmName_T -> Type_T -> Ctx_T
extCtx_T ctx nm ty = (nm, ty) : ctx

infixr 5 |:|
(|:|) :: (TmName_T, Type_T) -> Ctx_T -> Ctx_T
(x,t) |:| ctx = extCtx_T ctx x t

-----------------------------------------------------------------------
-- Typing contexts for S terms                                       --
-----------------------------------------------------------------------
type Ctx_S = [(TmName_S, Type_S)]

emptyCtx_S :: Ctx_S
emptyCtx_S = []

extCtx_S :: Ctx_S -> TmName_S -> Type_S -> Ctx_S
extCtx_S ctx nm ty = (nm, ty) : ctx

infixr 5 |::|
(|::|) :: (TmName_S, Type_S) -> Ctx_S -> Ctx_S
(x,t) |::| ctx = extCtx_S ctx x t

-----------------------------------------------------------------------
-- Free Variable Collection                                          --
-----------------------------------------------------------------------
-- The Fresh type class:
-- governs monads which can generate new globally unique Names based on a given Name
fv_T :: Fresh m => Term_T -> m [TmName_T]
fv_T Triv_T = return []
fv_T (Var_T x) = return [x]
fv_T (LetU_T t1 t2) = do
  t1' <- fv_T t1
  t2' <- fv_T t2
  return (t1' ++ t2')
-- unbind:
-- always generates a new globally fresh name
-- no matter what other names are or aren't in scope
-- (doesn't look pretty when printing)
fv_T (LetT_T t ty b) = do
  (tm, b') <- unbind b
  (tm', t') <- unbind b'
  t1 <- fv_T t
  t2 <- fv_T t'
  return (t1 ++ (t2 \\ [tm, tm']))
fv_T (Tens_T t1 t2) = do
  t1' <- fv_T t1
  t2' <- fv_T t2
  return (t1' ++ t2')
fv_T (Lam_T ty b) = do
  (tm, t) <- unbind b
  t' <- fv_T t
  return (t' \\ [tm])
fv_T (App_T t1 t2) = do
  t1' <- fv_T t1
  t2' <- fv_T t2
  return (t1' ++ t2')
fv_T (Gt_T s) = do
  s' <- fv_S s
  return $ s' >>= (\x -> [varS2varT x])



fv_S :: Fresh m => Term_S -> m [TmName_S]
fv_S Triv_S = return []
fv_S (Var_S x) = return [x]
fv_S (LetU_S s1 s2) = do
  s1' <- fv_S s1
  s2' <- fv_S s2
  return $ s1' ++ s2'
fv_S (LetT_S s ty b) = do
  (x1, b') <- unbind b
  (x2, s') <- unbind b'
  sfv <- fv_S s
  sfv' <- fv_S s'
  return $ sfv ++ (sfv' \\ [x1, x2])
fv_S (Tens_S s1 s2) = do
  s1' <- fv_S s1
  s2' <- fv_S s2
  return $ s1' ++ s2'
fv_S (LamL_S ty b) = do
  (x, s) <- unbind b
  s' <- fv_S s
  return $ s' \\ [x]
fv_S (LamR_S ty b) = do
  (x, s) <- unbind b
  s' <- fv_S s
  return $ s' \\ [x]
fv_S (AppL_S s1 s2) = do
  s1' <- fv_S s1
  s2' <- fv_S s2
  return $ s1' ++ s2'
fv_S (Ex_S s1 s2 b) = do
  (x1, b') <- unbind b
  (x2, s) <- unbind b'
  s1' <- fv_S s1
  s2' <- fv_S s2
  s' <- fv_S s
  return $ s1' ++ s2' ++ (s' \\ [x1, x2])
fv_S (Ft_S t) = do
  t' <- fv_T t
  return $ t' >>= (\x -> [varT2varS x])

-----------------------------------------------------------------------
-- Takes a list of free variables and a context,                     --
-- returns the subcontext that contains those free variables         --
-----------------------------------------------------------------------
subCtx_T :: [TmName_T] -> Ctx_T -> Maybe Ctx_T
subCtx_T _ [] = Just emptyCtx_T
-- (>>=) :: m a -> (a -> m b) -> m b
-- e.g. For the list monad, replace m by []
-- [1,2,3,4] >>= \ x -> [x - 1, x + 1]
-- we get [0,2,1,3,2,4,3,5]
subCtx_T (tm:tms) ctx = do
  ty <- lookup tm ctx
  (subCtx_T tms ctx) >>= (\ctx' -> return ((tm,ty):ctx'))

subCtx_S :: [TmName_S] -> Ctx_S -> Maybe Ctx_S
subCtx_S _ [] = Just emptyCtx_S
subCtx_S (tm:tms) ctx = do
  ty <- lookup tm ctx
  (subCtx_S tms ctx) >>= (\ctx' -> return ((tm,ty):ctx'))

-----------------------------------------------------------------------
-- Takes a context and a term                                        --
-- returns the subcontext that contains those free variables in term --
-----------------------------------------------------------------------
extractCtx_T :: Fresh m => Ctx_T -> Term_T -> ExceptT TypeException m Ctx_T
extractCtx_T ctx tm = do
  tm' <- fv_T tm
  let mctx' = subCtx_T tm' ctx in case mctx' of
    Just ctx' -> return ctx'
    Nothing -> throwError InvalidContextError

extractCtx_S :: Fresh m => Ctx_S -> Term_S -> ExceptT TypeException m Ctx_S
extractCtx_S ctx tm = do
  tm' <- fv_S tm
  let mctx' = subCtx_S tm' ctx in case mctx' of
    Just ctx' -> return ctx'
    Nothing -> throwError InvalidContextError

-- extractCtx :: Fresh m => Ctx_T -> Ctx_S -> Term_S -> ExceptT TypeException m 


-----------------------------------------------------------------------
-- Type checking                                                     --
-- Only for the non-linear part (T in Elle)                          --
-----------------------------------------------------------------------
typeCheck_T :: Fresh m => Ctx_T -> Term_T -> ExceptT TypeException m Type_T
typeCheck_T _ Triv_T = return Unit_T
typeCheck_T g (Var_T tm) = do
  ctx <- extractCtx_T g (Var_T tm)
  case (lookup tm ctx) of
    Nothing -> throwError VarException
    Just ty -> return ty
typeCheck_T g (LetU_T t1 t2) = do
  ctx1 <- extractCtx_T g t1
  ctx2 <- extractCtx_T g t2
  ty1 <- typeCheck_T ctx1 t1
  ty2 <- typeCheck_T ctx2 t2
  if (ty1 == Unit_T)
    then return ty2
    else throwError InvalidTypeError
typeCheck_T g (LetT_T t (Tensor_T ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, t') <- unbind b'
  ctx <- extractCtx_T g t
  ctx' <- extractCtx_T g t'
  ty <- typeCheck_T ctx t
  ty' <- typeCheck_T ((tm1, ty1) |:| (tm2, ty2) |:| ctx') t'
  case ty of
    (Tensor_T ty1' ty2') -> if ((ty1', ty2') == (ty1, ty2))
                            then return ty'
                            else throwError InvalidTypeError
    _ -> throwError InvalidTypeError

-- typeCheck_T g (LetG_T t (Gty_T ty) b) = ?

typeCheck_T g (Tens_T t1 t2) = do
  ctx1 <- extractCtx_T g t1
  ctx2 <- extractCtx_T g t2
  ty1 <- typeCheck_T ctx1 t1
  ty2 <- typeCheck_T ctx2 t2
  return (Tensor_T ty1 ty2)
typeCheck_T g (Lam_T ty b) = do
  (tm, t) <- unbind b
  ty' <- typeCheck_T ((tm, ty) |:| g) t
  return (Imp_T ty ty')
typeCheck_T g (App_T t1 t2) = do
  ctx1 <- extractCtx_T g t1
  ctx2 <- extractCtx_T g t2
  ty1 <- typeCheck_T ctx1 t1
  ty2 <- typeCheck_T ctx2 t2
  case ty1 of
    (Imp_T ty ty') -> if (ty2 == ty)
                        then return ty'
                        else throwError AppSrcError
    _ -> throwError AppSrcError
typeCheck_T g (Gt_T s) = do
  ty <- typeCheck_S g [] s
  return $ Gty_T ty

----------------------------------------
-- typeCheck ctx Triv_S = return Unit_S
-- typeCheck ctx (Var_S x) = 
----------------------------------------

typeCheck_S :: Fresh m => Ctx_T -> Ctx_S -> Term_S -> ExceptT TypeException m Type_S
typeCheck_S g d Triv_S = return Unit_S
typeCheck_S g d (Var_S x) = do
  d' <- extractCtx_S d (Var_S x)
  case (lookup x d') of
    Nothing -> throwError VarException
    Just ty -> return ty
typeCheck_S g d (LetU_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  ty1 <- typeCheck_S g ctx1 s1
  ty2 <- typeCheck_S g ctx2 s2
  case ty1 of
    Unit_S -> return ty2
    _      -> throwError InvalidTypeError
typeCheck_S g d (LetT_S s (Tensor_S ty1 ty2) b) = do
  (tm1, b') <- unbind b
  (tm2, s') <- unbind b'
  ctx <- extractCtx_S d s
  ctx' <- extractCtx_S d s'
  ty <- typeCheck_S g ctx s
  ty' <- typeCheck_S g ((tm1, ty1) |::| (tm2, ty2) |::| ctx') s'
  case ty of
    (Tensor_S ty1' ty2') -> if ((ty1', ty2') == (ty1, ty2))
                            then return ty'
                            else throwError InvalidTypeError
    _ -> throwError InvalidTypeError

-- typeCheck_S g d (LetF_S t (Fty_S ty) b) = ?

typeCheck_S g d (Tens_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  ty1 <- typeCheck_S g ctx1 s1
  ty2 <- typeCheck_S g ctx2 s2
  return (Tensor_S ty1 ty2)
typeCheck_S g d (LamL_S ty b) = do
  (tm, t) <- unbind b
  ty' <- typeCheck_S g ((tm, ty) |::| d) t
  return (ImpL_S ty ty')
typeCheck_S g d (LamR_S ty b) = do
  (tm, t) <- unbind b
  ty' <- typeCheck_S g ((tm, ty) |::| d) t
  return (ImpR_S ty ty')
typeCheck_S g d (AppL_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  ty1 <- typeCheck_S g ctx1 s1
  ty2 <- typeCheck_S g ctx2 s2
  case ty1 of
    (Imp_S ty ty') -> if (ty2 == ty)
                        then return ty'
                        else throwError AppSrcError
    _ -> throwError AppSrcError
typeCheck_S g d (AppR_S s1 s2) = do
  ctx1 <- extractCtx_S d s1
  ctx2 <- extractCtx_S d s2
  ty1 <- typeCheck_S g ctx1 s1
  ty2 <- typeCheck_S g ctx2 s2
  case ty1 of
    (Imp_S ty ty') -> if (ty2 == ty)
                        then return ty'
                        else throwError AppSrcError
    _ -> throwError AppSrcError
typeCheck_S g d (Ft_S t) = do
  ty <- typeCheck_T g t          -- Why isn't this giving an error?
  return $ Fty_S ty
typeCheck_S g d (Derelict_S (Gt_T s)) = do
  ctx <- extractCtx_S d s
  ty <- typeCheck_S g ctx s
  return ty









