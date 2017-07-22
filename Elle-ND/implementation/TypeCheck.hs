{-# LANGUAGE FlexibleContexts #-}

---------------------------------------------------------
-- Type checking the terms defined in Syntax.hs        --
---------------------------------------------------------

module TypeCheck where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax
import Parser

-----------------------------------------------------------------------
-- (T) Typing contexts                                               --
-----------------------------------------------------------------------
type Ctx_T = [(TmName_T, Type_T)]

emptyCtx_T :: Ctx_T
emptyCtx_T = []

extCtx_T :: Ctx_T -> TmName_T -> Type_T -> Ctx_T
extCtx_T ctx x ty = (x, ty) : ctx

infixr 5 |:|
(|:|) :: (TmName_T, Type_T) -> Ctx_T -> Ctx_T
(x,t) |:| ctx = extCtx_T ctx x t

-----------------------------------------------------------------------
-- (S) Typing contexts                                               --
-----------------------------------------------------------------------
type Ctx_S = [(TmName_S, Type_S)]

emptyCtx_S :: Ctx_S
emptyCtx_S = []

extCtx_S :: Ctx_S -> TmName_S -> Type_S -> Ctx_S
extCtx_S ctx x ty = (x, ty) : ctx

infixr 5 |::|
(|::|) :: (TmName_S, Type_S) -> Ctx_S -> Ctx_S
(x,t) |::| ctx = extCtx_S ctx x t

-----------------------------------------------------------------------
-- Typing contexts                                               --
-----------------------------------------------------------------------
type Ctx = (Ctx_T, Ctx_S)

emptyCtx :: Ctx
emptyCtx = ([], [])

extCtx1 :: Ctx -> TmName_T -> Type_T -> Ctx
extCtx1 (ctxT, ctxS) x ty = ((x, ty) : ctxT, ctxS)

infix 5 |+|
(|+|) :: (TmName_T, Type_T) -> Ctx -> Ctx
(x, t) |+| ctx = extCtx1 ctx x t

extCtx2 :: Ctx -> TmName_S -> Type_S -> Ctx
extCtx2 (ctxT, ctxS) x ty = (ctxT, (x, ty) : ctxS)

infix 5 |++|
(|++|) :: (TmName_S, Type_S) -> Ctx -> Ctx
(x, t) |++| ctx = extCtx2 ctx x t

-----------------------------------------------------------------------
-- (T) Free Variable Collection                                      --
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
  (fvt, fvs) <- fv s
  return $ fvs >>= (\x -> [varS2varT x])

-----------------------------------------------------------------------
-- (S) Free Variable Collection                                      --
-----------------------------------------------------------------------
fv :: Fresh m => Term_S -> m ([TmName_T], [TmName_S])
fv Triv_S = return ([], [])
fv (Var_S x) = return ([], [x])
fv (LetU_S s1 s2) = do
  (t1, s1') <- fv s1
  (t2, s2') <- fv s2
  return (t1 ++ t2, s1' ++ s2')
fv (LetT_S s ty b) = do
  (x, b') <- unbind b
  (y, s') <- unbind b'
  (fvt, fvs) <- fv s
  (fvt', fvs') <- fv s'
  return (fvt ++ fvt', fvs ++ (fvs' \\ [x, y]))
fv (Tens_S s1 s2) = do
  (fv1, fv1') <- fv s1
  (fv2, fv2') <- fv s2
  return (fv1 ++ fv2, fv1' ++ fv2')
fv (LamL_S ty b) = do
  (x, s) <- unbind b
  (fvt, fvs) <- fv s
  return (fvt, fvs \\ [x])
fv (LamR_S ty b) = do
  (x, s) <- unbind b
  (fvt, fvs) <- fv s
  return (fvt, fvs \\ [x])
fv (AppL_S s1 s2) = do
  (fv1, fv1') <- fv s1
  (fv2, fv2') <- fv s2
  return (fv1 ++ fv2, fv1' ++ fv2')
fv (AppR_S s1 s2) = do
  (fv1, fv1') <- fv s1
  (fv2, fv2') <- fv s2
  return (fv1 ++ fv2, fv1' ++ fv2')
fv (Ft_S t) = do
  fvt <- fv_T t
  return (fvt, [])

-----------------------------------------------------------------------
-- (T) Takes a list of T free variables and a T context,             --
-- returns the subcontext that contains those free variables         --
-----------------------------------------------------------------------
subCtx_T :: [TmName_T] -> Ctx_T -> Maybe Ctx_T
subCtx_T _ [] = Just emptyCtx_T
-- (>>=) :: m a -> (a -> m b) -> m b
-- e.g. For the list monad, replace m by []
-- [1,2,3,4] >>= \ x -> [x - 1, x + 1]
-- we get [0,2,1,3,2,4,3,5]
subCtx_T (x:xs) ctx = do
  ty <- lookup x ctx
  (subCtx_T xs ctx) >>= (\ctx' -> return ((x,ty):ctx'))

-----------------------------------------------------------------------
-- (S) Takes a list of S free variables and a S context,             --
-- returns the subcontext that contains those free variables         --
-----------------------------------------------------------------------
subCtx_S :: [TmName_S] -> Ctx_S -> Maybe Ctx_S
subCtx_S _ [] = Just emptyCtx_S
subCtx_S (x:xs) ctx = do
  ty <- lookup x ctx
  (subCtx_S xs ctx) >>= (\ctx' -> return ((x,ty):ctx'))

-----------------------------------------------------------------------
-- Takes a pair of lists of T and S free variables and a context,    --
-- returns the subcontext that contains those free variables         --
-----------------------------------------------------------------------
subCtx :: ([TmName_T], [TmName_S]) -> Ctx -> Maybe Ctx
subCtx _ ([], []) = Just emptyCtx
subCtx (_, s) ([], ctx) = do
  ctx' <- subCtx_S s ctx
  return ([], ctx')
subCtx (t, _) (ctx, []) = do
  ctx' <- subCtx_T t ctx
  return (ctx', [])
subCtx (t, s) (ctxt, ctxs) = do
  ctxt' <- subCtx_T t ctxt
  ctxs' <- subCtx_S s ctxs
  return (ctxt', ctxs')

-----------------------------------------------------------------------
-- (T) Takes a context and a term                                    --
-- returns the subcontext that contains those free variables in term --
-----------------------------------------------------------------------
extractCtx_T :: Fresh m => Ctx_T -> Term_T -> ExceptT TypeException m Ctx_T
extractCtx_T ctx tm = do
  tm' <- fv_T tm
  let mctx' = subCtx_T tm' ctx in case mctx' of
    Just ctx' -> return ctx'
    Nothing -> throwError InvalidContextError

-----------------------------------------------------------------------
-- (T) Takes a context and a term                                    --
-- returns the subcontext that contains those free variables in term --
-----------------------------------------------------------------------
extractCtx :: Fresh m => Ctx -> Term_S -> ExceptT TypeException m Ctx
extractCtx ctx s = do
  s' <- fv s
  let mctx' = subCtx s' ctx in case mctx' of
    Just ctx' -> return ctx'
    Nothing -> throwError InvalidContextError

-----------------------------------------------------------------------
-- (T) Type checking                                                 --
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
  ty <- typeCheck_S (g, []) s
  return $ Gty_T ty

-----------------------------------------------------------------------
-- (S) Type checking                                                 --
-----------------------------------------------------------------------
typeCheck_S :: Fresh m => Ctx -> Term_S -> ExceptT TypeException m Type_S
typeCheck_S _ Triv_S = return Unit_S
typeCheck_S (g, p) (Var_S x) = do
  (g', p') <- extractCtx (g, p) (Var_S x)
  case (lookup x p') of
    Nothing -> throwError VarException
    Just ty -> return ty
typeCheck_S (g, p) (LetU_S s1 s2) = do
  ctx1 <- extractCtx (g, p) s1
  ctx2 <- extractCtx (g, p) s2
  ty1 <- typeCheck_S ctx1 s1
  ty2 <- typeCheck_S ctx2 s2
  if (ty1 == Unit_S)
    then return ty2
    else throwError InvalidTypeError
typeCheck_S (g, p) (LetT_S s (Tensor_S ty1 ty2) b) = do
  (x, b') <- unbind b
  (y, s') <- unbind b'
  ctx <- extractCtx (g, p) s
  ctx' <- extractCtx (g, p) s'
  ty <- typeCheck_S ctx s
  ty' <- typeCheck_S ctx' s'
  case ty of
    (Tensor_S ty1' ty2') -> if ((ty1', ty2') == (ty1, ty2))
                              then return ty'
                              else throwError InvalidTypeError
    _ -> throwError InvalidTypeError
typeCheck_S (g, p) (Tens_S s1 s2) = do
  ctx1 <- extractCtx (g, p) s1
  ctx2 <- extractCtx (g, p) s2
  ty1 <- typeCheck_S ctx1 s1
  ty2 <- typeCheck_S ctx2 s2
  return (Tensor_S ty1 ty2)
typeCheck_S (g, p) (LamL_S ty b) = do
  (x, s) <- unbind b
  ty' <- typeCheck_S ((x, ty) |++| (g, p)) s
  return (ImpL_S ty ty')
typeCheck_S (g, p) (LamR_S ty b) = do
  (x, s) <- unbind b
  ty' <- typeCheck_S ((x, ty) |++| (g, p)) s
  return (ImpR_S ty ty')
typeCheck_S (g, p) (AppL_S s1 s2) = do
  ctx1 <- extractCtx (g, p) s1
  ctx2 <- extractCtx (g, p) s2
  ty1 <- typeCheck_S ctx1 s1
  ty2 <- typeCheck_S ctx2 s2
  case ty1 of
    (ImpL_S ty ty') -> if (ty2 == ty)
                         then return ty'
                         else throwError AppSrcError
    _ -> throwError AppSrcError
typeCheck_S (g, p) (AppR_S s1 s2) = do
  ctx1 <- extractCtx (g, p) s1
  ctx2 <- extractCtx (g, p) s2
  ty1 <- typeCheck_S ctx1 s1
  ty2 <- typeCheck_S ctx2 s2
  case ty1 of
    (ImpR_S ty ty') -> if (ty2 == ty)
                         then return ty'
                         else throwError AppSrcError
    _ -> throwError AppSrcError
typeCheck_S (g, p) (LetF_S s (Fty_S ty) b) = do
  (x, s') <- unbind b
  ctx <- extractCtx (g, p) s
  tys <- typeCheck_S ctx s
  ctx' <- extractCtx (g, p) s'
  ty' <- typeCheck_S ((varS2varT x, ty) |+| ctx') s'
  case tys of
    (Fty_S tys') -> return ty'
    _            -> throwError InvalidTypeError
typeCheck_S (g, p) (LetG_S t (Gty_T ty) b) = do
  (x, s') <- unbind b
  ctx <- extractCtx_T g t
  tyt <- typeCheck_T ctx t
  ctx' <- extractCtx (g, p) s'
  ty' <- typeCheck_S ((x, ty) |++| ctx') s'
  case tyt of
    (Gty_T tyt') -> return ty'
    _            -> throwError InvalidTypeError
typeCheck_S (g, p) (Ft_S t) = do
  ctx <- extractCtx_T g t
  ty <- typeCheck_T ctx t
  return $ Fty_S ty
typeCheck_S (g, p) (Ex_S t1 t2 b) = do
  (x, b') <- unbind b
  (y, s) <- unbind b'
  g1 <- extractCtx_T g t1
  g2 <- extractCtx_T g t2
  ty1 <- typeCheck_T g1 t1
  ty2 <- typeCheck_T g2 t2
  ctx <- extractCtx (g, p) s
  ty <- typeCheck_S ctx s
  case (lookup (varS2varT x) g) of
    ty2 -> case (lookup (varS2varT y) g) of
             ty1 -> return ty




  








