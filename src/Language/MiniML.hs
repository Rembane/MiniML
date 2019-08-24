{-# LANGUAGE LambdaCase #-}
module Language.MiniML where

import           Data.Functor.Foldable                    ( Fix(Fix)
                                                          , cata
                                                          , unfix
                                                          )

import           Language.MiniML.Expr

eval :: Fix ExprF -> Value
eval = cata alg
 where
  alg :: ExprF Value -> Value
  alg = VClosure . Fix . \case
    VarF v                -> VarF v
    AbsF v             e  -> AbsF v $ unValue e
    AppF (VClosure e1) v2 -> case unfix e1 of
      AbsF v e -> unfix $ unValue $ eval $ cata (subAlg v e) $ unValue v2
      _        -> AppF e1 (unValue v2)
    ValF i -> ValF i
    BinOpF op (VClosure e1) (VClosure e2) -> case (unfix e1, unfix e2) of
      (ValF i1, ValF i2) -> ValF $ (opToFun op) i1 i2
      (_      , _      ) -> BinOpF op e1 e2

  subAlg :: Var -> Fix ExprF -> ExprF (Fix ExprF) -> Fix ExprF
  subAlg v withThat (VarF v') | v' == v   = withThat
                              | otherwise = Fix $ VarF v'
  subAlg _ _ e = Fix e

  opToFun = \case
    BPlus  -> (+)
    BMinus -> subtract
    BMulti -> (*)
    BDiv   -> div
