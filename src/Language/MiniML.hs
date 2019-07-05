{-# LANGUAGE LambdaCase #-}
module Language.MiniML where

import           Data.Functor.Foldable                    ( Fix(Fix)
                                                          , cata
                                                          , unfix
                                                          )

import           Language.MiniML.Expr

-- | For evaluation
newtype E a = E { unE :: a }

instance Symantics E where
  val = E
  plus  (E e1) (E e2) = E $ e1 + e2

  minus (E e1) (E e2) = E $ e1 - e2
  multi (E e1) (E e2) = E $ e1 * e2
  divis (E e1) (E e2) = E $ e1 `div` e2

  lam (_, f) = E $ unE . f . E
  app e1 e2  = E $ (unE e1) (unE e2)

eval' :: E a -> a
eval' = unE

-- | Substitute a variable with an expression
substitute :: Var -> Fix ExprF -> Fix ExprF -> Fix ExprF
substitute v replaceInThis withThat = cata alg replaceInThis
 where
  alg :: ExprF (Fix ExprF) -> Fix ExprF
  alg (VarF v') | v' == v   = withThat
                | otherwise = Fix $ VarF v'
  alg e = Fix e

eval :: Fix ExprF -> Value
eval = cata alg
 where
  alg :: ExprF Value -> Value
  alg = \case
    VarF v                -> VClosure $ Fix $ VarF v
    AbsF v             e  -> VClosure $ Fix $ AbsF v $ unValue e
    AppF (VClosure e1) v2 -> case unfix e1 of
      AbsF v e -> eval $ substitute v e $ unValue v2
      _        -> VClosure $ Fix $ AppF e1 (unValue v2)
    ValF i -> VClosure $ Fix $ ValF i
    BinOpF op (VClosure e1) (VClosure e2) -> case (unfix e1, unfix e2) of
      (ValF i1, ValF i2) -> VClosure $ Fix $ ValF $ (opToFun op) i1 i2
      (_      , _      ) -> VClosure $ Fix $ BinOpF op e1 e2

  opToFun = \case
    BPlus  -> (+)
    BMinus -> subtract
    BMulti -> (*)
    BDiv   -> div
