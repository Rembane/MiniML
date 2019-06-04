{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances,
   LambdaCase, TemplateHaskell #-}

module Language.MiniML.Expr
  ( BinOp(..)
  , ExprF(..)
  , Value(..)
  , Var(..)
  )
where

import           Data.Functor.Foldable                    ( Fix
                                                          , cata
                                                          )
import           Data.Text.Prettyprint.Doc                ( Pretty
                                                          , pretty
                                                          )
import           Text.Show.Deriving

newtype Var
  = Var { unVar :: String }
  deriving (Eq, Show)

data ExprF r
  = VarF Var
  | AbsF Var r
  | AppF r r
  | ValF Int
  | BinOpF BinOp r r
  deriving (Functor, Show, Foldable, Traversable)

data BinOp = BPlus
  deriving (Show)

deriveShow1 ''ExprF

instance Pretty (Fix ExprF) where
  pretty = cata alg
    where
      alg = \case
        VarF v          -> pretty (unVar v)
        AbsF v e        -> pretty 'λ' <> pretty (unVar v) <> pretty ". " <> e
        AppF e1 e2      -> e1 <> pretty ' ' <> e2
        ValF n          -> pretty n
        BinOpF op e1 e2 -> pretty "(" <> e1 <> pretty op <> e2 <> pretty ")"

newtype Value
  = VClosure { unValue :: Fix ExprF }

instance Pretty Value where
  pretty = pretty . unValue

instance Pretty BinOp where
  pretty BPlus = pretty " + "
