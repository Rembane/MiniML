{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances,
   LambdaCase, TemplateHaskell #-}

-- | Inspiration: Tagless Final
-- | - http://okmij.org/ftp/tagless-final/course/lecture.pdf

module Language.MiniML.Expr
  ( Symantics(..)
  , BinOp(..)
  , ExprF(..)
  , Value(..)
  , Var(..)
  , P(..)
  , view
  )
where

import           Control.Monad
import           Data.Function                            ( on )
import           Data.Functor.Foldable                    ( Fix
                                                          , cata
                                                          )
import           Data.Text.Prettyprint.Doc                ( Doc
                                                          , Pretty
                                                          , pretty
                                                          )

class Symantics repr where
  val :: Int -> repr Int
--  var :: Var -> repr Var
  plus  :: repr Int -> repr Int -> repr Int
  minus :: repr Int -> repr Int -> repr Int
  multi :: repr Int -> repr Int -> repr Int
  divis :: repr Int -> repr Int -> repr Int

  lam :: (Var, repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

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

data BinOp = BPlus | BMinus | BMulti | BDiv
  deriving (Show)

-- | For prettyprinting.
newtype P a = P { unP :: String }

instance Symantics P where
  val = P . show
  plus  e1 e2 = P $ unP e1 <> " + " <> unP e2
  minus e1 e2 = P $ unP e1 <> " - " <> unP e2
  multi e1 e2 = P $ unP e1 <> " * " <> unP e2
  divis e1 e2 = P $ unP e1 <> " / " <> unP e2

  lam (v, e) = P $ "(λ" <> unVar v <> ". " <> unP (e $ P $ unVar v) <> ")"
  app e1 e2 = P $ "(" <> unP e1 <> " " <> unP e2 <> ")"

view :: P a -> String
view = unP

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
  pretty = pretty . \case
      BPlus  -> " + "
      BMinus -> " - "
      BMulti -> " * "
      BDiv   -> " / "
