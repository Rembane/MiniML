{-# LANGUAGE RankNTypes #-}
module Language.MiniML.Parser where

import           Control.Applicative                      ( liftA2 )
import           Control.Monad.Combinators.Expr           ( Operator(..)
                                                          , makeExprParser
                                                          )
import           Control.Monad.State.Strict               ( State )
import           Data.Foldable                            ( foldl' )
import           Data.Functor                             ( (<&>) )
import           Data.Functor.Foldable                    ( Fix(Fix) )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Data.Void                                ( Void )
import           Prelude                           hiding ( abs )
import           Text.Megaparsec                          ( (<|>)
                                                          , (<?>)
                                                          , Parsec
                                                          , ParsecT
                                                          , ParseErrorBundle
                                                          , between
                                                          , choice
                                                          , eof
                                                          , many
                                                          , optional
                                                          , parse
                                                          , some
                                                          , try
                                                          )
import           Text.Megaparsec.Char                     ( alphaNumChar
                                                          , char
                                                          , lowerChar
                                                          , space1
                                                          )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.MiniML.Expr

-- | The code in this file is heavily inspired by:
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
-- and https://markkarpov.com/megaparsec/megaparsec.html

type Parser = Parsec Void Text

-- | Filename -> Content of file -> Result
parseMiniML
  :: Symantics repr
  => String
  -> Text
  -> Either (ParseErrorBundle Text Void) (Parser (repr (a -> b)))
parseMiniML = parse minimlParser

minimlParser :: Symantics repr => Parser (repr (a -> b))
minimlParser = between sc eof exprParser

sc :: Parser ()
sc = L.space space1
             (L.skipLineComment (T.pack "--"))
             (L.skipBlockComment (T.pack "{-") (T.pack "-}"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser Text
symbol = L.symbol sc . T.pack

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

exprParser :: Symantics repr => Parser (repr Int)
exprParser =
  (makeExprParser noOperatorParsers (operatorTable) <?> "operators")
    <|> noOperatorParsers
  where noOperatorParsers = choice [appParser, valParser, parens exprParser]

-- | Variables start with a-z or _ and is then followed by alphanumeric characters, _ and '
varParser :: Parser Var
varParser = Var <$> (lexeme . try)
  (liftA2 (:) (char '_' <|> lowerChar) (many (char '\'' <|> alphaNumChar)))

-- | Parse a lambda abstraction
-- | Syntax: \var -> expr
--  lam :: (Var, repr a -> repr b) -> repr (a -> b)
lamParser :: Symantics repr => Parser (repr (a -> b))
lamParser = symbol "\\" *> liftA2 (\v e -> lam (v, \x -> (app e) x))
                                  varParser
                                  (symbol "->" *> exprParser)

-- | Parse an expression application
-- | Syntax: e1 e2 [e3 ... e_n]
-- app :: repr (a -> b) -> repr a -> repr b
appParser :: Symantics repr => Parser (repr Int)
appParser = undefined {- try $ liftA2
  (\f es -> foldl' (\acc x -> _) _ es)
  (let p = lamParser <|> ((\v -> \e -> lam (v, \x -> e)) <$> varParser)
   in  p <|> parens p
  )
  (some exprParser)
  -}

-- | Parse a value
-- | Syntax: Optional minus sign, integer literal.
valParser :: (Symantics repr) => Parser (repr Int)
valParser = undefined {- optional (symbol "-")
  >>= \f -> lexeme L.decimal <&> (const (val . maybe id (const negate) f))
  -}

-- | Parse an infix operator1
-- | Syntax: e1 OP e2
operatorTable :: Symantics repr => [[Operator Parser (repr Int)]]
operatorTable =
  [ [InfixL (multi <$ symbol "*"), InfixL (divis <$ symbol "/")]
  , [InfixL (plus <$ symbol "+"), InfixL (minus <$ symbol "-")]
  ]
