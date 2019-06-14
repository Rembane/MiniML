module Language.MiniML.Parser where

import           Control.Applicative                      ( liftA2 )
import           Control.Monad.Combinators.Expr           ( Operator(..)
                                                          , makeExprParser
                                                          )
import           Data.Foldable                            ( foldl' )
import           Data.Functor                             ( (<&>) )
import           Data.Functor.Foldable                    ( Fix(Fix) )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Void                                ( Void )
import           Text.Megaparsec                          ( (<|>)
                                                          , (<?>)
                                                          , Parsec
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

dot = (.) . (.)

type Parser = Parsec Void Text

-- | Filename -> Content of file -> Result
parseMiniML :: String -> Text -> Either (ParseErrorBundle Text Void) (Fix ExprF)
parseMiniML = parse minimlParser

minimlParser :: Parser (Fix ExprF)
minimlParser = between sc eof exprParser

sc :: Parser ()
sc = L.space space1
             (L.skipLineComment (T.pack "--"))
             (L.skipBlockComment (T.pack "{-") (T.pack "-}"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser Text
symbol = L.symbol sc . T.pack

parens :: Show a => Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

exprParser :: Parser (Fix ExprF)
exprParser =
  (makeExprParser noOperatorParsers operatorTable <?> "operators")
    <|> noOperatorParsers
 where
  noOperatorParsers =
    choice [absParser, appParser, valParser, fixVarParser, parens exprParser]

-- | Variables start with a-z or _ and is then followed by alphanumeric characters, _ and '
varParser :: Parser Var
varParser = Var <$> (lexeme . try)
  (liftA2 (:) (char '_' <|> lowerChar) (many (char '\'' <|> alphaNumChar)))

-- | Lift a Var into Fix ExprF
fixVarParser :: Parser (Fix ExprF)
fixVarParser = Fix . VarF <$> varParser

-- | Parse a lambda abstraction
-- | Syntax: \var -> expr
absParser :: Parser (Fix ExprF)
absParser =
  symbol "\\" *> liftA2 (dot Fix AbsF) varParser (symbol "->" *> exprParser)

-- | Parse an expression application
-- | Syntax: e1 e2 [e3 ... e_n]
appParser :: Parser (Fix ExprF)
appParser = try $ liftA2
  (foldl' (dot Fix AppF))
  (let p = absParser <|> fixVarParser in p <|> parens p)
  (some exprParser)

-- | Parse a value
-- | Syntax: Optional minus sign, integer literal.
valParser :: Parser (Fix ExprF)
valParser = optional (symbol "-")
  >>= \f -> lexeme L.decimal <&> (Fix . ValF . maybe id (const negate) f)

-- | Parse an infix operator1
-- | Syntax: e1 OP e2
operatorTable :: [[Operator Parser (Fix ExprF)]]
operatorTable =
  [ [ InfixL (dot Fix (BinOpF BMulti) <$ symbol "*")
    , InfixL (dot Fix (BinOpF BDiv) <$ symbol "/")
    ]
  , [ InfixL (dot Fix (BinOpF BPlus) <$ symbol "+")
    , InfixL (dot Fix (BinOpF BMinus) <$ symbol "-")
    ]
  ]
