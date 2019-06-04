{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Arrow                            ( (+++)
                                                          , (|||)
                                                          )
import qualified Data.ByteString               as B
import           Data.Text.Encoding                       ( decodeUtf8 )
import           Data.Text.Prettyprint.Doc                ( pretty )
import           Data.Text.Prettyprint.Doc.Util           ( putDocW )
import           System.Environment                       ( getArgs )
import           Text.Megaparsec.Error                    ( errorBundlePretty )

import           Language.MiniML
import           Language.MiniML.Parser

main :: IO ()
main = getArgs >>= \case
  (fn : _) ->
    (   B.readFile fn
      >>= ( putDocW 80
          . (pretty ||| pretty)
          . (errorBundlePretty +++ eval)
          . parseMiniML fn
          . decodeUtf8
          )
      )
      *> putStrLn ""
  _ -> putStrLn "Please supply a filename.\n\nUsage: ./MiniML <filename.min>\n"
