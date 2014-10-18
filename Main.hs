module Main where

import Parser


main = getContents >>= print . Parser.calc . Parser.lexer