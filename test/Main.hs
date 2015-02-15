{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs #-}
module Main where

import Entity
import Entity.TH

import Language.Haskell.TH

data Foo = Foo { a :: String
               , b :: String
               }



$(deriveStoreable ''Foo)

main :: IO ()
main = do
    putStrLn "hello"
