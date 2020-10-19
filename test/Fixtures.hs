{-# LANGUAGE OverloadedStrings #-}

module Fixtures where

import Language.Haskell.LSP.Types
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Data.Text as T
import Lens.Micro

currentDirectory = "file://" ++ unsafePerformIO getCurrentDirectory

fooPath =  currentDirectory ++ "/src/main/scala/Foo.scala"

barPath =  currentDirectory ++ "/src/main/scala/Bar.scala"

modulePath =  currentDirectory ++ "/module/src/main/scala/Module.scala"

predefPath =  currentDirectory ++ "/whatever-deps-folder/scala/Predef.scala"

fooUri = Uri $ T.pack fooPath

barUri = Uri $ T.pack barPath

predefUri = Uri $ T.pack predefPath

moduleUri = Uri $ T.pack modulePath

fooRange = Range (Position 2 6) (Position 2 9)

fooLocation = Location fooUri fooRange

stringInPredefRange = Range (Position 2 0) (Position 2 0)

stringLocation = Location predefUri stringInPredefRange

printFooLocation = Location fooUri printFooRange

fooInBarLocation = Location barUri fooInBarRange

fooPosition = Position 2 6

printFooPosition = Position 5 15

printFooRange = Range (Position 5 12) (Position 5 15)

fooInBarPosition = Position 2 14

fooInBarRange = Range (Position 2 12) (Position 2 15)

stringPosition = Position 2 14

fooTextDocumentIdentifier = TextDocumentIdentifier fooUri

barTextDocumentIdentifier = TextDocumentIdentifier barUri

predefTextDocumentIdentifier = TextDocumentIdentifier predefUri

moduleInFooPosition = Position 7 26

moduleRange = Range (Position 1 6) (Position 1 12)

moduleLocation = Location moduleUri moduleRange

messageId = IdInt 1
