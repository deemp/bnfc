{-
    BNF Converter: Main file
    Copyright (C) 2002-2013  Authors:
    Jonas Almström Duregård, Krasimir Angelov, Björn Bringert, Johan Broberg, Paul Callaghan,
    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson,
    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer and Aarne Ranta 2002 - 2013.

    Björn Bringert, Johan Broberg, Markus Forsberg, Peter Gammie,
    Patrik Jansson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer, Aarne Ranta

-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BNFC.Backend.Base
import BNFC.Backend.C
import BNFC.Backend.CPP.NoSTL
import BNFC.Backend.CPP.STL
import BNFC.Backend.Haskell
import BNFC.Backend.HaskellGADT
import BNFC.Backend.Java
import BNFC.Backend.Latex
import BNFC.Backend.OCaml
import BNFC.Backend.Pygments
import BNFC.Backend.TreeSitter
import BNFC.CF (CF)
import BNFC.GetCF
import BNFC.Options hiding (make, Backend)

import BNFC.License ( license )
import Paths_BNFC ( version )

import Control.Exception (Handler (..), SomeException, catches, displayException)

import Data.Version ( showVersion )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (stderr, hPutStrLn)
import Main.Utf8 (withUtf8)
import System.IO.CodePage (withCP65001)

withCorrectLocale :: IO a -> IO a
withCorrectLocale act = do
  let withCorrectLocale' = withCP65001 . withUtf8
  withCorrectLocale' act
    `catches` [ Handler $ \(x :: ExitCode) -> exitWith x
              , Handler $ \(x :: SomeException) ->
                  withCorrectLocale' do
                    putStrLn (displayException x)
                    exitFailure
              ]

-- Print an error message and a (short) usage help and exit
printUsageErrors :: [String] -> IO ()
printUsageErrors msg = do
  mapM_ (hPutStrLn stderr) msg
  hPutStrLn stderr usage
  exitFailure

main :: IO ()
main = withCorrectLocale do
  args <- getArgs
  let (mode, warnings) = parseMode args

  -- Print command-line argument warnings (if any).
  mapM_ (hPutStrLn stderr) warnings

  case mode of

    UsageError e -> printUsageErrors [e]
    Help         -> putStrLn help >> exitSuccess
    Version      -> putStrLn (showVersion version) >> exitSuccess
    License      -> putStr license >> exitSuccess

    Target options file
      | target options == TargetCheck ->
          readFile file
            >>= parseCF options TargetCheck
            >>  return ()
      | otherwise ->
          readFile file
            >>= parseCF options (target options)
            >>= writeFiles (outDir options) . maketarget (target options) options

maketarget :: Target -> SharedOptions -> CF -> Backend
maketarget = \case
    TargetC            -> makeC
    TargetCpp          -> makeCppStl
    TargetCppNoStl     -> makeCppNoStl
    TargetHaskell      -> makeHaskell
    TargetHaskellGadt  -> makeHaskellGadt
    TargetLatex        -> makeLatex
    TargetJava         -> makeJava
    TargetOCaml        -> makeOCaml
    TargetPygments     -> makePygments
    TargetCheck        -> error "impossible"
    TargetTreeSitter   -> makeTreeSitter
