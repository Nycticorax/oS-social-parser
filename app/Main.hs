{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Parser
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let (d, t) = tuplify args
    parseFromDir d t
    where   tuplify [] = throw $ userError "Missing directory and service."
            tuplify [_] = throw $ userError "Missing service."
            tuplify (x:xs:_) = (x, xs)
