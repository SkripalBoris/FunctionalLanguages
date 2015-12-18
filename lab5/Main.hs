module Main where

import System.Environment
import Data.Functor

import Calc(calculate)

main :: IO ()
main = do
            expr <- unwords <$> getArgs
            case (calculate expr) of
                 (Right result) -> print result
                 (Left error) -> print error
            return ()