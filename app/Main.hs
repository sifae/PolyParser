{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import PolyModule
import Text.Layout.Table
import Data.Maybe(fromJust, isNothing)
import Control.Monad(when)

main :: IO ()
main = do
    putStrLn "Enter a polynomial:"
    polyStr <- getLine
    when (null polyStr) $ putStrLn "Empty Input" >> main
    let parsedPoly = readPoly polyStr :: (Read a, Num a) => Maybe (Poly a)
    when (isNothing parsedPoly) $ putStrLn "No Parse" >> main
    putStrLn "Enter point: "
    pointStr <- getLine
    let poly = simplifyPoly . fromJust $ parsedPoly :: (Num a, Read a, Eq a) => Poly a
    let point = read pointStr :: (Read a, Num a, Eq a) => a
    let evPoly = evalPoly poly :: (Num a, Read a, Eq a) => a -> a
    let diffPoly = diff evPoly
    putStrLn $ tableString [def , numCol]
                       asciiS
                       def
                       [ rowG ["Poly", printPoly poly]
                       , rowG ["Poly value", show . evPoly   $ point]
                       , rowG ["Derivative", show . diffPoly $ point]
                       ]
    main
