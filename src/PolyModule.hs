module PolyModule( 
    readPoly,
    evalPoly,
    printPoly,
    simplifyPoly,
    diff,
    Poly(..)
) where

import ParserModule
import DiffModule
import Data.Char(isDigit)
import Data.Function (on)
import Data.List (sortBy)
import Control.Applicative(some, many, (<|>))

type Poly a = [(a,Int)]  -- a - coefficient type (Integer, Float ...)

-- | Variable name
var :: Char
var = 'X'


-- | Parse polynomials: x^2+2x + ...
-- | Add <* many (charP ' ') to handle last n spaces
-- | ParseVariable' is terminate condition
parsePoly :: (Read a, Num a) => Parser (Poly a)
parsePoly = parseVariable' <* many (charP ' ') <|> parse where
    -- | Parse all variables and coefficients 
    parse = handler <$> parseVariable' <*> parseSign <*> parsePoly
    -- | Check if first term goes with '-' sign
    parseVariable' = parseVariable <|> (handler' <$> parseMinus <*> parseVariable)
    -- | Handle situations when first term sign is '-'
    handler' sig ((x,deg):xs) = (x * sigInt sig, deg):xs
    -- | Add different signs support and handle computations
    handler xs sig ((y,deg):ys) = xs <|> (y * sigInt sig,deg):ys
    -- | Parse plus  sign
    parsePlus = many (charP ' ') *> charP '+' <* many (charP ' ')
    -- | Parse minus sign
    parseMinus = many (charP ' ') *> charP '-' <* many (charP ' ')
    -- | Parse sign
    parseSign = parsePlus <|> parseMinus
    -- | Convert char sign to Integer 
    sigInt x = if x == '+' then 1 else (-1)

-- | Covert Polynomial to Function
evalPoly :: Num a => Poly a -> a -> a
evalPoly []           _ = 0
evalPoly ((x,deg):xs) y = x*(y^deg) + evalPoly xs y

-- | Variable : ax^k, x^k, ax, x, a 
parseVariable :: (Read a, Num a) => Parser (Poly a)
parseVariable = powerK <|> coeffPower <|> power <|> coeff <|> coeffPowerK where
    -- Parser Poly | ax^k
    coeffPowerK   = (\a deg -> [(read a, read deg)]) <$> parseCoeffVar <*> parsePower
    -- Parser Poly | ax
    coeffPower    = (\a -> [(read a, 1)]) <$> parseCoeffVar
    -- Parser Poly | x
    power         = const [(1, 1)] <$> charP var
    -- Parser Poly | a
    coeff         = (\a -> [(read a,0)]) <$> some (predP isCoefficient)
    -- Parser Poly | x^k 
    powerK        = (\deg -> [(1,read deg)]) <$> ((charP var) *> parsePower)
    -- Parser Str  | ^k
    parsePower    = (charP '^') *> some (predP isCoefficient)
    -- Parser Str  | ax
    parseCoeffVar = some (predP isCoefficient) <* (charP var)

-- | Check if char is polynom coefficient (digit or float point)
isCoefficient :: Char -> Bool
isCoefficient c = (isDigit c) || (c == '.')

-- | Read polynom from input string
readPoly :: (Num a, Read a) => String -> Maybe (Poly a)
readPoly = runParserUnique parsePoly

-- | Sort polynomial by degree and remove zero components
simplifyPoly :: (Num a, Eq a) => Poly a -> Poly a 
simplifyPoly = foo . sortBy (flip compare `on` snd) where
    foo [] = []
    foo (x:[]) = [x]
    foo ((x,deg):(x',deg'):xs) 
        | (x == -x') && (deg == deg') = foo xs
        | deg == deg' = (x+x', deg) : foo xs
        | x' == 0     = foo $ (x, deg'):xs
        | x == 0      = foo $ (x', deg'):xs
        | otherwise   = (x, deg) : foo ((x',deg'):xs)

-- | Print Polynomial
printPoly :: (Show a, Ord a, Num a) => Poly a -> String
printPoly = printPoly' 0 where 
    printPoly' :: (Show a, Ord a, Num a) => Integer -> Poly a -> String
    printPoly' _ [(c,deg)]  
        | (c == 1) && (deg == 1) = [var]                 -- X
        | (c /= 1) && (deg == 1) = show c ++ [var]       -- aX
        | (deg == 0) = show c                            -- a
        | (c == 1) = [var] ++ "^" ++ show deg            -- X^k
        | otherwise = show c ++ [var] ++ "^" ++ show deg -- aX^k
    printPoly' n ((c,deg):(c',deg'):xs) 
        | (abs c == 1) && (deg == 1) = firstSign ++ [var] ++ next                -- X
        | (abs c /= 1) && (deg == 1) = firstSign ++ show c ++ [var] ++ next      -- aX
        | (deg == 0) = firstSign ++ show c ++ next                               -- a
        | (abs c == 1) = firstSign ++ [var] ++ "^" ++ show deg ++ next           -- X^k
        | otherwise = firstSign ++ show c ++ [var] ++ "^" ++ show deg ++ next    -- aX^k
        where 
            numToSig x = if x > 0 then "+" else (if x == -1 then "-" else "")
            next = numToSig c' ++ printPoly' (n+1) ((c',deg'):xs)
            firstSign = if (c == -1) && (n == 0) then "-" else ""