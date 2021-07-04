import Data.List ( sort, delete )
import GHC.Float
--import Data.Number

-- Conditionals
validateAndDivvy :: Integral txPayload => txPayload -> [Char]
validateAndDivvy payAmount
   | signum(payAmount) == -1 = "Payment amount can't be negative, sorry!"
   | payAmount == 100 = "Received amount equal to NFT price. Calling salesDivvy function..."
   | otherwise = "Transaction failed: Funds received do not match the price for requested item."
--validateAndDivvy payAmount = if payAmount == 100 then "Funds verified - allocating sales funds..." else "Sorry - you are too broke."
-- Check if the ADA amount in the transaction matches the asking price of the NFT


--Switch Statements
bmi :: Float -> Float -> String
bmi w h 
  | bmiCalc <= 18.5 = "Underweight"
  | bmiCalc <= 25.0 = "Normal"
  | bmiCalc <= 30.0 = "Overweight"
  | otherwise = "Obese"
  where bmiCalc = w/h^2



-- Sorting
gravityFlip :: Char -> [Int] -> [Int]
gravityFlip d boxData
  | d == 'R' = sort boxData
  | d == 'L' = reverse (sort boxData)


-- Basic String Operators
catLab :: [Char] -> [Char] -> [Char]
catLab c d = concat[c,d]


-- Number types affecting memory and rounding errors
squareFloat :: Float -> Float
squareFloat a = a / pi
-- other technique
squareDouble :: Double -> Double
squareDouble b = b + 1
-- other technique
squareFuck :: Float -> Double
squareFuck e = squareDouble (float2Double (squareFloat e))




-- Passing strings
greet :: String -> String
greet name = "Hello " ++ name ++ " how are you?"



removeExclamationMarks :: String -> String
-- removeExclamationMarks str = [ filter ('!') (head newString) | newString <- str ]
-- removeExclamationMarks str = [ if newString == '!' then '\\' else newString | if newString !== '!' <- str ]
removeExclamationMarks str = [ newString | newString <- str, newString /= '!' ]
-- , newString /= '!'
-- other technique
removeExclamationMarks' :: String -> String
removeExclamationMarks' = filter (/='!')



--- Repeat a string n number of times
repeatStr :: Int -> String -> String
repeatStr n str = concat [ str | _ <- [1..n], n /= 7 ]
-- other technique
repeatStr' :: Int -> String -> String
repeatStr' n = concat . replicate n  -- the dot means "do the left TO the right expression"
-- other technique
repeatStr'' :: Int -> String -> String
repeatStr'' n str = concat ( take n ( repeat str ) )

-- Convert numbers and strings
sumStr :: String -> String -> String
sumStr str1 str2 = show $ readInt str1 + readInt str2  -- the $ dollar 
readInt :: String -> Int
readInt "" = 0 -- if readInt input is a null character, convert to 0. Resolves a parsing error with "read" and empty strings.
readInt a = read a



-- Boilerplate code for all Plutus smart contracts
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
