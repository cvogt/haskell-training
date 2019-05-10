module Main where

--import qualified Circleci

import           Protolude

-- function add (a: Int, b: Int) -> Int{
--   a + b
-- }

-- foo :: (ToJSON a, Print a, Num a, ) => a -> ...

-- func myDrive (a Vehicle) {
--   return a.drive()
-- }

add :: Int -> Int -> Int
add a b = a + b


main :: IO ()
main = do
  putStrLn (show (add 5 6) :: Text)
