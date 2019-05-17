module Main where

--import qualified Circleci

import           Protolude

int1OrError :: Either Text Int
int1OrError = Right 5

int2OrError :: Either Text Int
int2OrError = Right 6

int1or2 :: Either Text Int
int1or2 = do
  i1 :: Int <- int1OrError
  i2 :: Int <- int2OrError
  Right (i1 + i2)

-- int1or2 == Left "nothing here"

baz :: ()
baz = ()

-- readIntegerFromCommandLine :: IO Int

-- toMonad :: Monad m => a -> m a

-- toMonad :: a -> Either Text a
-- toMonad a = Right a

-- toMonad :: a -> Maybe a
-- toMonad a = Just a

main :: IO ()
main = do
  value <- putStrLn ("Hello" :: Text)
  i <- foo
  j :: Int <- pure (5 :: Int) :: IO Int
  let k = 5
  putStrLn (show (i + j + k) :: Text)
  putStrLn (show value :: Text)

-- myQuery :: DatabaseMonad Text
-- myQuery = do
--   let sql = "SELECT * FROM PEOPLE"
--   result <- executeQuery sql
--   pure ( show result :: Text )

  -- putStrLn ("Hello world" :: Text)
  -- putStrLn ("Hello again" :: Text)

  -- putStrLn (show (int1or2) :: Text)
  -- _i <- foo

foo :: IO Int
foo = pure 5

add :: Int -> Int -> Int
add a b = a + b

-- bar a = ...

-- type Either a b = ...

-- foo :: (ToJSON a, Print a, Num a, ) => a -> ...

-- func myDrive (a Vehicle) {
--   return a.drive()
-- }



-- function carpark(v1 Vehicle, v2 Vehivle) -> Vehicle{
--   ...
-- }

-- firstCar :: (Vehicle v) => v -> v -> v
-- firstCar v1 v2 = v1



-- function add (a: Int, b: Int) -> Int{
--   return a + b
-- }


