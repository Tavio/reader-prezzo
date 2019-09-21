module ReaderPrezzo where

import Control.Monad.Reader

data DBConnection = DBConnection

data User = User {
  name :: String,
  boss :: String,
  email :: String } deriving Show

otavio = User "Otavio" "Miguel" "onascimento@zendesk.com"
miguel = User "Miguel" "Hemant" "mmolina@zendesk.com"

getUserById :: Int -> ReaderT DBConnection IO User
getUserById id = do
  conn <- ask
  -- use connection to run sql against some database
  return otavio

getUserByName :: String -> ReaderT DBConnection IO User
getUserByName name = do
  conn <- ask
  -- use connection to run sql against some database
  return miguel

main :: IO String
main = do
  conn <- pure DBConnection
  runReaderT bossEmailR conn
  where bossEmailR = do
          user <- getUserById 42
          boss <- getUserByName $ boss user
          return $ email boss
