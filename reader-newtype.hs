{-# LANGUAGE InstanceSigs #-}
module ReaderPrezzo where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> (Reader r a) -> (Reader r b)
  fmap a2b (Reader r2a) = Reader $ a2b . r2a

instance Applicative (Reader r) where
  pure :: a -> (Reader r a)
  pure a = Reader $ \r -> a

  (<*>) ::
    (Reader r (a -> b))
    -> (Reader r a)
    -> (Reader r b)
  (<*>) (Reader r2a2b) (Reader r2a) =
    Reader $ \r -> (r2a2b r) (r2a r)

instance Monad (Reader r) where
  (>>=) ::
    (Reader r a)
    -> (a -> (Reader r b))
    -> (Reader r b)
  (>>=) (Reader r2a) a2r2b =
    Reader $ \r -> runReader (a2r2b (r2a r)) r

ask :: Reader a a
ask = Reader id






data DBConnection = DBConnection

data User = User {
  name :: String,
  boss :: String,
  email :: String } deriving Show

otavio = User "Otavio" "Miguel" "onascimento@zendesk.com"
miguel = User "Miguel" "Hemant" "mmolina@zendesk.com"

getUserById :: Int -> Reader DBConnection User
getUserById id = do
  conn <- ask
  -- use connection to run sql against some database
  return otavio

getUserByName :: String -> Reader DBConnection User
getUserByName name = do
  conn <- ask
  -- use connection to run sql against some database
  return miguel

main :: String
main = runReader bossEmailR DBConnection
  where bossEmailR = do
          user <- getUserById 42
          boss <- getUserByName $ boss user
          return $ email boss





maybeGetUserById :: Int -> Reader DBConnection (IO User)
maybeGetUserById id = do
  conn <- ask
  -- use connection to run sql against some database
  return $ pure otavio

maybeGetUserByName :: String -> Reader DBConnection (IO User)
maybeGetUserByName name = do
  conn <- ask
  -- use connection to run sql against some database
  return $ pure miguel

main2 :: IO String
main2 = do
  conn <- pure DBConnection
  fmap email $ runReader bossR conn
  where bossR = do
          ioUser <- (maybeGetUserById 42)
          Reader $ \r -> do
            u <- ioUser
            runReader (maybeGetUserByName (boss u)) r






newtype ReaderIO e a = ReaderIO { runReaderIO :: e -> IO a }

instance Functor (ReaderIO e) where
    fmap f (ReaderIO ea) = ReaderIO (fmap f . ea)

instance Applicative (ReaderIO e) where
    pure a = ReaderIO $ \_ -> pure a
    ReaderIO f <*> ReaderIO a = ReaderIO $ \e -> f e <*> a e

instance Monad (ReaderIO e) where
    ReaderIO ea >>= f = ReaderIO $ \e ->
       ea e >>= \a -> runReaderIO (f a) e

askIO :: ReaderIO a a
askIO = ReaderIO $ \e -> pure e

getUserByIdRIO :: Int -> ReaderIO DBConnection User
getUserByIdRIO id = do
  conn <- askIO
  -- use connection to run sql against some database
  return otavio

getUserByNameRIO :: String -> ReaderIO DBConnection User
getUserByNameRIO name = do
  conn <- askIO
  -- use connection to run sql against some database
  return miguel

main3 :: IO String
main3 = do
  conn <- pure DBConnection
  runReaderIO bossEmailR conn
  where bossEmailR = do
          user <- getUserByIdRIO 42
          boss <- getUserByNameRIO $ boss user
          return $ email boss
