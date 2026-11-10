module Lessons.Lesson09 where

io :: IO ()
io = fmap (\_ -> ()) getLine

io' :: IO ()
io' = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name

askName :: IO String
askName = do
  putStrLn "What is your name?"
  getLine

askName' :: IO String
askName' = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "Thank you"
  pure name

io'' :: IO ()
io'' = do
  name <- askName
  putStrLn $ "Hello, " ++ name

f :: IO String
f = askName'

askNameWOdo :: IO String
askNameWOdo =
  putStrLn "What is your name?" >>= (\_ -> getLine)

askNameWOdo1 :: IO String
askNameWOdo1 =
  putStrLn "What is your name?" >> getLine

askNameWOdo' :: IO String
askNameWOdo' =
  putStrLn "What is your name?"
    >> getLine
    >>= ( \name ->
            putStrLn "Thank you" >> pure name
        )

askNameA :: IO String
askNameA = putStrLn "What is your name?" *> getLine <* putStrLn "Thank you"

maybeM :: Maybe (String, Int)
maybeM = do
  v1 <- Just 5
  v2 <- Just "hi"
  pure (v2, v1 * 2)

maybeM' :: Maybe (String, Int)
maybeM' = do
  v1 <- Just 5
  v2 <- Nothing
  pure (v2, v1 * 2)

maybeM'' :: Maybe (String, Int)
maybeM'' = do
  v1 <- Just 5
  v2 <- Just "hi"
  _ <- Nothing
  pure (v2, v1 * 2)

maybeMWOdo'' :: Maybe (String, Int)
maybeMWOdo'' =
  Just 5 >>= (\v1 -> Just "hi" >>= (\v2 -> Nothing >>= (\_ -> pure (v2, v1 * 2))))

eitherM :: Either Int Int
eitherM = do
  v1 <- Right 42
  v2 <- Left 0
  return $ v1 * v2

listM :: [Int]
listM = do
  v1 <- [1, 2, 3, 4]
  _ <- ['a', 'b']
  pure v1

listC :: [Int]
listC = [v1 | v1 <- [1, 2, 3, 4], _ <- ['a', 'b']]
