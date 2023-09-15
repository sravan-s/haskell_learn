fortunes = [
  "you are lucky"
  , "you are going to be decent"
  , "its gonna rain"]

tellFortune:: String -> String
tellFortune x = fortunes !! ((length x) `mod` (length fortunes))

main = do
  putStrLn "Hello, what's your name?"  
  name <- getLine  
  putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

