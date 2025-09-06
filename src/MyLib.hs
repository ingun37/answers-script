module MyLib (someFunc) where

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = putStrLn $ "someFunc" ++ prefixPath ++ src ++ dst