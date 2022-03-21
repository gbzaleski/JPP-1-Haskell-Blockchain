-- Author: Grzegorz B. Zaleski (418494)
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (label, val) str = (label ++ ':' : ' ' : show val) ++ str

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS ('\n' :)
pprH = intercalateS (' ' :)

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list str = foldl (\acc elem -> elem $ sep acc) (last list "") (drop 1 $ reverse list) ++ str

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith func lst = intercalateS ('\n' :) $ map func lst

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
