fact :: Int -> Int
fact x = if x <= 1 then 1 else x * fact (x - 1)

factString :: String -> String
factString str = "Answer: " ++ show (fact (read (head (lines str)))) ++ "\n"

main = interact factString