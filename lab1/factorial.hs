fact :: Int -> Int
fact x = if x <= 1 then 1 else x * fact (x - 1)

main = do
	print ("Please enter a number")
	x <- readLn :: IO Int
	print ("Factorial is ")
	print (fact x)
