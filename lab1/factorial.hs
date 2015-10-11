fact :: Int -> Int
fact x = if x <= 1 then 1 else x * fact (x - 1)

main = do
	print ("Please enter a number")
	x <- getLine 
	print ("Factorial is ")
	print ( show ( fact ( read x ) ) )
