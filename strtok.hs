strtokHelper :: [Char] -> Char -> Bool -> [[Char]]
strtokHelper inString delim ignoreDelim
	| inString == [] = [[]] 
	| firstChar == delim = 	
		if ignoreDelim 
			then strtokHelper (tail inString) delim True
			else []:(strtokHelper (tail inString) delim True)
	| otherwise = [ [firstChar] ++ head result ] ++ tail result
	where 	result = strtokHelper (tail inString) delim False
			firstChar = head inString

strtok :: [Char] -> Char -> [[Char]]
strtok string delimiter = strtokHelper string delimiter True