module HW3
import StdEnv

// Name: 
// Neptune Code: 

/*
Given a list of lists, for each list create a new list which has 2 numbers.
The first number represents the number of even numbers in that list.
The second number represents the sum of the even numbers in that list.

Tip: You can use in Built isEven function

For Example, 
	[[1,2,3,4],[71,43,42,92,3,8,1,8],[90,2,4,4],[]] => [[2,6],[4,150],[4,100],[0,0]]
*/

Working :: [Int] -> [Int]
Working list = [length filtered, foldr (+) 0 filtered ]
where
	filtered = filter (\x = isEven x) list

countSumEvens :: [[Int]] -> [[Int]]
countSumEvens list = map Working list

//Start = countSumEvens [[1,2,3,4],[71,43,42,92,3,8,1,8],[90,2,4,4],[]] // [[2,6],[4,150],[4,100],[0,0]]
//Start = countSumEvens [[1,3,5,6],[102,104,104,104],[2,2,2],[1,1,1]] // [[1,6],[4,414],[3,6],[0,0]]
//Start = countSumEvens [[]] // [0,0]
 
/*
Given a list of lists and a character, for every list create a new list with two elements.
The first element is the number of occurrences of the given character.
The second element is the number of occurrences of every element except the given character. 
Note that the input character will be given lowercased, but when counting the occurrences
of the given character you should count it's uppercased version as well.

For example, 
	[['A','b','c','D'],['d','D','e','l'],['a','p','x'],[]] 'd'-> [[1,3], [2,2],[0,3],[0,0]]
*/

// Convert Everthing to Lower for better understanding

countCharUtil :: [Char] Char -> [Int]
countCharUtil list letter = [length newList, length list - length newList]
where
	newList = [toLower(e)\\e<-list | toLower(e) == toLower(letter)]


countChar :: [[Char]] Char -> [[Int]]
countChar list letter = map (\e = countCharUtil e letter) list

//Start = countChar [['A','b','c','D'],['d','D','e','l'],['a','p','x'],[]] 'd' // [[1,3], [2,2],[0,3],[0,0]]
//Start = countChar [[]] 'c' // [[0,0]]
//Start = countChar [['A','a'],['A','A'],['a','a'],['b','c','a']] 'a' //[[2,0],[2,0],[2,0],[1,2]]
