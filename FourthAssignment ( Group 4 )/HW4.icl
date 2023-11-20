module HW4
import StdEnv

//Task 1
/*
	You are given two lists (one contains numbers, the other one contains indexes) and a number n.
	Your task is to extract the numbers from the given numbers lists of the coresponding indexes,
	and to concatinate it with the rest of the list. Repeat this process n times, and return all 
	the encodings.
	
	Example:
	encode [1, 2, 3, 4, 5, 6] [2, 4] 3 => [[3,5,1,2,4,6],[1,4,3,5,2,6],[3,2,1,4,5,6]]
	
	Explanation:
	we take the numbers under indexes [2, 4] from the list: [1, 2, 3, 4, 5, 6] => [3, 5]
	and to that, we add the rest of the numbers in the same order => [3, 5] ++ [1, 2, 4, 6]
	and we repeat this the same process, but now on the new list: [3, 5, 1, 2, 4, 6]
	[1, 4] ++ [3, 5, 2, 6] => [1, 4, 3, 5, 2, 6]
	[3, 2] ++ [1, 4, 5, 6] => [3, 2, 1, 4, 5, 6]
	
	You can expect the indexes to always be existent.
*/


aux :: [Int] [Int] Int Int -> [[Int]]
aux list index times loop
| loop == times = []
=  [p] ++ aux (p) index times (loop+1)
where
	p = [list!!(index!!0),list!!(index!!1)] ++ [el \\ el <- list | el <> list!!(index!!0) && el <> list!!(index!!1) ]

check :: [Int] -> [Int]
check [x] = [x,0]
check [x:xs] = [x:xs]

encode :: [Int] [Int] Int -> [[Int]]
encode list index times = aux list (check index) times 0

Start = [1..5]
//Start = check [10]
//Start = encode [1, 2, 3, 4, 5, 6] [2, 4] 1 // [[3,5,1,2,4,6]]
//Start = encode [1, 2, 3, 4, 5, 6] [2, 4] 3 // [[3,5,1,2,4,6],[1,4,3,5,2,6],[3,2,1,4,5,6]]
//Start = encode [1, 2, 3, 4] [1, 3] 5 // [[2,4,1,3],[4,3,2,1],[3,1,4,2],[1,2,3,4],[2,4,1,3]]
//Start = encode [1, 2, 3, 4, 5, 6] [0, 5] 3 // [[1,6,2,3,4,5],[1,5,6,2,3,4],[1,4,5,6,2,3]]
//Start = encode [1, 2, 3, 4, 5, 6] [0, 1] 3 // [[1,2,3,4,5,6],[1,2,3,4,5,6],[1,2,3,4,5,6]]
//Start = encode [1, 2, 3, 4] [2, 0, 1] 3 // [[3,1,2,4],[2,3,1,4],[1,2,3,4]]
//Start = encode [1, 2, 3] [1] 3 // [[2,1,3],[1,2,3],[2,1,3]]
//Start = encode [1, 2, 3] [2, 1, 0] 3 // [[3,2,1],[1,2,3],[3,2,1]]


//Task 2
/*
	Garfield is a very lazy cat, but he loves to eat lasagnia. His owner wants Garfield to
	lose some weight. Somehow, Garfield is on a table that has a lot of empty plates. Help
	Garfield's owner find the plate that is furthest away from Garfield, so that he could
	place the lasagnia there and Garfield would have to make more effort.
	
	You are given the coordinate of Garfield (a tuple) and a list with the coordinates 
	of the plates (list of tuples). Return the Real value of the longest distance between
	the Garfield's coordinate and a plate coordinate.
	
	Use the distance formula: distance = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2)
*/


 //x_2 = fst coord

// ( fst coord - fsr el )^2 + ( snc coord - snd el )^2


max_dist :: (Int, Int) [(Int, Int)] -> Real
max_dist coord maze = reverse ( sort [ sqrt( toReal (( fst coord - fst el)^2 + ( snd coord - snd el)^2)) \\el <- maze ] ) !! 0 

//Start = max_dist (0, 0) [(0, 1), (0, 2)] // 2
//Start = max_dist (0, 0) [(0, 4), (0, -2), (3, 0), (10, 0)] // 10
//Start = max_dist (1, 3) [(1, 9), (6, 3), (1, -12)] // 15
//Start = max_dist (1, 3) [(1, 90), (-90, 3), (11, 3)] // 91
//Start = max_dist (5, 7) [(2, 6), (3, 8), (1, 9), (12, 23)] // 17.464249196573
//Start = max_dist (5, 7) [(14, 26), (-33, 5), (-12, 22), (1, -50), (-10, -12)] // 57.1401785086466