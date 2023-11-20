module HW5
import StdEnv


// Name:   Neptun Code: 
// Plagiarism of any kind will not be accepted !

/*
Higher Order functions - This question should be solved using higher order functions

Given a list of lists, for each list let's create a tuple. 
If the length of the list isodd, the first item of the tuple should be the middle element.
If the length of the list is even, the first item of the tuple should be the average of the two middle elements.
The second item of the tuple should be a boolean value. The boolean value should be true, if the list remains increasingly sorted after
we remove the item at index of the first item of the tuple.

For example, 
	[3,4,2,5,6] -> (2, True) 
	The middle element is 2, hence the first item of the tuple is 2. 
	When we remove the number at index 2 the list is now [3,4,5,6] which is sorted.
	
	[1,2,2,4,3,9] -> (3, True)
	The first item of the tuple will be 3, because the list is of even length and (2+4)/ 2 = 3
	When we remove the item at index 3, the list is now [1,2,2,3,9] which is sorted, so the second item of the tuple should be true 
	
Tips: removeAt x list -> will remove the item at index x from the list supplied
	  sort list -> will returned a sorted version of the list supplied
*/


check :: [Int] Int -> Bool
check [] index = False
check list index = sort(removeAt index list) == removeAt index list

firstItem :: [Int] -> Int
firstItem [] = 0
firstItem list
| isEven len == True  = (list !! toInt((len-1)/2) + list !! toInt((len+1)/2))/2
= list !! toInt(len/2)
where
	len = length list

f :: [[Int]] -> [(Int,Bool)]
f list = [( firstItem el, check el (firstItem el) ) \\ el <- list]

//Start = f [[3,4,2,5,6],[1,2,2,4,3,9]]  // [(2, True),(3, True)]
//Start = f [[4,4,4,4,4,4,4],[1,2,10,2,3,4,5],[]] // [(4,True),(2,True),(0,False)]
//Start = f [[9,8,7,6,5,4],[1,2,1,2,3,4]] // [(6,False),(1,True)]


/*
List comprehension question - This question should be solved using only List comprehension.

Given two lists compute their intersection and their difference.
Example, 
	[1,2,3,4] [3,4,5,6] -> [[3,4],[1,2]]
	[10,20,30,34,32] [99,10,20,30,33] -> [[10,20,30],[34,32]]
	
*/

f2 :: [Int] [Int] -> [[Int]]
f2 listA listB = [[x\\ x <- listA, y <- listB | x == y], [x\\ x <- listA | not (isMember x listB) ]]

//Start = f2 [1..4] [5..9] // [[],[1,2,3,4]] 
Start = f2 [1..10] [1..10] // [[1,2,3,4,5,6,7,8,9,10],[]]
//Start = f2 [1..4] [3..6] // [[3,4],[1,2]]
//Start = f2 [10,20,30,34,32] [99,10,20,30,33] // [[10,20,30],[34,32]]
//Start = f2 [] [] //[[],[]]


