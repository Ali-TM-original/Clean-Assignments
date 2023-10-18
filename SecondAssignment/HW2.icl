module HW2
import StdEnv


//Name:       Ali-TM-original         Neptun Code: 000000

/*
Question 1
In maths, a happy number is a number which will eventually reach 1 when replaced by the sum of square
of each digit. On the contrary, unhappy numbers will eventually reach 4 when they undergo the same process.
For exmaple, 
	13 is a happy number, because 13 -> 1^2 + 3^2 = 10 -> 1^2 + 0^2 = 1
	
	11 is not a happy number, because 11 -> 1^2 + 1^2 = 2 -> 2^2 = 4 
	
Tip: You need a helper function which will compute the sum of the square of each digit.
*/

SumSquareOfDigits :: Int -> Int
SumSquareOfDigits n
| n < 10 = n ^ 2
= (n rem 10) ^ 2 + SumSquareOfDigits(n / 10)

RecursiveSSOD :: Int -> Int
RecursiveSSOD n
| n < 10 = n
= RecursiveSSOD(SumSquareOfDigits(n))

isHappy :: Int -> Bool
isHappy num | (RecursiveSSOD num) == 1 = True = False



//Start = isHappy 0 //False
//Start = isHappy 1 //True
//Start = isHappy 4 //False
//Start = isHappy 12 //False
//Start = isHappy 13 //True
//Start = isHappy 68 //True

/*
Question 2
Given a number n, go through all numbers between 1 and n, and append every HappyNumber that you come accross
to a string. Output the string. 
*/

Concator :: Int [Int] -> String
Concator counter lst 
| counter == (length (lst) - 1) = toString(lst!!counter)
= ( toString ( lst!!counter ) ) +++" "+++ Concator (counter+1) lst

HappyNumbers :: Int -> String
HappyNumbers num = Concator 0 (filter isHappy [num, num-1..1])

Start = HappyNumbers 100 // "100 97 94 91 86 82 79 70 68 49 44 32 31 28 23 19 13 10 7 1 "
