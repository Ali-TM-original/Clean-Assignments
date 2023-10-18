module HW1

import StdEnv


//Please write your neptun code here: D946BF

/*
Don't copy the others' work, otherwise, you won't get point for this homework.
Changing the function and variable does not help. 
	
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.
*/


/*
Task 1

Intro:
The binary number system is the base of all computing systems and operations. Hence, as Computer Scientists we 
should be informed about this number system!
Read about it here: https://www.cuemath.com/numbers/binary-to-decimal/
Task:
Define a function which will convert a 3 digit binary number to a decimal number.
The binary number will not be provided as a single integer, but rather 3 separate integers, each representing
one digit of the binary number.
As you should have already learned, binary digits can either be 0 or 1, so be sure to check that
*/


// Functions checks if an integer is 1 or 0
CheckIfBin :: Int -> Bool
CheckIfBin num 
| (num == 1) || (num == 0) = True
| otherwise = False


binToDec :: Int Int Int -> Int
binToDec pos1 pos2 pos3 = total
where
	/*
		For converting Binary to Denary We use this method
		1>. Let's say we have a Binary value of 1101
			
			16 8 4 2 1
			0  1 1 0 1 -> Look at the values where there is 1 and add those values together
			
			so 1101 becomes 8+4+1 = 13
			
		2>. Since we are taking 3 integers as inputs the max value they can give is 
			4 2 1
			1 1 1 -> 4+2+1=7 -> MAX VALUE IN DENARY
			
			4 2 1
			0 0 0 -> 0 -> LOWEST VALUE IN DENARY
			
			therefore any value above 7 is overflow and any value below 0 is underflow meaning
			digits other than 1,0 were used in the calculation of total 
		
	*/
	total
	| ((CheckIfBin pos1) && (CheckIfBin pos2) && (CheckIfBin pos3)) = (pos1*4) + (pos2*2) + (pos3*1) 
	| otherwise = abort "One of the inputs is Not a binary digit!"


//Start = binToDec 1 2 1 // "One of the inputs is NOT a binary digit!"
//Start = binToDec 9 1 1 // "One of the inputs is Not a binary digit!"
//Start = binToDec 1 2 7 // "One of the inputs is Not a binary digit"
//Start = binToDec 1 1 1 // 7
//Start = binToDec 0 1 0 // 2
//Start = binToDec 1 1 0 // 6
//Start = binToDec 1 0 0 // 4

// --------------------------------------------------------------------------------------------------------------------------------------------------

/*
Task 2
Create a function that determines whether or not a given year is a leap year. 
Leap years are determined by the following rules:

Leap years are years divisible by four (like 1984 and 2004). 
However, years divisible by 100 are not leap years (such as 1800 and 1900) 
unless they are divisible by 400 (for example 800, 1600).
*/

// Better version of rem I guess lol
CompleteDivision  :: Int Int -> Int
CompleteDivision dividend divisor
| dividend < divisor = dividend
| otherwise = CompleteDivision (dividend - divisor) divisor

leapYear :: Int -> Bool
leapYear year = check
where
	// Checks if year / 100 
	check
  		| ( ( CompleteDivision year 100 ) == 0 ) && ( ( CompleteDivision year 400 ) == 0 ) = True
  		| ( ( CompleteDivision year 4 ) == 0 ) = True
  		| otherwise = False 

//Start = leapYear 1996 // true
//Start = leapYear 1997 // false
//Start = leapYear 1900 //false
//Start = leapYear 1600 //true
