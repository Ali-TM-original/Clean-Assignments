module HW7
import StdEnv

// NAME: Ali-TM-Original ->>>>>  NEPTUNCODE : 000000

/*
Task 1 - Modify Strings	
Mr. Johnson, an elderly individual, faces difficulty in finding special characters on his keyboard. So
he just writes something he can find in the keyboard instead. As a helpful gesture, 
our task is to assist Mr. Johnson by replacing certain characters in the sentences he writes. 
Specifically:

Replace '+' with ','
Replace '-' with '.'
Replace ']' with '?'
Replace '[' with '!'
	
For example,
	"I am getting old+ I think-" -> "I am getting old, I think."
	"Can you hear me]" -> "Can you hear me?"
*/

charCheck :: Char -> Char
charCheck n
| n == '+' = ','
| n == '-' = '.'
| n == ']' = '?'
| n == '[' = '!'
= n

combineTogether :: [Char] -> String
combineTogether [x:xs]
| xs == [] = toString(x)
= toString(x) +++ (combineTogether xs)

replaceSpecial :: String -> String
replaceSpecial word = combineTogether [charCheck el \\ el <-: word]

//Start = replaceSpecial "+-][" // ",.?!"
//Start = replaceSpecial "I am getting old+ I think-" // "I am getting old, I think."
//Start = replaceSpecial "Can you hear me]" // "Can you hear me?"
//Start = replaceSpecial "I am happy[" // "I am happy!"


/*
Task 2 - caesar cypher (Decryption)
The Caesar cipher is a simple and ancient encryption technique that involves shifting 
the letters of the alphabet by a fixed number of positions.
In this exercise, you have to decrypt the given string using the given integer,
This means that you have to shift every letter in the string backwards by the given integer.
For example,
	 Given "khoor" 3, return "hello"
	 In the alphabet, 3 letters before k comes h
	 3 letters before h comes e
	 3 letters before o comes l
	 3 letters before o comes l 
	 3 letters before r comes o
	 
You should not shift special characters,but leave them as is
*/


checkAndConvert :: Char Int -> Char
checkAndConvert el key
| ((fromChar el) >= 65 && (fromChar el) <= 90) || ((fromChar el) >= 97 && (fromChar el) <= 122) = fromInt(fromChar el - key)
= el

decryptCaesar :: String Int -> String
decryptCaesar word key = toString [checkAndConvert el key \\ el <-: word]

//Start = decryptCaesar "khoor" 3 // "hello"
//Start = decryptCaesar "Kzshyntsfq-nx-httq" 5 // "Functional-is-cool"
//Start = decryptCaesar "Rlajobw dhz bzlk hz tlkpjpul!" 7 // "Ketchup was used as medicine!"