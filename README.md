# Alex (Lexer)
## alex_7
In alex_7 you will find a parser that reconognize the following numbers:
* integer, eventually preceded by sign (+/-). Ex. +3, 3, -3
* rational, composed by 2 integers, one of which can be omitted. Ex. 1.2, .3, 1.
* floating point, Ex. 1e+3, -3e-5

This lexer will output a list of tokens

# Happy (Parser)
Both parser also import a lexer, which you will find in the same folders, built with alex.

N.B.: alex_7 isn't the lexer of happy_7, they are separate, happy_7's parser inside happy_7 folder.

## happy_2
* reconognize the matematical language of the direct polish notation, with integers and the operations: +,-,*,/. The parser will output a syntattic derivation tree.
## happy_7
Reconognize a language composed by the following expressions:
* integers
* mathemantical operations: +,-,*,/
* equality test: =
* "if then else" function

This parser will output the result of the operations (not a syntattic tree this time).

Ex. 
* input = "if = 3 3 then * + 3 4 / 6 3 else / 9 3"
* output = "14"