{
module Main (main) where
}
%wrapper "basic"

-- reg expressions
$digit = 0-9
@int_v1 = [\-] $digit+ | $digit+
@int_v2 = [\+] $digit+
@int = @int_v1 | @int_v2

@frac_d_d = $digit+ \. $digit+
@frac_d_v = $digit+ \.
@frac_v_d = \. $digit+

@frac_d_d_v1 = [\-] @frac_d_d | @frac_d_d
@frac_d_d_v2 = [\+] @frac_d_d
@frac_d_v_v1 = [\-] @frac_d_v | @frac_d_v
@frac_d_v_v2 = [\+] @frac_d_v
@frac_v_d_v1 = [\-] @frac_v_d | @frac_v_d
@frac_v_d_v2 = [\+] @frac_v_d
@frac = @frac_d_d_v1 | @frac_d_d_v2 | @frac_d_v_v1 | @frac_d_v_v2 | @frac_v_d_v1 | @frac_v_d_v2

@float_pt = @int [\e\E] @int | @frac [\e\E] @int

tokens :-

-- rules
@int_v1 {\s -> TokenInt (read s) (read $ show s)}
@int_v2 {\s -> TokenInt (read (tail s)) (read $ show s)}


@frac_d_d_v1 {\s -> TokenFrac (read s) (read $ show s)}
@frac_d_d_v2 {\s -> TokenFrac (read (tail s)) (read $ show s)}
@frac_d_v_v1 {\s -> TokenFrac (read (s++"0")) (read $ show s)}
@frac_d_v_v2 {\s -> TokenFrac (read ((tail s) ++ "0")) (read $ show s)}
@frac_v_d_v1 {\s -> TokenFrac (read ("0"++s)) (read $ show s)}
@frac_v_d_v2 {\s -> TokenFrac (read ("0" ++ (tail s))) (read $ show s)}

@float_pt {\s -> TokenFloatingPt (read s) (read $ show s)}

$white+ ;


{
{-
Riconoscere, in un file di testo, le sequenze di caratteri che rappresentano un numero in uno dei seguenti formati:

* numero intero: stringa di cifre decimali, eventualmente precedute dal segno (+/-)
* numero frazionari: coppia di stringhe di cifre intervallate da un punto ed eventualmente precedute dal segno, una delle due stringe, ma non entrambe può essere vuota.
	6 regole
	: [epsilon,-] 0.4 = 0.4 ; +0.4 = 0.4
	: [epsilon,-] .1 = 0.1 ; +.1 = 0.1
	: [epsilon,-] 3. = 3.0 ; +3. = 3.0
	
* floating point: numero intero o frazionario, seguito da “e”, oppure “E”, seguito da un numero intero.
	
	Per ciascuna sequenza riconosciuta, stampare in uscita: la sequenza stessa, il tipo di numero rappresentato, il numero di cifre usate nella rappresentazione.
-}

path_in = "C:\\Users\\grand\\Desktop\\Uniud\\linguaggi_di_programmazione\\esercizi_happy_alex_esame\\in_file.txt"

data Token = TokenInt Int String | TokenFrac Float String | TokenFloatingPt Float String
 deriving (Eq,Show)
 
-- functions that check constructor
isTokenInt (TokenInt _ _) = True
isTokenInt _ = False

isTokenFrac (TokenFrac _ _) = True
isTokenFrac _ = False

isTokenFloatingPt (TokenFloatingPt _ _) = True
isTokenFloatingPt _ = False

-- functions that return the number inside the tokens
getTokenIntVal (TokenInt val _) = val

getTokenFracVal (TokenFrac val _) = val

getTokenFloatingPtVal (TokenFloatingPt val _) = val

-- functions that return the string inside the tokens
getTokenIntStr (TokenInt _ str) = str

getTokenFracStr (TokenFrac _ str) = str

getTokenFloatingPtStr (TokenFloatingPt _ str) = str

-- function that prints triplets containing: <value, type of value, length of the original expression that became value>
get_sequence_numberType_length [] = []
get_sequence_numberType_length (x:xs) | (isTokenInt x) = (print ((getTokenIntVal x), "Integer", (length(getTokenIntStr x)))):(get_sequence_numberType_length xs)
 | (isTokenFrac x) = (print ((getTokenFracVal x), "Float", (length(getTokenFracStr x)))):(get_sequence_numberType_length xs)
 | (isTokenFloatingPt x) = (print ((getTokenFloatingPtVal x), "Float", (length(getTokenFloatingPtStr x)))):(get_sequence_numberType_length xs)
 
main = do
 s <- readFile path_in;
 sequence (get_sequence_numberType_length (alexScanTokens s));
}