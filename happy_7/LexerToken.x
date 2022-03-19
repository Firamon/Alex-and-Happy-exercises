{
module LexerToken (Token(..), lexer) where
}

%wrapper "basic"

-- reg expressions
$digit = 0-9
@num = $digit+

tokens :-

-- rules
@num {\s -> TokenInt (read s)}
\+ {\s -> TokenAdd}
\- {\s -> TokenMinus}
\* {\s -> TokenMul}
"==" {\s -> TokenEqual}
"if " {\s -> TokenIf}
"then " {\s -> TokenThen}
"else " {\s -> TokenElse}
$white+ ;


{
{-
Sequenze di espressioni in linguaggio Haskell formate da:

* numeri interi
* operazioni aritmetiche: +, *, -
* test di uguaglianza: ==
* la funzione if then else
	L’analizzatore deve valutare le espressioni ricevute in ingresso.
-}
data Token = TokenInt Int | TokenAdd | TokenMinus | TokenMul | TokenEqual | TokenIf | TokenThen | TokenElse
 deriving (Eq,Show)
 
 
main = do
 s <- getContents
 print (alexScanTokens s)

lexer = alexScanTokens
}