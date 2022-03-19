{
module NotazionePolaccaToken (Token(..), lexer) where
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
\/ {\s -> TokenDiv}
$white+ ;


{
-- Il linguaggio formato da espressioni aritmetiche scritte in notazione poloacca diretta e costruite a partire dalle costanti intere e le 4 operazioni aritmetiche. L’analizzatore deve valutare l’espressione ricevuta in ingresso.
data Token = TokenInt Int | TokenAdd | TokenMinus | TokenMul | TokenDiv
 deriving (Eq,Show)
 
 
main = do
 s <- getContents
 print (alexScanTokens s)

lexer = alexScanTokens
}