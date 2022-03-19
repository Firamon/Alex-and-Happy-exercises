{
module Main where
import LexerToken
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      int { TokenInt $$ }
      '+' { TokenAdd }
      '-' { TokenMinus }
      '*' { TokenMul }
	  "==" { TokenEqual }
	  if { TokenIf }
	  then { TokenThen }
	  else { TokenElse }
	  

%left else
%left then
%left '+' '-'
%left '*'
%nonassoc "=="
%left if
%%

Exp : int {$1}
 | if Exp "==" Exp then Exp else Exp {fun $2 $4 $6 $8}
 | Exp '*' Exp {$1 * $3}
 | Exp '+' Exp {$1 + $3}
 | Exp '-' Exp {$1 - $3}



{
parseError :: [Token] -> a
parseError _ = error "Parse error"

fun x1 x2 then_exp else_expr | (x1==x2) = then_exp | otherwise = else_expr

main = do s <- getContents
	  print (calc (lexer s))
}
