{
module Main where
import NotazionePolaccaToken
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      int             { TokenInt $$ }
      '+'             { TokenAdd }
      '-'             { TokenMinus }
      '*'             { TokenMul }
      '/'             { TokenDiv }
%%
 
Exp : int {$1}
 | '/' Exp Exp {$2 `div` $3}
 | '*' Exp Exp {$2 * $3}
 | '-' Exp Exp {$2 - $3}
 | '+' Exp Exp {$2 + $3}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"


main = do s <- getContents
	  print (calc (lexer s))
}

