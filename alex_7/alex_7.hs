{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "alex_7.x" #-}

module Main (main) where

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc551d_0/ghc_2.h" #-}














































































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
{-# LINE 28 "templates/wrappers.hs" #-}

import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

{-# LINE 79 "templates/wrappers.hs" #-}

{-# LINE 102 "templates/wrappers.hs" #-}

{-# LINE 120 "templates/wrappers.hs" #-}

{-# LINE 138 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

{-# LINE 161 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 274 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 379 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing



-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 425 "templates/wrappers.hs" #-}

{-# LINE 440 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 457 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 473 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 15)
  [ -8
  , -37
  , -23
  , 2
  , 14
  , 26
  , 38
  , 60
  , 71
  , 93
  , 126
  , 136
  , 159
  , 169
  , 48
  , 121
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 424)
  [ 0
  , 15
  , 15
  , 15
  , 15
  , 15
  , 1
  , 0
  , 1
  , 0
  , 0
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 0
  , 0
  , 0
  , 15
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 4
  , 0
  , 5
  , 3
  , 0
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 2
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 3
  , 0
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 10
  , 0
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 6
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 14
  , 11
  , 1
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 1
  , 15
  , 15
  , 15
  , 15
  , 15
  , 0
  , 0
  , 0
  , 0
  , 1
  , 1
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 15
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 8
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 1
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 12
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 13
  , 1
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 424)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 43
  , -1
  , 45
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , 32
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 43
  , -1
  , 45
  , 46
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 46
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 46
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 46
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 46
  , 69
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 69
  , 9
  , 10
  , 11
  , 12
  , 13
  , -1
  , -1
  , -1
  , -1
  , 101
  , 69
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 101
  , 69
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 101
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 101
  , 69
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 69
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 101
  , 69
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 101
  , 69
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 101
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 101
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 15)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 15)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  , AlexAccSkip
  ]

alex_actions = array (0 :: Int, 9)
  [ (8,alex_action_0)
  , (7,alex_action_1)
  , (6,alex_action_2)
  , (5,alex_action_3)
  , (4,alex_action_4)
  , (3,alex_action_5)
  , (2,alex_action_6)
  , (1,alex_action_7)
  , (0,alex_action_8)
  ]

{-# LINE 45 "alex_7.x" #-}

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

alex_action_0 = \s -> TokenInt (read s) (read $ show s)
alex_action_1 = \s -> TokenInt (read (tail s)) (read $ show s)
alex_action_2 = \s -> TokenFrac (read s) (read $ show s)
alex_action_3 = \s -> TokenFrac (read (tail s)) (read $ show s)
alex_action_4 = \s -> TokenFrac (read (s++"0")) (read $ show s)
alex_action_5 = \s -> TokenFrac (read ((tail s) ++ "0")) (read $ show s)
alex_action_6 = \s -> TokenFrac (read ("0"++s)) (read $ show s)
alex_action_7 = \s -> TokenFrac (read ("0" ++ (tail s))) (read $ show s)
alex_action_8 = \s -> TokenFloatingPt (read s) (read $ show s)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8a9b_0/ghc_2.h" #-}














































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 21 "templates/GenericTemplate.hs" #-}

{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 72 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 93 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 105 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
