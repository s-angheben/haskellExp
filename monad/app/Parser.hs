module Parser where

import ParserLib
import MyType
import GHC.IO.Device (IODevice(isTerminal))
import MyData (MyMonoid(myMappend))
import MyControl


------------------------------------------------------
------ Term Parser

-- termP ::= number | '(' term '/' term ')' 

termP :: MP Term
termP = myMappend (myBind numberB (myReturn . Con))
        (myBind (lit '(') (\_ ->
            myBind termP (\t ->
                myBind (lit '/') (\_ ->
                    myBind termP (\u ->
                        myBind (lit ')') (\_ ->
                            myReturn (Div t u)))))))


getParsedResult :: MP a -> ParserState -> MyMaybe a
getParsedResult (MParser k) s = case k s of
                            Nil -> MyNothing  
                            Cons (a, s) xs -> MyJust a






------------------------------------------------------
------ EXAMPLES

-- Parse one Char

exampleItem :: MyList (Char, ParserState)
exampleItem = runParser item (Cons 'a' (Cons 'b' Nil))
exampleItem' :: MyList (Char, ParserState)
exampleItem' = runParser item Nil

-- Succeeds only if it parse two Char

exampleTwoItems :: MyList ((Char, Char), ParserState)
exampleTwoItems = runParser twoItems (Cons 'a' (Cons 'b' (Cons '3' Nil)))
exampleTwoItems' :: MyList ((Char, Char), ParserState)
exampleTwoItems' = runParser twoItems (Cons 'a' Nil)


-- Alternation

exampleAlternation = runParser oneOrTwoItems (Cons 'a' (Cons 'b' (Cons '3' Nil)))

-- Filter
exampleOk = runParser letter (Cons 'a' Nil)
exampleFail = runParser letter (Cons '4' Nil)

exampleDigit = runParser digit (Cons '1' Nil)


-- Iterate
iterateDigit = runParser (iterateNP 3 digit) (Cons '1' (Cons '3' (Cons '4' Nil)))

exampleIterate = runParser (iterateP digit) (Cons '1' (Cons '3' (Cons 'a' Nil)))


-- reiterate
exampleReiterate = runParser (reiterate digit) (Cons '2' (Cons '3' (Cons 'a' Nil)))

-- combines alternation
-- all ways of taking one or two items from the string "many"
exampleMany = runParser (reiterate oneOrTwoItems) (Cons 'm' (Cons 'a' (Cons 'n' (Cons 'y' Nil))))

