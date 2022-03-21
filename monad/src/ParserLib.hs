{-# LANGUAGE LambdaCase #-}
module ParserLib where

import MyControl
import MyType
import Data.Monoid (Ap)
import MyData
import Data.Char (isLetter, isDigit, digitToInt)

newtype MParser s a = MParser {runParser :: s -> MyList (a, s)}
type ParserState = MyList Char

type MP = MParser ParserState

instance MyFunctor (MParser s) where
    myFmap f (MParser k) = MParser (\s -> let x = k s
                                          in myFmap (\(q, w) -> (f q, w)) x)

instance MyApplicative (MParser s) where
    myPure a = MParser (\s -> Cons (a, s) Nil)

    myApply (MParser f) (MParser a) = MParser (\s -> let gt = f s
                                                         zu = join $ myFmap (\(g, t) -> a t) gt
                                                     in lift2 (\(g, t) (z, u) -> (g z, u)) gt zu)

instance MyMonad (MParser s) where
    myReturn = myPure

    myBind (MParser m) k = MParser (\s -> let ay = m s
                                          in join $ myFmap (\(a, y) -> runParser (k a) y) ay)


getParsedResult :: MP a -> ParserState -> MyMaybe a
getParsedResult (MParser k) s = case k s of
                            Nil -> MyNothing  
                            Cons (a, s) xs -> MyJust a


item :: MP Char
item = MParser (\case
                Nil -> Nil
                Cons x xs -> Cons (x, xs) Nil)

twoItems :: MP (Char, Char)
twoItems = myBind item (\a -> myBind item (\b -> myReturn (a, b)))


-- ALTERNATIONS

instance MySemigroup (MParser s a) where
  mult p1 p2 =
      MParser (\s -> let r1 = runParser p1 s
                         r2 = runParser p2 s
                     in mult r1 r2)


instance MyMonoid (MParser s a) where
  myMempty = MParser (const Nil)


oneOrTwoItems :: MP String
oneOrTwoItems = myMappend (myBind item (\a -> myReturn [a]))
                          (myBind twoItems (\(a, b) -> myReturn [a,b]))

-- FILTERING

filterP :: MP a -> (a -> Bool) -> MP a
filterP m p = myBind m (\a -> if p a then myReturn a
                                     else myMempty)


letter :: MP Char
letter = filterP item isLetter

digit :: MP Int
digit = myBind (filterP item isDigit) (myReturn . digitToInt)

lit :: Char -> MP Char
lit c = filterP item (==c)

-- ITERATION

iterateNP :: Int -> MP a -> MP [a]
iterateNP n p = go n []
    where
        go 0 acc = myReturn acc
        go n acc = myBind p (\a -> go (n-1) (acc++[a]))

iterateP :: MP a -> MP [a]
iterateP m = myMappend (myBind m (\a -> myBind (iterateP m) (\x -> myReturn (a:x))))
                        (myReturn [])


number :: MP Int
number = myBind digit (\a ->
--    myBind (iterateP digit) (\as -> myReturn (asNumber (show a ++ show as))))
    myBind (iterateP digit) (\as -> myReturn (asNumber (a:as))))

asNumber xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))



-- BIASED CHOICE

bChoise :: MP a -> MP a -> MP a
bChoise m n = MParser (\s -> case runParser m s of
                             Nil -> runParser n s
                             otherwise ->  runParser m s)

reiterate :: MP a -> MP [a]
reiterate m = bChoise (myBind m (\a -> myBind (reiterate m) (\x -> myReturn (a:x))))
                      (myReturn [])

numberB :: MP Int
numberB = myBind digit (\a -> myBind (reiterate digit) (\x -> myReturn (asNumber (a:x))))

