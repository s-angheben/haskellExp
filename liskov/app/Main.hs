module Main where

import qualified MyLib (someFunc)

import Data.Profunctor

data Animal = MkAnimal {attributes :: String}

data Cat = MkCat {animalAttr :: Animal,
                  catAttr    :: String}

printAnimalAtt :: Animal -> IO ()
printAnimalAtt a = putStr "Animal Attributes: " >> print (attributes a)

animalExample = MkAnimal {attributes = "A"}
catExample = MkCat {animalAttr = (MkAnimal "B"),
                    catAttr = "grigio"}

class HasAnimal a where
  extract :: a -> Animal

instance HasAnimal Animal where
  extract = id

instance HasAnimal Cat where
  extract = animalAttr 

printAnimalAtt' :: HasAnimal a => a -> IO ()
printAnimalAtt' = 
  lmap extract printAnimalAtt

main :: IO ()
main = do
  printAnimalAtt' animalExample 
  printAnimalAtt' catExample


-------
-- Animal Attributes: "A"
-- Animal Attributes: "B"
