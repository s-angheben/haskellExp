Example of contravariant type

In OOP when if we have 2 classes:\
Animal\
Cat extends Animal\

and we define a function\
fun (Animal a) \
we can call it with an Animal or Cat instance

If we try to export this concepts in Haskell and we can maybe consider Animal and Cat as type, in particular Cat contains the type Animal.
Then we can create an haskell type class HasAnimal that indicate if the type contains the "Animal part", and extract it. Ideally we could create an instance of the class HasAnimal for the Cat and the Animal at compile time with template haskell. 
Now if we have a function  \
f (Animal a) \
we could make it works with every type that has an instance of HasAnimal with \
lmap extract f \

The idea is to use the contravarinat property of the arrow operation (-> a b). The one on the first argument, on the type a in this case.


The goal of this example is **only to illustrate the contravariant property**, otherwise this is consider an antipatter in haskell since there is no reason to make a typeclass in that way and not convert directly the Cat in Animal.
