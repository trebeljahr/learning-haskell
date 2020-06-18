import qualified Data.Map as Map 

persons = [("Rico", "21"), ("Ole", "21"), ("Ale", "34")]
personMap = Map.fromList persons

emptyMap = Map.empty
-- withCatAndDog 
-- fromList []
withDog = Map.insert "Dog" "Woof" emptyMap
-- withDog  
-- fromList [("Dog","Woof")]
withCatAndDog = Map.insert "Cat" "Meow" withDog
-- withCatAndDog 
-- fromList [("Cat","Meow"),("Dog","Woof")]

animalSounds = Map.fromList [
    ("Dog", "Woof"), 
    ("Cow", "Moo"), 
    ("Donkey", "IIAAAH"), 
    ("Cat", "Meow"), 
    ("Lion", "Roar"), 
    ("Elephant", "Töröö")
    ]

dogSound = Map.lookup "Dog" animalSounds 
-- Just "Woof"
cowSound = Map.lookup "Cow" animalSounds
-- Just "Moo"
monkeySound = Map.lookup "Monkey" animalSounds
-- Nothing 
monkeySoundExists = Map.member "Monkey" animalSounds
-- monkeySoundExists == False
elephantSoundExists = Map.lookup "Elephant" animalSounds
-- elephantSoundExists == True 
howManyAnimals = Map.size animalSounds
-- howManyAnimals == 6

allAnimalsMakeMeow = Map.map (\(a) -> "Meow") animalSounds
lionSound = Map.lookup "Lion" allAnimalsMakeMeow
-- lionSound == "Meow"

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

