data Person = Person {firstName :: String  , lastName :: String, age :: Int, occupation :: Float, gender :: String} deriving (Show) 

data Animal = Animal {mainClass :: String, name::String, order :: String, family :: String, genus :: String, species :: String} deriving (Show)

fox = Animal {
    mainClass="Mammalia", 
    order="Carnivora", 
    family="Canidae", 
    genus="Vulpes", 
    species="Vulpes vulpes",
    name="Fox"
    }

printPhylo :: Animal -> String
printPhylo (Animal {
    mainClass = mainClass, 
    name = name, 
    order = order, 
    family = family, 
    genus = genus, 
    species = species})= 
    ("The animal named " ++ name ++ " belongs to the class " ++ 
    mainClass ++ ", of the order " ++ order ++ 
    ", the family " ++ family ++ " and the genus " ++ 
    genus ++ ".")