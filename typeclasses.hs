import qualified BinaryTree as B

-- class Eq elem where 
--     (==) :: elem -> elem -> Bool
--     (/=) :: elem -> elem -> Bool 
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green 
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)


instance Eq TrafficLight where 
    Red == Red = True 
    Green == Green = True 
    Yellow == Yellow = True 
    _ == _ = False 

instance Show TrafficLight where 
    show Red = "The Red Light is on!"
    show Yellow = "The Yellow Light is on!"
    show Green = "The Green Light is on!"

data KittensAndDogs = Dogs | Kittens 

instance Show KittensAndDogs where 
    show Dogs = "Dogs suck!"
    show Kittens = "Adorable kittens!"

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True  

instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True 

instance YesNo Bool where  
    yesno = id 

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False 

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True  

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True  

instance YesNo KittensAndDogs where 
    yesno Dogs = False 
    yesno _ = True 

instance Functor B.Tree where  
    fmap f B.EmptyTree = B.EmptyTree  
    fmap f (B.Node x leftsub rightsub) = B.Node (f x) (fmap f leftsub) (fmap f rightsub)  
