import qualified Data.Map as Map

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

data ThreeDVector a = ThreeDVector a a a deriving (Show)  
  
vectPlus :: (Num t) => ThreeDVector t -> ThreeDVector t -> ThreeDVector t  
(ThreeDVector i j k) `vectPlus` (ThreeDVector l m n) = ThreeDVector (i+l) (j+m) (k+n)  
  
scalarMult :: (Num t) => ThreeDVector t -> t -> ThreeDVector t  
(ThreeDVector i j k) `scalarMult` m = ThreeDVector (i*m) (j*m) (k*m)  
  
dotProduct :: (Num t) => ThreeDVector t -> ThreeDVector t -> t  
(ThreeDVector i j k) `dotProduct` (ThreeDVector l m n) = i*l + j*m + k*n  

data Month = January | February | March | April | Mai | June | July | August | September | October | November | December 
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- January == February 
-- False
-- January == January 
-- True
-- March > April 
-- False 
-- June < December
-- True 

-- succ January 
-- February

allTheMonths = [January .. December]
allTheMonths' = [minBound .. maxBound] :: [Month]

type Name = String
type Email = String
type SuccessMessage = String

persons = Map.fromList [("Rico", "mail@ricotrebeljahr.com")]

sendEmailTo :: Name -> SuccessMessage  
sendEmailTo name = 
    if Map.lookup name persons == Nothing 
    then "You do not know this person." 
    else "Success"


-- Locker Example 
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code 
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
            Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
            Just (state, code) -> if state /= Taken 
                                    then Right $ "The locker has the code: " ++ code
                                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

-- recursive type definition for a list.
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

data List' a = Empty' | Cons' { listHead::a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- quadtree example 
data Point = Point Float Float deriving (Show, Read, Eq, Ord)
type Width = Float 
type Height = Float 
data Boundary = Boundary Point Width Height deriving (Show, Read, Eq, Ord)
data QuadTree a = EmptyQuadTree | QuadNode a Boundary (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) deriving (Show, Read, Eq, Ord)

middle :: Point -> Point -> Point 
middle (Point x1 y1) (Point x2 y2) = Point ((x2 + x1) / 2) ((y2 + x1) / 2)

northWest :: Boundary -> Boundary
northWest (Boundary (Point x1 y1) w h) = 
    let newPoint = Point x1 y1 
    in Boundary (newPoint) (w/2) (h/2)

northEast :: Boundary -> Boundary
northEast (Boundary (Point x1 y1) w h) = 
    let newPoint = Point (x1 + w/2) (y1)  
    in Boundary (newPoint) (w/2) (h/2)

southWest :: Boundary -> Boundary
southWest (Boundary (Point x1 y1) w h) = 
    let newPoint = Point x1 (y1 + h/2)
    in Boundary (newPoint) (w/2) (h/2)

southEast :: Boundary -> Boundary
southEast (Boundary (Point x1 y1) w h) = 
    let newPoint = Point (x1 + w/2) (y1 + h/2)
    in Boundary (newPoint) (w/2) (h/2)
 

-- between :: Int 
-- quadInsert :: Int -> Int -> QuadTree data -> QuadTree data 
-- quadInsert x y (EmptyQuadTree (Boundary x1 x2 y1 y2)) 
--     | northEast 
--     | northWest 
--     | southEast
--     | southWest
--     where northEast = Boundary
--           northWest = 
--           southEast = 
--           southWest = 
-- quadInsert data boundary 
--     | 
--     | 
--     | 

quadNode :: a -> Boundary -> QuadTree a
quadNode a boundary = QuadNode a boundary EmptyQuadTree EmptyQuadTree EmptyQuadTree EmptyQuadTree 

-- quadLookup :: Int -> Int -> QuadTree data -> Bool 
-- quadLookup x y EmptyQuadTree = False 
-- quadLookup x y (QuadNode data x2 y2 (northeast northwest southeast southwest))
--     | x == x2 && y = y2 
--     | 
--     | 