applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  

decideOver x = if x > 2 then Just x else Nothing
x = Just 3 `applyMaybe` decideOver
-- x ==> Just 3
y = Just 1 `applyMaybe` decideOver
-- y ==> Nothing
z = Nothing `applyMaybe` decideOver
-- z ==> Nothing 


-- the Monad Class 
-- class Monad m where  
--     return :: a -> m a  
  
--     (>>=) :: m a -> (a -> m b) -> m b  
  
--     (>>) :: m a -> m b -> m b  
--     x >> y = x >>= \_ -> y  
  
--     fail :: String -> m a  
--     fail msg = error msg  

type Birds = Int  
type Pole = (Birds,Birds)  

-- defining the infix syntax 
x -: f = f x 

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right) 
 | abs ((left + n) - right) < 4 = Just (left + n, right)
 | otherwise = Nothing 
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right) 
 | abs (left - (right + n))  < 4 = Just (left, right + n)
 | otherwise = Nothing 


-- return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
-- ==> Just (2,4)

banana :: Pole -> Maybe Pole  
banana _ = Nothing  

-- return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- ==> Nothing 
-- which is the same as 
-- return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- ==> Nothing 

lameDoStuff :: Maybe String 
lameDoStuff = Just "The sum is " >>= (\a -> 
              Just 5 >>= (\b ->
              Just 10 >>= (\c -> 
              Just "!" >>= (\d -> 
                  Just (a ++ (show $ b + c) ++ d)
                  )))) 

-- everything inside do is a monad -> mind blown.
-- lameDoStuff and epicDoStuff are functionally equivalent. 
-- the epic version is just so much cleaner.
epicDoStuff :: Maybe String 
epicDoStuff = do 
    a <- Just "The sum is "
    b <- Just 5
    c <- Just 10
    d <- Just "!"
    Just (a ++ (show $ b + c) ++ d)

poleWalkWithBirds :: Maybe Pole 
poleWalkWithBirds = do 
    start <- return (0,0)
    first <- landLeft 2 start 
    second <- landRight 4 first 
    third <- landRight 4 second
    landLeft 2 third 

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [3,4,5]  
    ch <- ['x','y','z']  
    return (n,ch) 

greetEveryone :: [String] -> [String]
greetEveryone names = do
    name <- names 
    greet <- ["Hello "]
    return (greet ++ name)
-- greetEveryone ["Paul", "Ole", "Verena"]
-- ==> ["Hello Paul", "Hello Ole", "Hello Verena"]

-- list comprehensions are also just monads. wtf. 
-- [ (n,ch) | n <- [3,4,5], ch <- ['x','y', 'z'] ] == listOfTuples
-- ==> True