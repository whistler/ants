module Main where

import Data.List
import Data.Maybe (mapMaybe)
import System.IO
import System.Random

import Ants

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

{- |
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}

do_move_location :: GameState -> Point -> Point -> [Order] ->Bool
do_move_location gs loc des orders = do_move_location_help gs loc des orders (directions loc des)

directions :: Point -> Point -> [Direction]
directions loc des
	|  row des - row loc > 0 && col des - col loc > 0 = [North, East]
	|  row des - row loc > 0 && col des - col loc < 0 = [South, East]
	|  row des - row loc < 0 && col des - col loc > 0 = [North, West]
	|  row des - row loc < 0 && col des - col loc < 0 = [South, West]
	|  row des - row loc == 0 && col des - col loc > 0 = [East]
	|  row des - row loc ==0 && col des - col loc < 0 = [West]
	|  row des - row loc > 0 && col des - col loc == 0 = [South]
	|  row des - row loc < 0 && col des - col loc == 0 = [North]

do_move_location_help :: GameState ->Point -> Point ->[Order]->[Direction] -> Bool
do_move_location_help gs loc des orders []  = False
do_move_location_help gs loc des orders (dir:directionls) = 
	if do_move_direction gs loc dir orders
		then True
       else do_move_location_help gs loc des orders directionls 

ant_dist::GameState ->GameParams->[(Int,Point,Point)]
ant_dist gs gp = sort [((distance gp des loc),des,loc)|des<-(food gs),loc<-myantlist (ants gs)]

myantlist::[Ant]->[Point]
myantlist list = [pointAnt p| p<-myAnts list]


gatherfood::GameState->[(Int,Point,Point)]->[(Point,Point)]->[(Point,Point)]
gatherfood gs  [] targets = targets
gatherfood gs  ((dis,des,loc) : xs) targets =  
                    if (elem (des,loc) targets)
                         then gatherfood gs xs targets
                    else
		       if (do_move_location gs loc des (pltoOrder targets))  
                       then gatherfood gs xs ((des,loc): targets)
		       else gatherfood gs xs targets


--main function to find food by choosing any direction towards the food                  
gogatherfood::GameState->GameParams->[Order]
gogatherfood gs gp = [toOrder loc (head(directions loc des)) |(des,loc) <- (gatherfood gs (ant_dist gs gp) [])]

--generaterest of myantslist that hasn't been assigned task to
restants::[Order]->[Ant]->[Ant]
restants orders ants = [ant | ant<-ants,order<-orders,ant /= (ant order)]

--translate [(des,loc)] to [Order]
pltoOrder::[(Point,Point)]->[Order]
pltoOrder [] = []
pltoOrder list = [toOrder loc (head(directions (loc des))) |(des,loc) <-list]


alldirectioncombination::[Point]->[(Point,Direction)]
alldirectioncombination antslist = [(loc,dir)|loc<-antslist,dir<-[North,East,South,West]]

--main default move fuction
domove::GameState-> [Order]
domove gs  = domovehelper gs (alldirectioncombination (myantlist (ants gs))) []


domovehelper::GameState->[(Point,Direction)]->[Order]-> [Order]
domovehelper gs [] orders = orders
domovehelper gs ((loc,dir):xs) orders = if do_move_direction gs loc dir orders
                                        then domovehelper gs xs ((toOrder loc dir):orders)                        
                                        else  domovehelper gs xs orders
                             
toOrder :: Point -> Direction -> Order
toOrder loc dir = let location = loc
		      dir1 = dir 
		  in Order {ant = (let p = location
                                       o = Me 
                                   in Ant {pointAnt = p,ownerAnt = o}), directionOrder = dir1}

--check if we colides with previous orders
do_move_direction :: GameState -> Point -> Direction -> [Order]-> Bool
do_move_direction gs loc dir [] = False
do_move_direction gs loc dir (order:orders) = 
	if (unoccupied (world gs) order) && (not (elem (move dir loc ) [move (directionOrder order1) (pointAnt (ant order1)) | order1 <- orders]))
		then True else False
      

-- gives a list of my ants on my hills      
-- ants_on_hill :: [Hill] -> [Ant] -> [Order] -> [Ant]
-- ants_on_hill myhills myants orders = [ y | x <- myhills, y <- myants, z <- orders, (pointAnt y)==(pointHill x), (ant z)/=y]

-- move ant to an empty location
--move_ant_to_empty_location :: Ant -> Order
  
-- move any of myants on myhill off the hill
--get_ants_off_hill :: GameState -> [Order]
--get_ants_off_hill gs =
    
  --where
    --ants_to_move = ants_on_hill (myHills(hills GameState), myAnts(ants GameState))

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- orders = domove gs
  -- generate orders for all ants belonging to me
  --let generatedOrders = map generateOrders $ myAnts $ ants gs
  -- for each ant take the first "passable" order, if one exists
    --  orders = mapMaybe (tryOrder (world gs)) generatedOrders
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return (domove gs) --orders

-- | This runs the game
main :: IO ()
main = game doTurn



