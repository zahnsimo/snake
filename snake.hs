import System.IO
import System.Timeout
import System.Console.ANSI
import System.Random

x_max = 20
y_max = 10

data Snake = Snake [(Int,Int)]
  deriving Show

data WASD = W | A | S | D

data Food = Food (Int,Int)

new_snake :: Snake -> Food -> (Int,Int) -> (Snake, Maybe Food)
new_snake (Snake []) _ dir = (Snake [(0,0)], Nothing)
new_snake (Snake snake) (Food food) dir | snake_bite new_head snake = (Snake [], Nothing) 
                                        | bonk new_head             = (Snake [], Nothing)
                                        | food_eaten new_head food  = (Snake (new_head : snake), Nothing)
                                        | otherwise                 = (Snake (new_head : init snake), Just $ Food food)
                               where old_head = head snake
                                     new_head = (fst old_head + fst dir, snd old_head + snd dir)
                                     snake_bite h t = h `elem` t
                                     food_eaten h f = h == f
                                     bonk h = fst h < 0 || fst h > x_max || snd h < 0 || snd h > y_max

spawn_food :: Snake -> IO Food
spawn_food (Snake snake) = do
  rand_x <- randomIO :: IO Int
  let x = rand_x `mod` (x_max + 1)
  rand_y <- randomIO :: IO Int
  let y = rand_y `mod` (y_max + 1)
  if (x,y) `elem` snake then
    spawn_food (Snake snake)
  else  
    return (Food (x,y))
 
wasd_to_dir :: WASD -> (Int,Int)
wasd_to_dir s = case s of
  W -> (0,1)
  A -> (-1,0)
  S -> (0,-1)
  D -> (1,0)

--snake_crawls :: [(Int,Int)] -> String -> IO [(Int,Int)]
--snake_crawls snake inputs = foldl new_snake snake dirs
--                            where dirs = map wasd_to_dir inputs


-- snake_crawls :: Snake -> [WASD] -> IO Snake
-- snake_crawls snake (d:ds) = do
--   let new = new_snake snake (wasd_to_dir d)
--   --print new
--   draw new
--   print ""
--   snake_crawls new ds
-- snake_crawls snake [] = return snake


bools_to_pixel :: (Bool,Bool) -> Char
bools_to_pixel b = case b of
          (True , _) -> '#'
          (False , True) -> 'O'
          (False, False) -> '.'

drawline :: Snake -> Food -> Int -> String
drawline (Snake snake) (Food food) y =  map  (\ x -> bools_to_pixel $ ( ( (x,y) `elem` snake) , ((x,y) == food) )) [0..x_max]

draw :: Snake -> Food -> IO [()] --[String]
draw snake food = traverse putStrLn $ map (\ y -> drawline snake food y) [y_max, y_max-1..0]

main = do
  hSetBuffering stdin NoBuffering
  let starter = Snake [(5,0),(4,0),(3,0),(2,0)]
  starter_food <- spawn_food starter
  putStrLn "\n\nMove snake with WASD \n"
  draw starter starter_food
  let starter_dir = D
  mainloop starter starter_food starter_dir
  where mainloop snake food dir = do
          input <- timeout 1000000 getWASD
          let next_dir = case input of
                Just c -> c
                Nothing -> dir
          let (new, mfood) = new_snake snake food (wasd_to_dir next_dir)
          cursorUp (y_max + 1)
          setCursorColumn 0
          draw new food
          new_food <- case mfood of
            Nothing -> spawn_food new
            (Just food) ->return food
          mainloop new new_food next_dir

getWASD :: IO WASD
getWASD = do
  key <- getChar
  case key of
   'w' -> return W
   'a' -> return A
   's' -> return S
   'd' -> return D
   _ -> getWASD
