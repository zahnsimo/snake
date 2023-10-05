import System.IO
import System.Timeout
import System.Console.ANSI
import System.Random
import Control.Monad.Reader

x_max = 20
y_max = 10
game_mode = (0,0)
-- (0,0)        rectangle
-- (0,1), (1,0) cylinder
-- (1,1)        torus
-- (0,2), (2,0) möbius
-- (1,2), (2,1) klein bottle
-- (2,2)        projective plane

-- data Settings = Settings{
--   x_max :: Int,
--   y_max :: Int,
--   game_mode :: Int
--                         }

data Snake = Snake [(Int,Int)]
  deriving Show

data Food = Food (Maybe (Int,Int))

data WASD = W | A | S | D

data Game = Game{
  prSnake :: Snake,
  prFood  :: Food,
  prKey   :: WASD
                }

wasd_to_dir :: WASD -> (Int,Int)
wasd_to_dir s = case s of
  W -> (0,1)
  A -> (-1,0)
  S -> (0,-1)
  D -> (1,0)   

move_Snake :: Game -> Maybe Game
move_Snake (Game (Snake []) (Food food) key) = Nothing --Game (Snake [(0,0)]) (Food food) key
move_Snake (Game (Snake snake) (Food Nothing) key) = Nothing --Game (Snake []) (Food Nothing) key
move_Snake (Game (Snake snake) (Food (Just food)) key)
  | snake_bite new_head snake = Nothing --Game (Snake []) (Food Nothing) key
  | bonk new_head             = Nothing --Game (Snake []) (Food Nothing) key
  | food_eaten new_head food  = Just $ Game (Snake (new_head : snake)) (Food Nothing) key
  | otherwise                 = Just $ Game (Snake (new_head : init snake)) (Food $ Just food) key
      where old_head = head snake
            dir = wasd_to_dir key
            new_head = (fst old_head + fst dir, snd old_head + snd dir)
            snake_bite h t = h `elem` t
            food_eaten h f = h == f
            bonk h = fst h < 0 || fst h > x_max || snd h < 0 || snd h > y_max

-- wrap_around :: 

spawn_food :: Snake -> IO Food
spawn_food (Snake snake) = do
  rand_x <- randomIO :: IO Int
  let x = rand_x `mod` (x_max + 1)
  rand_y <- randomIO :: IO Int
  let y = rand_y `mod` (y_max + 1)
  if (x,y) `elem` snake then
    spawn_food (Snake snake)
  else  
    return (Food $ Just (x,y))
 
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

drawline :: Game -> Int -> String
drawline (Game (Snake snake) (Food food) key) y | y == (-1) = horizontal
                                                | y == y_max + 1 = horizontal
                                                | otherwise = drawline_game
  where  horizontal = map (\ x -> '-') [-1..x_max+1]
         drawline_game =  "|" ++ map  (\ x -> generate_pixel(x,y)) [0..x_max] ++ "|"
           where generate_pixel (x,y) | not (null snake) && (x,y) == head snake = case key of
                                                                           W -> '^'
                                                                           A -> '<'
                                                                           S -> 'v'
                                                                           D -> '>'                                
                                      | (x,y) `elem` snake = '+'
                                      | Just (x,y) == food = 'O'
                                      | otherwise = ' '

draw :: Game -> IO [()]
draw game = traverse putStrLn $ map (\ y -> drawline game y) [y_max +1, y_max..(-1)]

game_start :: IO Game
game_start = do
   let starter_snake = Snake [(5,0),(4,0),(3,0),(2,0)]
   starter_food <- spawn_food starter_snake
   let starter_key = D
   return $ Game starter_snake starter_food starter_key

main = do
  hSetBuffering stdin NoBuffering
  start <- game_start
  putStrLn "\n\nMove snake with WASD \n"
  mainloop start
  where mainloop game = do
          draw game
          cursorUp (y_max + 3)
          setCursorColumn 0
          input <- timeout 1000000 getWASD 
          next_game <- (check_for_gameover $ move_Snake $ update_Key game input) >>= update_Food
          mainloop next_game

getWASD :: IO WASD
getWASD = do
  key <- getChar
  case key of
   'w' -> return W
   'a' -> return A
   's' -> return S
   'd' -> return D
   _ -> getWASD

update_Key :: Game -> Maybe WASD -> Game
update_Key (Game snake food key) new_key = case new_key of
  Just c -> (Game snake food c)
  Nothing -> (Game snake food key)

update_Food :: Game -> IO Game
update_Food (Game snake food key) = do
  new_food <- case food of
   Food Nothing -> spawn_food snake
   Food (Just food) -> return (Food $ Just food)
  return (Game snake new_food key)

check_for_gameover :: (Maybe Game) -> IO Game
check_for_gameover (Just game) = return game
check_for_gameover Nothing = game_start
