{-# LANGUAGE FlexibleContexts, MultiWayIf #-}

import System.IO
import System.Timeout
import System.Console.ANSI
import System.Random
import Control.Monad.Reader

-- game_modes :
-- (0,0)        rectangle
-- (0,1), (1,0) cylinder
-- (1,1)        torus
-- (0,2), (2,0) möbius
-- (1,2), (2,1) klein bottle
-- (2,2)        projective plane

data Settings = Settings{
  prx_max :: Int,
  pry_max :: Int,
  prgame_mode :: (Int,Int)
                        }

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

move_Snake :: (Monad m, MonadReader Settings m) => Game -> m (Maybe Game)
move_Snake (Game (Snake []) (Food food) key) = return Nothing
move_Snake (Game (Snake snake) (Food Nothing) key) = return Nothing
move_Snake (Game (Snake snake) (Food (Just food)) key) = do
  let old_head = head snake
  let dir = wasd_to_dir key
  let new_head = (fst old_head + fst dir, snd old_head + snd dir)
  let snake_bite h t = h `elem` t
  let food_eaten h f = h == f
  x_max <- asks prx_max
  y_max <- asks pry_max
  let bonk h = fst h < 1 || fst h > x_max || snd h < 1 || snd h > y_max
  if
    | snake_bite new_head snake -> return Nothing
    | bonk new_head             -> do wrapped_head <- wrap_around new_head
                                      if | fst wrapped_head == 0 || snd wrapped_head == 0 -> return Nothing
                                         | food_eaten wrapped_head food -> return $ Just $ Game (Snake (wrapped_head : snake)) (Food Nothing) key
                                         | otherwise -> return $ Just $ Game (Snake (wrapped_head : init snake)) (Food $ Just food) key                                         
    | food_eaten new_head food  -> return $ Just $ Game (Snake (new_head : snake)) (Food Nothing) key
    | otherwise                 -> return $ Just $ Game (Snake (new_head : init snake)) (Food $ Just food) key


--wrap_around :: (Monad m, MonadReader Settings m) => (Int,Int) -> (Int,Int)
wrap_around new_head = do
  Settings x_max y_max mode <- ask
  let x = fst new_head
  let y = snd new_head
  if x `mod` (x_max + 1) == 0 then let x_wrapped = abs (x_max - x) `mod` (x_max + 1)
                                       y_wrapped = y * fst mode `mod` (y_max + 1)
                                   in return (x_wrapped,y_wrapped)
   else let y_wrapped = abs (y_max - y) `mod` (y_max + 1)
            x_wrapped = x * snd mode `mod` (x_max + 1)
         in return (x_wrapped,y_wrapped)

spawn_food :: (Monad m, MonadReader Settings m, MonadIO m) => Snake -> m Food
spawn_food (Snake snake) = do
  x_max <- asks prx_max
  y_max <- asks pry_max
  rand_x <- randomIO
  let x = rand_x `mod` x_max + 1
  rand_y <- randomIO
  let y = rand_y `mod` y_max + 1
  if (x,y) `elem` snake then
    spawn_food (Snake snake)
  else  
    return (Food $ Just (x,y))

------------------------drawing--------------------------------------------

drawline :: (Monad m, MonadReader Settings m) => Game -> Int -> m String
drawline (Game (Snake snake) (Food food) key) y = do
  x_max <- asks prx_max
  y_max <- asks pry_max
  if y == 0 || y == y_max + 1 then return $ horizontal x_max
  else return $ drawline_game x_max
    where  horizontal x_max = map (\ x -> '-') [0..x_max + 1]
           drawline_game x_max =  "|" ++ map  (\ x -> generate_pixel(x,y)) [1..x_max] ++ "|"
             where generate_pixel (x,y) | not (null snake) && (x,y) == head snake = case key of
                                                                           W -> '^'
                                                                           A -> '<'
                                                                           S -> 'v'
                                                                           D -> '>'                                
                                      | (x,y) `elem` snake = '+'
                                      | Just (x,y) == food = 'O'
                                      | otherwise = ' '

draw :: (Monad m, MonadReader Settings m, MonadIO m) => Game -> m [()]
draw game = do
  settings <- ask
  let y_max = pry_max settings
  sequence $ map (\ y -> drawline game y >>= (\ s -> liftIO $ putStrLn s)) [y_max +1, y_max..0]

game_start :: (Monad m, MonadReader Settings m, MonadIO m) => m Game
game_start = do
   let starter_snake = Snake [(5,1),(4,1),(3,1),(2,1)]
   starter_food <- spawn_food starter_snake
   let starter_key = D
   return $ Game starter_snake starter_food starter_key

mainloop :: (Monad m, MonadReader Settings m, MonadIO m) => Game -> m Game
mainloop game = do
  settings <- ask
  let y_max = pry_max settings
  draw game
  liftIO $ cursorUp (y_max + 2)
  liftIO $ setCursorColumn 0
  input <- liftIO $ timeout 1000000 getWASD
--  next_game <- (check_for_gameover =<< move_Snake $ update_Key game input) >>= update_Food
  next_game <- (check_for_gameover $ runReader (move_Snake $ update_Key game input) settings) >>= update_Food
  mainloop next_game


main = do
  hSetBuffering stdin NoBuffering
  let settings = (Settings 20 10 (0,-1) )
  start <- runReaderT game_start settings
  putStrLn "\n\nMove snake with WASD \n"
  runReaderT (mainloop start) settings


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

update_Food :: (Monad m, MonadReader Settings m, MonadIO m) => Game -> m Game
update_Food (Game snake food key) = do
  new_food <- case food of
   Food Nothing -> spawn_food snake
   Food (Just food) -> return (Food $ Just food)
  return (Game snake new_food key)

check_for_gameover :: (Monad m, MonadReader Settings m, MonadIO m) => (Maybe Game) -> m Game
check_for_gameover (Just game) = return game
check_for_gameover Nothing = game_start
