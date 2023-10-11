{-# LANGUAGE FlexibleContexts, MultiWayIf #-}

import System.IO
import System.Timeout
import System.Console.ANSI
import System.Random
import Control.Monad.Reader
import Control.Monad.State.Class
import Text.Read


data Settings = Settings{
  prx_max :: Int,
  pry_max :: Int,
  prgame_mode :: (Int,Int)
                        }
-- game_modes :
-- (0,0)        rectangle
-- (0,1), (1,0) cylinder
-- (1,1)        torus
-- (0,2), (2,0) möbius
-- (1,2), (2,1) klein bottle
-- (2,2)        projective plane

data Snake = Snake [(Int,Int)]
  deriving Show

data Food = Food (Maybe (Int,Int))

data WASD = W | A | S | D

data Game = Game{
  prSnake :: Snake,
  prFood  :: Food,
  prKey   :: WASD
                }

data Score = Score Int

--------------------movement of the snake------------------------------

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
  settings <- ask
  let old_head = head snake
  let dir = wasd_to_dir key
  new_head <- new_head_position old_head dir
  let snake_bite h t = h `elem` t
  let food_eaten h f = h == f
  if| runReader (out_of_bounds new_head) settings -> return Nothing
    | snake_bite new_head snake -> return Nothing                                       
    | food_eaten new_head food  -> return $ Just $ Game (Snake (new_head : snake)) (Food Nothing) key
    | otherwise                 -> return $ Just $ Game (Snake (new_head : init snake)) (Food $ Just food) key

new_head_position :: (Monad m, MonadReader Settings m) => (Int,Int) -> (Int,Int) -> m (Int,Int)
new_head_position old_head dir = do
  let new_head_shadow = (fst old_head + fst dir, snd old_head + snd dir)
  settings <- ask
  let x_max = prx_max settings
  let y_max = pry_max settings
  let bonk h = fst h < 1 || fst h > x_max || snd h < 1 || snd h > y_max
  let new_head | bonk new_head_shadow = runReader (wrap_around new_head_shadow) settings
               | otherwise            = new_head_shadow
  return new_head

wrap_around :: (Monad m, MonadReader Settings m) => (Int,Int) -> m (Int,Int)
wrap_around (x,y) = do
  Settings x_max y_max mode <- ask
  let (x_wrapped, y_wrapped) | x `mod` (x_max + 1) == 0
        = (abs (x_max - x) `mod` (x_max + 1), y * fst mode `mod` (y_max + 1))
                             | otherwise
        = (x * snd mode `mod` (x_max + 1), abs (y_max - y) `mod` (y_max + 1))
  return (x_wrapped, y_wrapped)

out_of_bounds :: (Monad m, MonadReader Settings m) => (Int,Int) -> m Bool
out_of_bounds (x,y) = do
  x_max <- asks prx_max
  y_max <- asks pry_max
  return (x_max <= 0 || x > x_max || y <= 0 || y > y_max)
  
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

-----------------settings for the game----------------

game_start :: (Monad m, MonadReader Settings m, MonadIO m) => m Game
game_start = do
   let starter_snake = Snake [(5,1),(4,1),(3,1),(2,1)]
   starter_food <- spawn_food starter_snake
   let starter_key = D
   return $ Game starter_snake starter_food starter_key

getInt :: IO Int
getInt = do
  s <- getLine
  let x = readMaybe s :: Maybe Int
  case x of
    Just y -> return y
    Nothing -> do putStrLn "Enter an Int"
                  getInt

getInt_withConstraints :: Int -> Int -> IO Int
getInt_withConstraints minVal maxVal = do
  x <- getInt
  if x >= minVal && x <= maxVal then return x
    else do putStrLn $ "Please enter value between " ++ show minVal ++ " and " ++ show maxVal
            getInt_withConstraints minVal maxVal

getSettings :: IO Settings
getSettings = do
  putStrLn "Set x_max"
  x_max <- getInt_withConstraints 5 50
  putStrLn "Set y_max"
  y_max <- getInt_withConstraints 5 50
  putStrLn "Set game_mode (2 x Int)"
  m1 <- getInt_withConstraints (-1) 1
  m2 <- getInt_withConstraints (-1) 1
  return $ Settings x_max y_max (m1,m2)


-----------------updates and checks during game------------------------

update_Key :: Game -> Maybe WASD -> Game
update_Key (Game snake food key) new_key = case new_key of
  Just c -> (Game snake food c)
  Nothing -> (Game snake food key)

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

update_Food :: (Monad m, MonadReader Settings m, MonadIO m) => Game -> m Game
update_Food (Game snake food key) = do
  new_food <- case food of
   Food Nothing -> spawn_food snake
   Food (Just food) -> return (Food $ Just food)
  return (Game snake new_food key)

check_for_gameover :: (Monad m, MonadReader Settings m, MonadIO m) => (Maybe Game) -> m Game
check_for_gameover (Just game) = return game
check_for_gameover Nothing = game_start

-----------------actual game-------------------------------------------------

getWASD :: IO WASD
getWASD = do
  key <- getChar
  case key of
   'w' -> return W
   'a' -> return A
   's' -> return S
   'd' -> return D
   _ -> getWASD

mainloop :: (Monad m, MonadReader Settings m, MonadIO m) => Game -> m Game
mainloop game = do
  settings <- ask
  let y_max = pry_max settings
  draw game
  liftIO $ cursorUp (y_max + 2)
  liftIO $ setCursorColumn 0
  input <- liftIO $ timeout 1000000 getWASD
--  next_game <- (check_for_gameover =<< move_Snake $ update_Key game input) >>= update_Food
  let test_game = runReader (move_Snake $ update_Key game input) settings
  next_game <- (check_for_gameover $ runReader (move_Snake $ update_Key game input) settings) >>= update_Food
  mainloop next_game


main = do
  hSetBuffering stdin NoBuffering
  --let settings = (Settings 20 10 (0,-1) )
  settings <- getSettings
  start <- runReaderT game_start settings
  putStrLn "\n\nMove snake with WASD \n"
  runReaderT (mainloop start) settings
