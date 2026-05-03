module RPG where

import Data.List (intercalate)

data Direction = LeftDir | RightDir | UpDir | DownDir

instance Show Direction where
  show LeftDir = "left"
  show RightDir = "right"
  show UpDir = "up"
  show DownDir = "down"


data AttackType = Overhand | Slash

instance Show AttackType where
  show Overhand = "overhand"
  show Slash = "slash"


data Item = Potion | Sword | Armor
  deriving (Eq)

instance Show Item where
  show Potion = "Potion"
  show Sword  = "Sword"
  show Armor  = "Armor"

data Command = Move Int
  | Turn Direction
  | Grab Item
  | Drop Item
  | Use Item
  | Equip Item
  | Unequip Item
  | Attack AttackType

instance Show Command where
  show (Move i) = "move: " ++ show i
  show (Turn dir) = "turn: " ++ show dir
  show (Grab item) = "grab: " ++ show item
  show (Drop item) = "drop: " ++ show item
  show (Use item) = "use: " ++ show item
  show (Equip item) = "equip: " ++ show item
  show (Unequip item) = "unequip: " ++ show item
  show (Attack atk) = "attack: " ++ show atk


data Program = Program Decl [Command]

instance Show Program where
  show (Program (b, p) cmds) =
    "=== Program ===\n"
    ++ show b ++ "\n"
    ++ show p ++ "\n"
    ++ "Commands: " ++ intercalate ", " (map show cmds)


type Location = (Int, Int)
type Decl = (GameBoard, Player)


data Player = Player {
  name :: String,
  hp :: Int,
  items :: [Item],
  equipped :: Maybe Item,
  direction :: Direction,
  position :: Location
}

instance Show Player where
  show (Player n h items Nothing dir pos) =
          n ++ ":\n"
          ++ "  hp: " ++ show h ++ "\n"
          ++ "  items: {" ++ intercalate ", " (map show items) ++ "}\n"
          ++ "  direction: " ++ show dir ++ "\n"
          ++ "  position: " ++ show pos
  show (Player n h items (Just itm) dir pos) =
          n ++ ":\n"
          ++ "  hp: " ++ show h ++ "\n"
          ++ "  items: {" ++ intercalate ", " (map show items) ++ "}\n"
          ++ "  equipped: " ++ show itm ++ "\n"
          ++ "  direction: " ++ show dir ++ "\n"
          ++ "  position: " ++ show pos


data GameBoard = GameBoard {
  width :: Int,
  height :: Int,
  boardItems :: [(Location, Item)],
  enemies :: [(Location, Int)]
}

instance Show GameBoard where
  show b =
    "Board: " ++ show (width b) ++ " x " ++ show (height b) ++ "\n"
    ++ "  Items: " ++ showBoardItems (boardItems b) ++ "\n"
    ++ "  Enemies: " ++ showEnemies (enemies b)
    where
      showBoardItems [] = "none"
      showBoardItems is = intercalate ", " (map (\(loc, item) -> show item ++ " at " ++ show loc) is)
      showEnemies [] = "none"
      showEnemies es = intercalate ", " (map (\(loc, ehp) -> "enemy at " ++ show loc ++ " (hp: " ++ show ehp ++ ")") es)


data Env = Env {
  player :: Player,
  board :: GameBoard
}

instance Show Env where
  show (Env p b) =
    "=== Game State ===\n"
    ++ show b ++ "\n"
    ++ show p


initialPlayer :: Player
initialPlayer = Player "Vince" 80 [] Nothing UpDir (0,0)

initialBoard :: GameBoard
initialBoard = GameBoard 10 5
  [((2,3), Sword), ((1,1), Potion), ((4,4), Armor)]
  [((2,3), 30), ((5,2), 50)]

decl :: Decl
decl = (initialBoard, initialPlayer)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x:xs)
  | f x       = xs
  | otherwise = x : removeFirst f xs


clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)


evalCommand :: Command -> Env -> Env

evalCommand (Turn dir) (Env p b) =
  Env (p { direction = dir }) b

evalCommand (Move i) (Env p b) =
  let (x, y) = position p
      (newX, newY) = case direction p of
        LeftDir  -> (x - i, y)
        RightDir -> (x + i, y)
        UpDir    -> (x, y + i)
        DownDir  -> (x, y - i)
      clampedPos = (clamp 0 (width b - 1) newX, clamp 0 (height b - 1) newY)
  in Env (p { position = clampedPos }) b

evalCommand (Grab item) (Env p b) =
  let pos = position p
      found = any (\(loc, itm) -> loc == pos && itm == item) (boardItems b)
  in if found
     then Env (p { items = item : items p })
              (b { boardItems = removeFirst (\(loc, itm) -> loc == pos && itm == item) (boardItems b) })
     else Env p b

evalCommand (Drop item) (Env p b) =
  if item `elem` items p
  then Env (p { items = removeFirst (== item) (items p) })
           (b { boardItems = (position p, item) : boardItems b })
  else Env p b


evalCommand (Use Potion) (Env p b) =
  if Potion `elem` items p
  then Env (p { hp = min 100 (hp p + 20), items = removeFirst (== Potion) (items p) }) b
  else Env p b

evalCommand (Use _) (Env p b) =
  Env p b

evalCommand (Equip item) (Env p b) =
  if item `elem` items p
  then Env (p { equipped = Just item, items = removeFirst (== item) (items p) }) b
  else Env p b

evalCommand (Unequip _) (Env p b) =
  case equipped p of
    Nothing  -> Env p b
    Just itm -> Env (p { equipped = Nothing, items = itm : items p }) b

evalCommand (Attack atk) (Env p b) =
  case equipped p of
    Just Sword ->
      let pos = position p
          dmg = case atk of
            Overhand -> 20
            Slash    -> 10
          enemyHere = any (\(loc, _) -> loc == pos) (enemies b)
          retaliation = if enemyHere then (if Armor `elem` items p then 2 else 5) else 0
          newEnemies = filter (\(_, ehp) -> ehp > 0)
                       (map (\(loc, ehp) -> if loc == pos then (loc, ehp - dmg) else (loc, ehp)) (enemies b))
      in Env (p { hp = hp p - retaliation }) (b { enemies = newEnemies })
    _ -> Env p b


runProgram :: Program -> Env
runProgram (Program (b, p) cmds) =
  foldl (\env cmd -> evalCommand cmd env) (Env p b) cmds
