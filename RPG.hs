module RPG where
import Data.List (intercalate)
data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show)

data AttackType = Overhand | Slash
  deriving (Show)


data Item = Potion | Sword | Armor 
  deriving (Show)

data Command = Move Int
  | Dodge Direction
  | Grab Item
  | Drop Item
  | Use Item
  | Equip Item
  | Unequip Item
  | Attack AttackType
 -- deriving (Show)
instance Show Command where
  show (Move i) = "move: " ++ show i
  show (Dodge dir) = "dodge: " ++ show dir
  show _ = undefined

data CommandList
  = Single Command
  | Cons Command CommandList
  deriving (Show)

-- data Maybe a = Nothing | Just a
convert :: [Command] -> CommandList
convert [c] = Single c
convert (c : cs) = Cons c (convert cs)

data Program = Program Decl CommandList
  deriving (Show)

type Location = (Int, Int)
type Decl = (GameBoard, Player)

data Player = Player {
  name :: String,
  items :: [Item],
  equipped :: Maybe Item,
  direction :: Direction,
  position :: Location
}

instance Show Player where
  show (Player n items Nothing dir pos) = show n ++ ":\n"
          ++ "items: {" ++ intercalate ", " (map show items) ++ "}" ++ "\n"
          ++ "direction: " ++ show dir ++ "\n"
          ++ "position: " ++ show pos
  show (Player n items (Just itm) dir pos) = show n ++ ":\n"
          ++ "items: {" ++ intercalate ", " (map show items) ++ "}" ++ "\n"
          ++ "equipped: " ++ show itm ++ "\n"
          ++ "direction: " ++ show dir ++ "\n"
          ++ "position: " ++ show pos


data GameBoard = GameBoard {
  width :: Int,
  height :: Int,
  items :: [(Location, Item)]
}

data Env = Env {
  player :: Player,
  board :: GameBoard
}

initialPlayer = Player "Vince" [] Nothing UpDir (0,0)
initialBoard = GameBoard 10 5 [((2,3), Sword), ((1,1), Potion), ((4,6), Sword)]
decl = (initialBoard, initialPlayer)

p1 = Program decl (convert [Move 3, Dodge LeftDir])