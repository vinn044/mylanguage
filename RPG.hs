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
  deriving (Show)


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
  show (Player n items Nothing dir pos) =
          n ++ ":\n"
          ++ "items: {" ++ intercalate ", " (map show items) ++ "}\n"
          ++ "direction: " ++ show dir ++ "\n"
          ++ "position: " ++ show pos
  show (Player n items (Just itm) dir pos) =
          n ++ ":\n"
          ++ "items: {" ++ intercalate ", " (map show items) ++ "}\n"
          ++ "equipped: " ++ show itm ++ "\n"
          ++ "direction: " ++ show dir ++ "\n"
          ++ "position: " ++ show pos


-- FIXED HERE: added deriving (Show)
data GameBoard = GameBoard {
  width :: Int,
  height :: Int,
  boardItems :: [(Location, Item)]
} deriving (Show)


data Env = Env {
  player :: Player,
  board :: GameBoard
} deriving (Show)


initialPlayer = Player "Vince" [] Nothing UpDir (0,0)

initialBoard = GameBoard 10 5 [((2,3), Sword), ((1,1), Potion), ((4,6), Sword)]

decl = (initialBoard, initialPlayer)

p1 = Program decl [Move 3, Turn LeftDir]


evalCommand (Turn dir) (Env p b) =
  Env (p {direction = dir}) b