data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show)

data Command = 
    Move Direction
  | Dodge Direction
  | Grab Item
  | Drop Item
  | Use Item
  | Equip Item
  | Unequip Item
  deriving (Show)

data Item = Potion | Sword | Armor 
  deriving (Show)

