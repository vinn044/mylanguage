data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show)

data Command = Move Direction
  deriving (Show)