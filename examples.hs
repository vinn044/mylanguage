import RPG

prog1 :: Program
prog1 = Program decl [Move 3, Attack Slash, Grab Sword]

prog2 :: Program
prog2 = Program decl [Turn RightDir, Use Potion]

prog3 :: Program
prog3 = Program decl [Equip Armor, Attack Overhand]

main :: IO ()
main = do
  print prog1
  print prog2
  print prog3