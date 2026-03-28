import RPG

-- Example Program 1
-- move left
-- attack slash
-- grab sword

prog1 :: Program
prog1 = Program (Cons (Move LeftDir) (Cons (Attack Slash) (Single (Grab Sword))))

-- Example Program 2
-- dodge right
-- use potion

prog2 :: Program
prog2 = Program (Cons (Dodge RightDir) (Single (Use Potion)))

-- Example Program 3
-- equip armor
-- attack overhand

prog3 :: Program
prog3 = Program (Cons (Equip Armor) (Single (Attack Overhand)))

main :: IO ()
main = do
  print prog1
  print prog2
  print prog3