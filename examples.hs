import RPG hiding (main)

-- Potion run: navigate to a potion, grab it, and heal
prog1 :: Program
prog1 = Program decl
  [ Turn RightDir
  , Move 1
  , Turn UpDir
  , Move 1
  , Grab Potion
  , Use Potion
  ]

-- Combat: grab a sword, fight an enemy, drop the sword after
prog2 :: Program
prog2 = Program decl
  [ Move 3
  , Turn RightDir
  , Move 2
  , Grab Sword
  , Equip Sword
  , Attack Overhand
  , Attack Slash
  , Unequip Sword
  , Drop Sword
  ]

-- Full adventure: collect items, equip gear, fight, heal, explore
prog3 :: Program
prog3 = Program decl
  [ Turn RightDir
  , Move 1
  , Turn UpDir
  , Move 1
  , Grab Potion
  , Turn RightDir
  , Move 1
  , Turn UpDir
  , Move 2
  , Grab Sword
  , Equip Sword
  , Attack Overhand
  , Attack Overhand
  , Use Potion
  , Turn RightDir
  , Move 2
  , Turn DownDir
  , Move 1
  ]

main :: IO ()
main = do
  putStrLn "=== Program 1: Potion Run ==="
  print prog1
  putStrLn "\nResult:"
  print (runProgram prog1)

  putStrLn "\n=== Program 2: Combat ==="
  print prog2
  putStrLn "\nResult:"
  print (runProgram prog2)

  putStrLn "\n=== Program 3: Full Adventure ==="
  print prog3
  putStrLn "\nResult:"
  print (runProgram prog3)
