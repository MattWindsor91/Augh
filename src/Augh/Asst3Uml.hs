module Asst3Uml where
import Uml
import UmlTypes
import Graphviz
import IO

uml = [ClassUml
       (Concrete
        (Class "Command"
         [Field Private ("argument" :- "String")]
         [Method Public "getCommand" [] "CommandWord"])),
       ClassUml
       ("Enum" :>> Concrete
        (Class "CommandWord"
         [ Field Public ("ATTACK" :- "CommandWord"),
           Field Public ("DROP" :- "CommandWord"),
           Field Public ("GET" :- "CommandWord"),
           Field Public ("HELP" :- "CommandWord"),
           Field Public ("INVENTORY" :- "CommandWord"),
           Field Public ("LOAD" :- "CommandWord"),
           Field Public ("LOOK" :- "CommandWord"),
           Field Public ("MOVE" :- "CommandWord"),
           Field Public ("QUIT" :- "CommandWord"),
           Field Public ("RANDOM" :- "CommandWord"),
           Field Public ("SAVE" :- "CommandWord"),
           Field Public ("SCRIPT" :- "CommandWord"),
           Field Public ("TALK" :- "CommandWord"),
           Field Public ("UNDO" :- "CommandWord") ]
         []
        )
       ),
       ClassUml (Concrete (Class "Conversation" [] [])),
       ClassUml (Concrete (Class "ConversationLoader" [] [])),
       ClassUml (Concrete (Class "Direction" [] [])),
       ClassUml (Concrete (Class "Game" [] [])),
       ClassUml (Concrete (Class "GameCharacter" [] [])),
       ClassUml (Concrete (Class "Item" [] [])),
       ClassUml (Concrete (Class "Parser" [] [])),
       ClassUml (Concrete (Class "Player" [] [])),
       ClassUml (Concrete (Class "PlayerInventory" [] [])),
       ClassUml (Concrete (Class "Printer" [] [])),
       ClassUml (Concrete (Class "Room" [] [])),
       ClassUml (Concrete (Class "RoomInventory" [] [])),
       ClassUml (Concrete (Class "StartGame" [] [])),
       ClassUml (Concrete (Class "World" [] []))]
compiled :: String
compiled = compile(transformUmlList "Asst3Uml" uml)

main = do
  toHandle <- openFile "out.dot" WriteMode
  hPutStr toHandle compiled
