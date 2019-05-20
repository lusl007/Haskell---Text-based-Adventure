--Import necessary libraries
import Data.List
import Data.Char

--define data types
type Location = String
type Direction = String
type Item = String
type Response = String

--directions to all rooms in and out which are defined by
--room, direction entered and the resulting room
type DirectionMap = [((Location, Direction), Location)]
paths :: DirectionMap
paths = [
    (("Hallway", "s"), "Exit"),
    (("Exit", "n"), "Hallway"),
    (("Hallway", "n"), "Upper Hallway"),
    (("Hallway", "e"), "Kitchen"),
    (("Hallway", "w"), "Living Room"),
    (("Hallway", "ne"), "Dining Room"),
    (("Hallway", "nw"), "Guest Room"),
    (("Kitchen", "n"), "Dining Room"),
    (("Kitchen", "w"), "Hallway"),
    (("Living Room", "e"), "Hallway"),
    (("Guest Room", "e"), "Hallway"),
    (("Dining Room", "w"), "Hallway"),
    (("Dining Room", "s"), "Kitchen"),
--directions for upper floor
    (("Upper Hallway", "s"), "Hallway"),
    (("Upper Hallway", "e"), "Bedroom 1"),
    (("Upper Hallway", "w"), "Bedroom 2"),
    (("Upper Hallway", "se"), "Dressing Room"),
    (("Upper Hallway", "sw"), "Study"),
    (("Bedroom 2", "e"), "Upper Hallway"),
    (("Study", "e"), "Upper Hallway"),
    (("Dressing Room", "n"), "Bedroom 1"),
    (("Dressing Room", "w"), "Upper Hallway"),
    (("Bedroom 1", "w"), "Upper Hallway"),
    (("Bedroom 1", "s"), "Dressing Room"),
    (("Bedroom 1", "e"), "Bathroom"),
    (("Bedroom 1", "s"), "Dressing Room")
    ]

--defining which item (Item) is located in which room (Location)
type ItemLocationMap = [(Item, Location)]
locations :: ItemLocationMap
locations =  [
    ("me", "Hallway"),
    ("candle", "Dining Room"),
    ("food", "Kitchen"),
    ("trap", "Living Room"),
    ("ghost", "Bathroom"),
    ("bathroom key", "Bedroom 2"),
    ("candle", "Study"),
    ("food", "Study"),
    ("dressing room key", "Bedroom 1"),
    ("trap", "Dressing Room")
    ]

--response and reaction of the gameplay when entered direction
type Mansion = (DirectionMap, ItemLocationMap, Response)
mansion :: IO (DirectionMap, ItemLocationMap, Response)
mansion = return (paths, locations, "")

--introduction before starting the game
--player is asked to enter his name
--getLine xs and putStr in between the sentence
--Monad expression
game :: IO()
game = do putStrLn "Please enter a username."
          xs <- getLine
          putStrLn ""
          putStr "Welcome player "
          putStr xs
          putStr " to the game Find the ghost.\n"
          putStrLn ""
          putStrLn introduction
          putStrLn instructions

--starting the game
start :: IO (String)
start = do
    play_game $ return (paths, locations, "")
    return "End. Thank you for playing!"

--short background story to the game
introduction =
         "As a ghost hunter, you have been called to catch a ghost in the Kingston Mansion.\n"++
         "You get all your equipment ready and get into your car.\n"++
         "When arriving you see a big gate which is locked with a chain.\n"++
         "You take your pliers out of the trunk and easily break the chain open.\n"++
         "You park in front of the mansion's main entrance and get out of the car.\n"++
         "The mansion looks very old and run-down and you can already hear strange noises.\n"++
         "Get ready for the ghost hunt.\n"

--instructions for the player about which commands to use
--in order to play and proceed in the game
instructions =
          "Enter commands using one or two words.\n"++
          "start:                      Starts the game\n" ++
          "n  e  s  w ne nw se sw:     Choose direction (North, East, South, West, North East, North West, South East, South West).\n" ++
          "pick up (name  of object):  Pick up an object.\n" ++
          "drop (name of object):      to put down the named object.\n" ++
          "attack:                     Attack an enemy.\n" ++  
          "look:                       Look around you again.\n" ++
          "i:                          View the items you possess in your bag.\n" ++
          "quit:                       End and quit the game."

--function works throughout the game, as a loop
play_game :: IO (Mansion) -> IO (Mansion)
play_game mansion = do
    (paths, locations, response) <- mansion
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
--player will see where to enter the command
        else do
            putStr "Command >> "
            command <- getLine
--if "quit" has been entered do this function
            if command == "quit"
                then return (paths, locations, "Quitting.")
            else  play_game $ return (play_command command paths locations)

--function for when player lost/won the game
game_over :: ItemLocationMap -> Bool
game_over locations =
    let current_location = get "me" locations
        candle_location = get "candle" locations
    in current_location == "dead" || (current_location == "Bathroom" && candle_location == "holding")

--ensures that certain doors/rooms can be unlocked
--by using a specific item    
allowed_move :: Location -> Direction -> DirectionMap -> ItemLocationMap -> Bool
allowed_move "Bedroom 1" "e" _ locations = get "bathroom key" locations == "holding"
allowed_move "Bedroom 1" "s" _ locations = get "dressing room key" locations == "holding"
allowed_move "Upper Hallway" "se" _ locations = get "dressing room key" locations == "holding"
allowed_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

--restricts interacting with locked doors
--cannot enter locked doors without appropriate item
no_move :: Location -> Direction -> Response
no_move "Bedroom 1" "e" = "You need the bathroom key to open this door."
no_move "Bedroom 1" "s" = "You need the dressing room key to open this door."
no_move "Upper Hallway" "se" = "You need the dressing room key to open this door."
no_move "Living Room" "e" = "You can't move. You are dead. Quit the game and awaken from the dead. Ha!"
no_move "Dressing Room" "n" = "You can't move. You are dead. Quit the game and awaken from the dead. Ha!"
no_move "Dressing Room" "w" = "You can't move. You are dead. Quit the game and awaken from the dead. Ha!"
no_move _ _ = "Nothing to see here."

--player moves from current location in entered diretion
--based on the pre-defined paths above and is in a new location
move :: Location -> Direction -> DirectionMap -> Location
move from direction paths = get (from, direction) paths

--playing commands when entered directions
--rules of directions/commands
play_command :: String -> DirectionMap -> ItemLocationMap -> Mansion
play_command "n" paths locations = go "n" paths locations
play_command "e" paths locations = go "e" paths locations
play_command "s" paths locations = go "s" paths locations
play_command "w" paths locations = go "w" paths locations
play_command "ne" paths locations = go "ne" paths locations
play_command "nw" paths locations = go "nw" paths locations
play_command "se" paths locations = go "se" paths locations
play_command "sw" paths locations = go "sw" paths locations
play_command "look" paths locations = look paths locations
play_command "attack" paths locations = attack paths locations
play_command "i" paths locations = (paths, locations, bag locations)
play_command "quit" paths locations = (paths, locations, "quit")
play_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
play_command cmd paths locations = play_command_2 cmd paths locations

--more commands for taking and dropping items
play_command_2 :: String -> DirectionMap -> ItemLocationMap -> Mansion
play_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          pick_up (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "Invalid input: " ++ cmd)

--pick up an object if there is one in the current location
--if object is already in inventory player will get notified
--when successfully picked up object player will be notified
pick_up :: Item -> DirectionMap -> ItemLocationMap -> Mansion          
pick_up item paths locations =
    let here = get "me" locations
        there = get item locations
    in if here == there
       then (paths, (put item "holding" locations), "You took the object.")
       else if there == "holding"
            then (paths, locations, "You already have that.")
            else (paths, locations, "I don't see it here.")

--dropping objects from bag        
game_drop :: Item -> DirectionMap -> ItemLocationMap -> Mansion          
game_drop item paths locations =
    let here = get "me" locations
        there = get item locations
    in if there == "holding"
        then (paths, (put item here locations), "Object dropped.")
        else (paths, locations, "You aren't holding this object.")

--moving the player in the direction only if the move
--is allowed
--updated location of player
go :: String -> DirectionMap -> ItemLocationMap -> Mansion
go direction paths locations = do
    let current_location = get "me" locations
    if allowed_move current_location direction paths locations
        then do
            let new_location = move current_location direction paths
            let new_locations = put "me" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, no_move current_location direction)

--look around in the room to check if there are any items to pick up
look :: DirectionMap -> ItemLocationMap -> Mansion
look paths locations =
    if items == []
        then (paths, locations, describe current_location locations)
        else (paths, locations, describe current_location locations ++ "\n\n" ++ items)
    where current_location = get "me" locations
          items = items_here locations

--attacking creatures
--sometimes the player needs a certain item to attack
attack :: DirectionMap -> ItemLocationMap -> Mansion
attack paths locations =
    case get "me" locations of
        "Guest Room" -> (paths, locations,
                   "You were lucky. You managed to kill the spiders by jumping on them.\n"++
                   "No HP lost. Go East to exit.")
        "Kitchen" -> (paths, locations,
                   "The rats are too strong together. They manage to bite and scratch you.\n"++
                   "You lost 30HP.")
        "Bedroom 2" -> (paths, locations,
                   "Probably not the best idea to attack him. The dog is vicious and bites\n"++
                   "you in your arm. You lost 40HP.")
        _ -> (paths, locations, "You cannot see any creatures here.")

--inventory holding items
bag :: ItemLocationMap -> Response
bag locations =
    let my_items = [item | (item, "holding") <- locations]
    in if my_items == []
        then "You don't have any items."
        else store ", " my_items

--informs the user which items are in the current
items_here :: ItemLocationMap -> Response
items_here locations =
    let here = get "me" locations
        items = ["There is a " ++ item ++ " here." |
                  (item, place) <- locations, place == here, item /= "me"]
    in store "\n" items

-- "get" finds the value of a key in a (key, value) list
--e.g get an item to do something with it
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

--puts the value of the (key, value) list
put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> ItemLocationMap -> String
describe new_location locations =
    let here = get "me" locations
        ghost_status = get "ghost" locations
        candle_location = get "candle" locations
    in describe_helper here ghost_status candle_location  locations 

--more descriptions depending on the situation and
--depending if alive or dead
describe_helper :: Location -> String -> String -> ItemLocationMap -> String
describe_helper "Bathroom" "alive" "holding" locations = description "Bathroom."
describe_helper "Living Room" "dead" _ locations = description "Living Room."
describe_helper "Dressing Room" "dead" _ locations = description "Dressing Room"
describe_helper here _ _ locations = description here

--description for each location inside the mansion
--tells the player what to do and in which directions he can go
description :: Location -> String

description "Bathroom" = "As you enter you can feel the mysterious presence in the room.\n"++
    "You look to the window and see a black figure that seems to flicker. It has red glooming eyes.\n"++
    "Despite the creepy and uncomfortable feeling you don't hesitate, light the candle and\n"++
    "throw it onto the figure. It screeches so loud but soon after it disappears.\n"++
    "Congratulations! You have caught the ghost and secured the mansion."

description "Hallway" =
    "You are standing in the Hallway.\n" ++
    "Go North to go upstairs.\n" ++
    "Go East to go to the Kitchen.\n"++
    "Walk West to enter the Living Room.\n"++
    "Walk North East to see the Dining Room or.\n"++
    "choose North West for the Guest Room.\n"++
    "South is the exit and ends the game."

description "Kitchen" =
    "You entered the Kitchen. There is a very bad smell in here.\n"++
    "Rats are trying to bite and scratch you! They could seriously infect you!\n"++
    "Maybe do something?"

description "Dining Room" =
    "You are in the Dining Room. It's all dusty here and there are many\n"++
    "spiderwebs too. No one has lived in the mansion for years.\n"++
    "You can look around in the room or choose another direction:\n"++
    "Go South to enter the Kitchen or West to go to the Hallway."

description "Guest Room" =
    "You entered the Guest Room. It is quite dark in here.\n" ++
    "Suddenly, you feel a little tingle on your legs and it goes up to your arms.\n" ++
    "When you look down your arms you see many wolf spiders crawling up.\n"++
    "You must know that they can bite! Maybe you want to do something about it?\n"

description "Living Room" =
    "The Living Room is very very dark. it is hard to look around.\n" ++
    "You cannot turn on the lights because the lamp is broken.\n"++
    "But you still decide to walk further inside but you did not notice\n"++
    "the small trap in the middle of the room.\n"++
    "The chandelier falls on you and knocks you out. This is where you die.\n"

description "Bedroom 2" =
    "The moment you entered this room a stray dog is barking at you.\n"++
    "He looks very intimidating and you should be careful about your next move.\n"++
    "Either attack or leave the room in East direction."

description "Study" =
    "This is the Study. Looks like any other ordinary study room.\n" ++
    "You can either look around or go East and exit the room."

description "Dressing Room" =
    "You thought there was going to be a huge suprise in here because\n"++
    "the room was locked? Well, too bad. All you can find here is an unstable, crumbly floor\n"++
    "through which you fall down to the basement. You did not survive."

description "Bedroom 1" =
    "In here you can find the key to the Dressing Room.\n"++
    "East there is a door that leads to another room but it seems locked.\n"++
    "As well as in South direction: a locked door leading to the Dressing Room.\n"++
    "Go West to exit the room into the Upper Hallway."

description "Upper Hallway" =
    "You went upstairs. You are standing in the Upper Hallway.\n"++
    "From here you can explore more different rooms:\n"++
    "Go South to go back downstairs.\n"++
    "Choose East to enter Bedroom 1\n"++
    "Choose West for Bedroom 2.\n"++
    "In South East there is the dressing Room and South West is the Study located.\n"

description somewhere = somewhere ++ ", you are not able to see anything here."