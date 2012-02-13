module Lambdo where
import System.IO
import System.Environment
import System.Directory
import Control.Monad.Instances
import Text.Printf
import Data.List
import Data.Char
import Data.Maybe
import Data.Time
import Data.Time.Calendar
import Data.Time.Format
 
-- Two helpers
--io f = interact (unlines . f . lines)
--showln  = (++ "\n") . show

today :: IO Day
today = getCurrentTime >>= return . utctDay

main :: IO ()
main = command_loop

test = do
    d <- today
    let time1 = fromGregorian 2012 3 14
    let time2 = fromGregorian 2012 3 15 
    let time3 = fromGregorian 2012 3 17 
    let a = ToDo "Finish this program" time1 Academic High InProgress
    let b = ToDo "Write a SOSC paper" time2 Academic High Open
    let c = ToDo "Study for PBPL exam" time3 Academic High Open
    let e = ToDo "Work out" time1 Personal High Open
    let f = ToDo "Shady Dealer Meeting" time1 RSO Medium Open
    let l = [a,b,c,e,f]
    return l

command_loop = do
    putStrLn "l \t Show To Do Items"
    putStrLn "a \t Add a To Do Item"
    putStrLn "d \t Delete a To Do Item"
    putStrLn "q \t Quit"
    putStr "\n"
    command <- getLine
    if command == "l"
        then do putStrLn "list"
                command_loop
        else if command == "a" 
            then do putStrLn "add"
                    command_loop
        else if command == "d" 
            then do putStrLn "delete"
                    command_loop
        else if command == "q" 
            then do putStrLn "Bye!"
        else do putStrLn "Sorry, that's not a valid command."
                command_loop


showList :: ToDoList -> IO ()
showList l = do
    d <- today
    putStr "\n"
    print (reverse $ sortBy (sortToDo d) l)


data ToDo = ToDo {
            toDoDesc        :: String,
            todoDue         :: Day,
            toDoCat         :: Category,
            toDoImportance  :: Level,
            toDoStatus      :: Status
            }
            deriving (Read, Eq)

data Category = Personal | Academic | Work | RSO
            deriving (Show, Read, Eq, Ord)

data Level = Low | Medium | High
            deriving (Show, Read, Eq, Ord)

data Status = Complete | InProgress | Open
            deriving (Show, Read, Eq, Ord)

type ToDoList = [ToDo]

instance Show ToDo where
    show (ToDo a b _ _ _ ) =
        a ++ ". Due: " ++ (showGregorian b)
    showList ls s = show1 shows ls s where
        show1 _     []     s = "[]" ++ s
        show1 showx (x:xs) s = ' ' : showx x (showl xs)
          where
            showl []     = ' ' : s
            showl (y:ys) = '\n' : showx y (showl ys)
        

calculatePriority :: Day -> ToDo -> Int
calculatePriority today (ToDo _ due cat imp stat) = 15*dueVal + 5*catVal + 5*statVal + 2*impVal where
                                        dueVal
                                            | diffDays due today == 0 = 9 
                                            | diffDays due today == 1 = 7
                                            | diffDays due today == 2 = 5
                                            | diffDays due today == 3 = 3
                                            | otherwise = 1
                                        catVal 
                                            | cat == Personal = 3
                                            | cat == Academic = 10
                                            | cat == Work = 6 
                                            | cat == RSO = 5 
                                        statVal
                                            | stat == InProgress = 5
                                            | stat == Open = 10
                                        impVal
                                            | imp == Low = 0
                                            | imp == Medium = 5
                                            | imp == High = 10 

addToDo :: ToDoList -> ToDo -> ToDoList
addToDo ts t = ts ++ [t]

removeToDo :: ToDoList -> ToDo -> ToDoList
removeToDo ts t = delete t ts

sortToDo :: Day -> ToDo -> ToDo -> Ordering
sortToDo d a b 
    | (calculatePriority d a) < (calculatePriority d b)  = LT
    | (calculatePriority d a) > (calculatePriority d b)  = GT
    | (calculatePriority d a) == (calculatePriority d b) = EQ 


load :: (Read a) => FilePath -> IO a
load f = do s <- readFile f
            return (read s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (show x)

