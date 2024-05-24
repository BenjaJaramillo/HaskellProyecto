import System.IO
import Control.Exception (try, SomeException)

data MyUTCTime = MyUTCTime
    { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    , second :: Int
    } deriving (Show, Read)

getCurrentTime :: IO MyUTCTime
getCurrentTime = return $ MyUTCTime 2024 5 21 12 0 0

data Task = Task
    { taskId :: Int
    , description :: String
    , category :: String
    , createdAt :: Maybe MyUTCTime
    , dueDate :: Maybe (Int, Int, Int)  -- Fecha de vencimiento como (Año, Mes, Día)
    , priority :: Priority
    } deriving (Show, Read)

type TaskList = [Task]

data Priority = Low | Medium | High deriving (Show, Read)

mainMenu :: TaskList -> IO ()
mainMenu tasks = do
    putStrLn "\nMain Menu:"
    putStrLn "1. Add a task"
    putStrLn "2. Delete a task"
    putStrLn "3. List all tasks"
    putStrLn "4. Exit"
    putStr "Choose an option: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> addTask tasks
        "2" -> deleteTask tasks
        "3" -> listTasks tasks
        "4" -> saveTasks tasks
        _   -> do
            putStrLn "Invalid option, please try again."
            mainMenu tasks

addTask :: TaskList -> IO ()
addTask tasks = do
    putStrLn "Enter task description:"
    description <- getLine
    putStrLn "Enter task category:"
    category <- getLine
    priority <- getPriority
    currentTime <- getCurrentTime
    dueDate <- getDueDate
    let task = Task (generateTaskId tasks) description category (Just currentTime) dueDate priority
    putStrLn "Task added successfully."
    mainMenu (tasks ++ [task])

getPriority :: IO Priority
getPriority = do
    putStrLn "Enter task priority (Low, Medium, High):"
    priorityStr <- getLine
    case priorityStr of
        "Low" -> return Low
        "Medium" -> return Medium
        "High" -> return High
        _ -> do
            putStrLn "Invalid priority. Please enter Low, Medium, or High."
            getPriority

getDueDate :: IO (Maybe (Int, Int, Int))
getDueDate = do
    putStrLn "Enter due date (YYYY-MM-DD):"
    dateStr <- getLine
    case parseDate dateStr of
        Just (year, month, day) -> return $ Just (year, month, day)
        Nothing -> do
            putStrLn "Invalid date format. Please enter date in YYYY-MM-DD format."
            getDueDate

parseDate :: String -> Maybe (Int, Int, Int)
parseDate str =
    case splitDate str of
        [yearStr, monthStr, dayStr] ->
            case (readMaybe yearStr, readMaybe monthStr, readMaybe dayStr) of
                (Just year, Just month, Just day) -> Just (year, month, day)
                _ -> Nothing
        _ -> Nothing
  where
    splitDate :: String -> [String]
    splitDate [] = []
    splitDate s@(c:cs)
        | c == '-' = splitDate cs
        | otherwise = let (part, rest) = break (== '-') s
                      in part : splitDate rest


readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x, "")] -> Just x
    _         -> Nothing

generateTaskId :: TaskList -> Int
generateTaskId [] = 1
generateTaskId tasks = maximum (map taskId tasks) + 1

deleteTask :: TaskList -> IO ()
deleteTask tasks = do
    putStrLn "Enter task ID to delete:"
    idStr <- getLine
    case reads idStr :: [(Int, String)] of
        [(taskIdToDelete, "")] -> do
            let taskExists = any (\task -> taskId task == taskIdToDelete) tasks
            if taskExists
                then do
                    let updatedTasks = filter (\task -> taskId task /= taskIdToDelete) tasks
                    putStrLn "Task deleted successfully."
                    mainMenu updatedTasks
                else do
                    putStrLn "Task not found."
                    mainMenu tasks
        _ -> do
            putStrLn "Invalid task ID. Please enter a valid number."
            deleteTask tasks

listTasks :: TaskList -> IO ()
listTasks tasks = do
    putStrLn "\nYour Tasks:"
    mapM_ printTask tasks
    mainMenu tasks

printTask :: Task -> IO ()
printTask task = putStrLn $ show (taskId task) ++ ". " ++ description task ++ " [" ++ show (priority task) ++ "]" ++ dueDateStr
    where
        dueDateStr = case dueDate task of
            Just (year, month, day) -> " Due: " ++ show year ++ "-" ++ show month ++ "-" ++ show day
            Nothing -> ""

doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try (withFile path ReadMode (\_ -> return ())) :: IO (Either SomeException ())
    case result of
        Left _ -> return False
        Right _ -> return True

readFileSafe :: FilePath -> IO (Either SomeException String)
readFileSafe path = try (readFile path)

writeFileSafe :: FilePath -> String -> IO (Either SomeException ())
writeFileSafe path content = try (writeFile path content)

loadTasks :: IO TaskList
loadTasks = do
    fileExists <- doesFileExist "tasks.txt"
    if fileExists
        then do
            result <- readFileSafe "tasks.txt"
            case result of
                Left ex -> do
                    putStrLn $ "Error reading tasks: " ++ show ex
                    return []
                Right contents ->
                    if null contents
                        then return []
                        else case reads contents :: [(TaskList, String)] of
                            [(tasks, "")] -> return tasks
                            _ -> do
                                putStrLn "Error: Invalid format in tasks.txt"
                                return []
        else return []

saveTasks :: TaskList -> IO ()
saveTasks tasks = do
    result <- writeFileSafe "tasks.txt" (show tasks)
    case result of
        Left ex -> putStrLn $ "Error saving tasks: " ++ show ex
        Right _ -> putStrLn "Tasks saved. Goodbye!"

main :: IO ()
main = do
    tasks <- loadTasks
    putStrLn "Welcome to the Haskell To-Do List Manager"
    mainMenu tasks
