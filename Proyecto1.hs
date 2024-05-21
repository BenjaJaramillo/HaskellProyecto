import System.IO
import Control.Exception (try, SomeException) -- Importa el módulo para manejar excepciones

-- Define un tipo de dato simple para representar la fecha y hora
data MyUTCTime = MyUTCTime
    { year :: Int      -- Año
    , month :: Int     -- Mes
    , day :: Int       -- Día
    , hour :: Int      -- Hora
    , minute :: Int    -- Minuto
    , second :: Int    -- Segundo
    } deriving (Show, Read) -- Deriva las instancias de Show y Read para impresión y lectura

-- Función para obtener la fecha y hora actual (simulada)
getCurrentTime :: IO MyUTCTime
getCurrentTime = do
    -- Esta función debería obtener la fecha y hora real del sistema
    -- Aquí simulamos con una fecha fija para simplicidad
    return $ MyUTCTime 2024 5 21 12 0 0

-- Define el tipo de dato 'Task' para representar una tarea
data Task = Task
    { taskId :: Int                -- ID de la tarea
    , description :: String        -- Descripción de la tarea
    , category :: String           -- Categoría de la tarea
    , createdAt :: Maybe MyUTCTime -- Fecha de creación de la tarea (puede ser Nothing si no se especifica)
    , priority :: Priority         -- Prioridad de la tarea
    } deriving (Show, Read)        -- Deriva las instancias de Show y Read

type TaskList = [Task] -- Alias para una lista de tareas

data Priority = Low | Medium | High deriving (Show, Read) -- Prioridad puede ser Baja, Media o Alta

-- Función para mostrar el menú principal
mainMenu :: TaskList -> IO ()
mainMenu tasks = do
    putStrLn "\nMain Menu:"       -- Imprime el menú principal
    putStrLn "1. Add a task"      -- Opción para agregar una tarea
    putStrLn "2. Delete a task"   -- Opción para eliminar una tarea
    putStrLn "3. List all tasks"  -- Opción para listar todas las tareas
    putStrLn "4. Exit"            -- Opción para salir
    putStr "Choose an option: "   -- Solicita una opción al usuario
    hFlush stdout                 -- Asegura que la salida se imprima inmediatamente
    option <- getLine             -- Lee la opción del usuario
    case option of
        "1" -> addTask tasks      -- Llama a 'addTask' si la opción es 1
        "2" -> deleteTask tasks   -- Llama a 'deleteTask' si la opción es 2
        "3" -> listTasks tasks    -- Llama a 'listTasks' si la opción es 3
        "4" -> saveTasks tasks    -- Llama a 'saveTasks' si la opción es 4
        _   -> do                 -- Si la opción no es válida:
            putStrLn "Invalid option, please try again."
            mainMenu tasks        -- Vuelve a mostrar el menú principal

-- Función para agregar una tarea
addTask :: TaskList -> IO ()
addTask tasks = do
    putStrLn "Enter task description: " -- Solicita la descripción de la tarea
    description <- getLine              -- Lee la descripción
    putStrLn "Enter task category: "    -- Solicita la categoría de la tarea
    category <- getLine                 -- Lee la categoría
    priority <- getPriority             -- Solicita la prioridad de la tarea
    currentTime <- getCurrentTime       -- Obtiene la hora y fecha actual
    let task = Task (generateTaskId tasks) description category (Just currentTime) priority -- Crea una nueva tarea
    putStrLn "Task added successfully." -- Informa que la tarea fue agregada con éxito
    mainMenu (tasks ++ [task])          -- Agrega la tarea a la lista y vuelve al menú principal

-- Función para obtener la prioridad de la tarea del usuario
getPriority :: IO Priority
getPriority = do
    putStrLn "Enter task priority (Low, Medium, High): " -- Solicita la prioridad de la tarea
    priorityStr <- getLine                               -- Lee la prioridad
    case priorityStr of
        "Low" -> return Low                              -- Devuelve 'Low' si la entrada es "Low"
        "Medium" -> return Medium                        -- Devuelve 'Medium' si la entrada es "Medium"
        "High" -> return High                            -- Devuelve 'High' si la entrada es "High"
        _ -> do                                          -- Si la entrada no es válida:
            putStrLn "Invalid priority. Please enter Low, Medium, or High."
            getPriority                                  -- Solicita nuevamente la prioridad

-- Función para generar un nuevo ID de tarea
generateTaskId :: TaskList -> Int
generateTaskId [] = 1                                    -- Si la lista de tareas está vacía, el ID es 1
generateTaskId tasks = maximum (map taskId tasks) + 1    -- Si no, el ID es el máximo ID existente más 1

-- Función para eliminar una tarea
deleteTask :: TaskList -> IO ()
deleteTask tasks = do
    putStrLn "Enter task ID to delete: "                 -- Solicita el ID de la tarea a eliminar
    idStr <- getLine                                     -- Lee el ID
    case reads idStr :: [(Int, String)] of               -- Intenta convertir la entrada a un número
        [(taskIdToDelete, "")] -> do                     -- Si la conversión es exitosa:
            let taskExists = any (\task -> taskId task == taskIdToDelete) tasks -- Verifica si la tarea existe
            if taskExists
                then do
                    let updatedTasks = filter (\task -> taskId task /= taskIdToDelete) tasks -- Filtra la tarea a eliminar
                    putStrLn "Task deleted successfully." -- Informa que la tarea fue eliminada
                    mainMenu updatedTasks                -- Vuelve al menú principal con la lista actualizada
                else do
                    putStrLn "Task not found."           -- Informa que no se encontró la tarea
                    mainMenu tasks                       -- Vuelve al menú principal
        _ -> do                                          -- Si la conversión falla:
            putStrLn "Invalid task ID. Please enter a valid number."
            deleteTask tasks                             -- Solicita nuevamente el ID

-- Función para listar todas las tareas
listTasks :: TaskList -> IO ()
listTasks tasks = do
    putStrLn "\nYour Tasks:"                             -- Imprime el encabezado de la lista de tareas
    mapM_ printTask tasks                                -- Imprime cada tarea
    mainMenu tasks                                       -- Vuelve al menú principal

-- Función para imprimir una tarea
printTask :: Task -> IO ()
printTask task = putStrLn $ show (taskId task) ++ ". " ++ description task ++ " [" ++ show (priority task) ++ "]"
    -- Imprime la tarea en el formato: "ID. Descripción [Prioridad]"

-- Función para verificar si un archivo existe
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try (withFile path ReadMode (\_ -> return ())) :: IO (Either SomeException ())
    -- Intenta abrir el archivo en modo lectura
    case result of
        Left _ -> return False -- Si hay una excepción, el archivo no existe
        Right _ -> return True -- Si no hay excepción, el archivo existe

-- Función para leer un archivo de manera segura
readFileSafe :: FilePath -> IO (Either SomeException String)
readFileSafe path = try (readFile path) -- Intenta leer el archivo y maneja posibles excepciones

-- Función para escribir un archivo de manera segura
writeFileSafe :: FilePath -> String -> IO (Either SomeException ())
writeFileSafe path content = try (writeFile path content) -- Intenta escribir el archivo y maneja posibles excepciones

-- Función para cargar las tareas desde un archivo
loadTasks :: IO TaskList
loadTasks = do
    fileExists <- doesFileExist "tasks.txt" -- Verifica si el archivo "tasks.txt" existe
    if fileExists
        then do
            result <- readFileSafe "tasks.txt" -- Intenta leer el archivo
            case result of
                Left ex -> do -- Si hay una excepción al leer el archivo:
                    putStrLn $ "Error reading tasks: " ++ show ex
                    return [] -- Devuelve una lista vacía
                Right contents ->
                    if null contents
                        then return [] -- Si el archivo está vacío, devuelve una lista vacía
                        else case reads contents :: [(TaskList, String)] of
                            [(tasks, "")] -> return tasks -- Si la lectura es exitosa, devuelve la lista de tareas
                            _ -> do
                                putStrLn "Error: Invalid format in tasks.txt"
                                return [] -- Si el formato es inválido, devuelve una lista vacía
        else return [] -- Si el archivo no existe, devuelve una lista vacía

-- Función para guardar las tareas en un archivo
saveTasks :: TaskList -> IO ()
saveTasks tasks = do
    result <- writeFileSafe "tasks.txt" (show tasks) -- Intenta escribir la lista de tareas en el archivo
    case result of
        Left ex -> putStrLn $ "Error saving tasks: " ++ show ex -- Si hay una excepción, imprime un mensaje de error
        Right _ -> putStrLn "Tasks saved. Goodbye!" -- Si la escritura es exitosa, informa que las tareas fueron guardadas

-- Función principal
main :: IO ()
main = do
    tasks <- loadTasks -- Carga las tareas desde el archivo
    putStrLn "Welcome to the Haskell To-Do List Manager" -- Imprime un mensaje de bienvenida
    mainMenu tasks -- Muestra el menú principal
