-- Importamos el módulo System.IO para manejar la entrada/salida
import System.IO
-- Importamos la excepción SomeException desde Control.Exception para manejar excepciones
import Control.Exception (try, SomeException)

-- Definimos un tipo de dato llamado MyUTCTime que representa una fecha y hora
data MyUTCTime = MyUTCTime
    { year :: Int      -- Año
    , month :: Int     -- Mes
    , day :: Int       -- Día
    , hour :: Int      -- Hora
    , minute :: Int    -- Minuto
    , second :: Int    -- Segundo
    } deriving (Show, Read) -- Derivamos instancias de Show y Read para MyUTCTime

-- Definimos una función llamada getCurrentTime que devuelve la fecha y hora actual
getCurrentTime :: IO MyUTCTime
getCurrentTime = return $ MyUTCTime 2024 5 21 12 0 0
-- La función simplemente devuelve una instancia de MyUTCTime con los valores proporcionados (simulando la fecha y hora actual)


-- Definimos un tipo de dato llamado Task que representa una tarea
data Task = Task
    { taskId :: Int                 -- ID de la tarea
    , description :: String         -- Descripción de la tarea
    , category :: String            -- Categoría de la tarea
    , createdAt :: Maybe MyUTCTime -- Fecha de creación de la tarea (puede ser Nothing si no se especifica)
    , dueDate :: Maybe (Int, Int, Int)  -- Fecha de vencimiento de la tarea (puede ser Nothing si no se especifica)
    , priority :: Priority         -- Prioridad de la tarea
    } deriving (Show, Read)         -- Derivamos instancias de Show y Read para Task

-- Definimos un alias para una lista de tareas
type TaskList = [Task]

-- Definimos un tipo de dato llamado Priority que representa la prioridad de una tarea
data Priority = Low | Medium | High deriving (Show, Read) -- La prioridad puede ser Baja, Media o Alta

-- Definimos una función llamada mainMenu que muestra el menú principal y gestiona las opciones elegidas por el usuario
mainMenu :: TaskList -> IO ()
mainMenu tasks = do
    putStrLn "\nMain Menu:"                -- Imprime el encabezado del menú principal
    putStrLn "1. Add a task"               -- Opción para agregar una tarea
    putStrLn "2. Delete a task"            -- Opción para eliminar una tarea
    putStrLn "3. List all tasks"           -- Opción para listar todas las tareas
    putStrLn "4. Exit"                     -- Opción para salir
    putStr "Choose an option: "            -- Solicita una opción al usuario
    hFlush stdout                          -- Asegura que la salida se imprima inmediatamente
    option <- getLine                      -- Lee la opción ingresada por el usuario
    case option of
        "1" -> addTask tasks               -- Si la opción es "1", llama a la función addTask
        "2" -> deleteTask tasks            -- Si la opción es "2", llama a la función deleteTask
        "3" -> listTasks tasks             -- Si la opción es "3", llama a la función listTasks
        "4" -> saveTasks tasks             -- Si la opción es "4", llama a la función saveTasks
        _   -> do                          -- Si la opción no es válida:
            putStrLn "Invalid option, please try again."  -- Imprime un mensaje de error
            mainMenu tasks                 -- Vuelve a mostrar el menú principal para que el usuario elija otra opción

-- Definimos una función llamada addTask que permite al usuario agregar una nueva tarea
addTask :: TaskList -> IO ()
addTask tasks = do
    putStrLn "Enter task description:"  -- Solicita al usuario que ingrese la descripción de la tarea
    description <- getLine              -- Lee la descripción ingresada por el usuario
    putStrLn "Enter task category:"     -- Solicita al usuario que ingrese la categoría de la tarea
    category <- getLine                 -- Lee la categoría ingresada por el usuario
    priority <- getPriority             -- Solicita al usuario que ingrese la prioridad de la tarea
    currentTime <- getCurrentTime       -- Obtiene la fecha y hora actual
    dueDate <- getDueDate               -- Solicita al usuario que ingrese la fecha de vencimiento de la tarea
    let task = Task                     -- Crea una nueva tarea con los datos proporcionados por el usuario
            (generateTaskId tasks)      -- Genera un nuevo ID de tarea
            description                 -- Utiliza la descripción ingresada por el usuario
            category                    -- Utiliza la categoría ingresada por el usuario
            (Just currentTime)          -- Utiliza la fecha y hora actual
            dueDate                     -- Utiliza la fecha de vencimiento ingresada por el usuario
            priority                    -- Utiliza la prioridad ingresada por el usuario
    putStrLn "Task added successfully." -- Imprime un mensaje indicando que la tarea ha sido agregada con éxito
    mainMenu (tasks ++ [task])          -- Retorna al menú principal con la lista actualizada de tareas, incluyendo la nueva tarea


-- Definimos una función llamada getPriority que solicita al usuario que ingrese la prioridad de una tarea y devuelve la prioridad seleccionada como un valor del tipo de dato Priority
getPriority :: IO Priority
getPriority = do
    putStrLn "Enter task priority (Low, Medium, High):"  -- Solicita al usuario que ingrese la prioridad de la tarea
    priorityStr <- getLine                              -- Lee la prioridad ingresada por el usuario
    case priorityStr of
        "Low" -> return Low                              -- Si la prioridad ingresada es "Low", devuelve el valor Low
        "Medium" -> return Medium                        -- Si la prioridad ingresada es "Medium", devuelve el valor Medium
        "High" -> return High                            -- Si la prioridad ingresada es "High", devuelve el valor High
        _ -> do                                          -- Si la prioridad ingresada no es válida:
            putStrLn "Invalid priority. Please enter Low, Medium, or High."  -- Imprime un mensaje de error
            getPriority                                    -- Llama recursivamente a getPriority para solicitar nuevamente al usuario que ingrese la prioridad


-- Definimos una función llamada getDueDate que solicita al usuario que ingrese la fecha de vencimiento de una tarea y devuelve la fecha ingresada como una tupla (Maybe (Int, Int, Int))
getDueDate :: IO (Maybe (Int, Int, Int))
getDueDate = do
    putStrLn "Enter due date (YYYY-MM-DD):"  -- Solicita al usuario que ingrese la fecha de vencimiento de la tarea
    dateStr <- getLine                       -- Lee la fecha ingresada por el usuario
    case parseDate dateStr of                -- Llama a la función parseDate para analizar la fecha ingresada por el usuario
        Just (year, month, day) -> return $ Just (year, month, day)  -- Si la fecha se puede analizar correctamente, devuelve Just (year, month, day) como una tupla envuelta en Just
        Nothing -> do                         -- Si la fecha no se puede analizar correctamente:
            putStrLn "Invalid date format. Please enter date in YYYY-MM-DD format."  -- Imprime un mensaje de error
            getDueDate                        -- Llama recursivamente a getDueDate para solicitar nuevamente al usuario que ingrese la fecha de vencimiento


-- Definimos una función llamada parseDate que analiza una cadena de texto para extraer una fecha en formato YYYY-MM-DD y devuelve Just (year, month, day) si la cadena es válida o Nothing si no lo es
parseDate :: String -> Maybe (Int, Int, Int)
parseDate str =
    case splitDate str of                         -- Llama a la función splitDate para dividir la cadena en partes (año, mes, día)
        [yearStr, monthStr, dayStr] ->           -- Si splitDate devuelve una lista con tres elementos (año, mes, día):
            case (readMaybe yearStr, readMaybe monthStr, readMaybe dayStr) of  -- Intenta convertir cada parte de la fecha en un entero
                (Just year, Just month, Just day) -> Just (year, month, day)  -- Si todas las conversiones son exitosas, devuelve Just (year, month, day)
                _ -> Nothing                        -- Si alguna conversión falla, devuelve Nothing
        _ -> Nothing                               -- Si splitDate no devuelve una lista con tres elementos, devuelve Nothing
  where
    -- Definimos una función interna llamada splitDate que divide una cadena en partes usando el guion '-' como delimitador
    splitDate :: String -> [String]
    splitDate [] = []                                          -- Caso base: si la cadena está vacía, devuelve una lista vacía
    splitDate s@(c:cs)
        | c == '-' = splitDate cs                              -- Si el primer carácter es '-', ignóralo y llama recursivamente a splitDate con el resto de la cadena
        | otherwise = let (part, rest) = break (== '-') s      -- Si el primer carácter no es '-', divide la cadena en dos partes en el primer '-'
                      in part : splitDate rest                 -- Agrega la parte dividida a la lista y llama recursivamente a splitDate con el resto de la cadena

-- La función readMaybe intenta analizar una cadena de texto en un valor de algún tipo que implementa la clase Read. Si el análisis es exitoso, devuelve Just x, donde x es el valor analizado. De lo contrario, devuelve Nothing.
readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of       -- Intenta analizar la cadena de texto usando la función reads
    [(x, "")] -> Just x                 -- Si el análisis es exitoso (devuelve una lista con un solo elemento y una cadena vacía), devuelve Just x
    _         -> Nothing                 -- Si el análisis falla (devuelve cualquier otra cosa), devuelve Nothing

-- La función generateTaskId genera un nuevo ID para una tarea basado en la lista de tareas existente. Si la lista está vacía, devuelve 1. Si no, devuelve el máximo ID existente más 1.
generateTaskId :: TaskList -> Int
generateTaskId [] = 1                                                    -- Si la lista de tareas está vacía, el ID de la nueva tarea será 1
generateTaskId tasks = maximum (map taskId tasks) + 1                    -- Si hay tareas en la lista, el ID de la nueva tarea será el máximo ID existente más 1

-- La función deleteTask permite al usuario eliminar una tarea de la lista de tareas.
deleteTask :: TaskList -> IO ()
deleteTask tasks = do
    putStrLn "Enter task ID to delete:"         -- Solicita al usuario que ingrese el ID de la tarea a eliminar
    idStr <- getLine                            -- Lee el ID ingresado por el usuario como una cadena de texto
    case reads idStr :: [(Int, String)] of      -- Intenta convertir la cadena de texto a un entero utilizando reads
        [(taskIdToDelete, "")] -> do           -- Si la conversión es exitosa (devuelve una lista con un solo elemento y una cadena vacía):
            let taskExists = any (\task -> taskId task == taskIdToDelete) tasks  -- Verifica si la tarea con el ID especificado existe en la lista de tareas
            if taskExists                        -- Si la tarea existe:
                then do
                    let updatedTasks = filter (\task -> taskId task /= taskIdToDelete) tasks  -- Filtra la lista de tareas para eliminar la tarea con el ID especificado
                    putStrLn "Task deleted successfully."  -- Informa al usuario que la tarea ha sido eliminada con éxito
                    mainMenu updatedTasks          -- Vuelve al menú principal con la lista de tareas actualizada
                else do
                    putStrLn "Task not found."     -- Informa al usuario que la tarea no fue encontrada en la lista
                    mainMenu tasks                 -- Vuelve al menú principal con la lista de tareas original
        _ -> do                                  -- Si la conversión falla:
            putStrLn "Invalid task ID. Please enter a valid number."  -- Informa al usuario que el ID de la tarea ingresado no es válido
            deleteTask tasks                     -- Vuelve a solicitar al usuario que ingrese el ID de la tarea para eliminarla


-- La función listTasks muestra todas las tareas de la lista de tareas y luego vuelve al menú principal.
listTasks :: TaskList -> IO ()
listTasks tasks = do
    putStrLn "\nYour Tasks:"       -- Imprime el encabezado para mostrar las tareas
    mapM_ printTask tasks          -- Mapea la función printTask sobre todas las tareas y las imprime
    mainMenu tasks                 -- Vuelve al menú principal

-- La función printTask imprime los detalles de una tarea, incluido su ID, descripción, prioridad y fecha de vencimiento si está definida.
printTask :: Task -> IO ()
printTask task = putStrLn $ show (taskId task) ++ ". " ++ description task ++ " [" ++ show (priority task) ++ "]" ++ dueDateStr
    where
        dueDateStr = case dueDate task of                   -- Verifica si la tarea tiene una fecha de vencimiento definida
            Just (year, month, day) -> " Due: " ++ show year ++ "-" ++ show month ++ "-" ++ show day  -- Si hay una fecha de vencimiento, la muestra
            Nothing -> ""                                 -- Si no hay una fecha de vencimiento, muestra una cadena vacía

-- La función doesFileExist verifica si un archivo existe en la ruta especificada.
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try (withFile path ReadMode (\_ -> return ())) :: IO (Either SomeException ())  -- Intenta abrir el archivo en modo lectura
    case result of
        Left _ -> return False          -- Si ocurre una excepción al abrir el archivo, significa que el archivo no existe, por lo que devuelve False
        Right _ -> return True           -- Si no hay excepción al abrir el archivo, significa que el archivo existe, por lo que devuelve True

-- La función readFileSafe intenta leer un archivo de manera segura, manejando cualquier excepción que pueda ocurrir durante la lectura.
readFileSafe :: FilePath -> IO (Either SomeException String)
readFileSafe path = try (readFile path)  -- Intenta leer el contenido del archivo en la ruta especificada
                                        -- Si ocurre una excepción durante la lectura, se manejará y se devolverá como una instancia de SomeException
                                        -- Si la lectura es exitosa, se devuelve el contenido del archivo como una cadena

-- La función writeFileSafe intenta escribir en un archivo de manera segura, manejando cualquier excepción que pueda ocurrir durante la escritura.
writeFileSafe :: FilePath -> String -> IO (Either SomeException ())
writeFileSafe path content = try (writeFile path content)  -- Intenta escribir el contenido en el archivo en la ruta especificada
                                                         -- Si ocurre una excepción durante la escritura, se manejará y se devolverá como una instancia de SomeException
                                                         -- Si la escritura es exitosa, se devuelve () para indicar que se completó correctamente
-- La función loadTasks carga las tareas desde un archivo "tasks.txt" y devuelve una lista de tareas.
loadTasks :: IO TaskList
loadTasks = do
    fileExists <- doesFileExist "tasks.txt"  -- Verifica si el archivo "tasks.txt" existe
    if fileExists                            -- Si el archivo existe:
        then do
            result <- readFileSafe "tasks.txt"  -- Intenta leer el contenido del archivo de manera segura
            case result of
                Left ex -> do                     -- Si hay una excepción al leer el archivo:
                    putStrLn $ "Error reading tasks: " ++ show ex  -- Imprime un mensaje de error
                    return []                     -- Devuelve una lista vacía de tareas
                Right contents ->                -- Si la lectura es exitosa:
                    if null contents             -- Si el contenido del archivo está vacío:
                        then return []           -- Devuelve una lista vacía de tareas
                        else case reads contents :: [(TaskList, String)] of  -- Intenta analizar el contenido del archivo en una lista de tareas
                            [(tasks, "")] -> return tasks    -- Si el análisis es exitoso y no hay caracteres sobrantes después de la lista de tareas, devuelve la lista de tareas
                            _ -> do                         -- Si el análisis falla o hay caracteres sobrantes después de la lista de tareas:
                                putStrLn "Error: Invalid format in tasks.txt"  -- Imprime un mensaje de error
                                return []                     -- Devuelve una lista vacía de tareas
        else return []                      -- Si el archivo no existe, devuelve una lista vacía de tareas

-- La función saveTasks guarda la lista de tareas en un archivo "tasks.txt".
saveTasks :: TaskList -> IO ()
saveTasks tasks = do
    result <- writeFileSafe "tasks.txt" (show tasks)  -- Intenta escribir la lista de tareas en el archivo de manera segura
    case result of                                   -- Maneja el resultado de la operación de escritura
        Left ex -> putStrLn $ "Error saving tasks: " ++ show ex  -- Si hay una excepción durante la escritura, imprime un mensaje de error
        Right _ -> putStrLn "Tasks saved. Goodbye!"               -- Si la escritura es exitosa, imprime un mensaje indicando que las tareas fueron guardadas

-- La función principal del programa.
main :: IO ()
main = do
    tasks <- loadTasks                            -- Carga las tareas desde el archivo "tasks.txt"
    putStrLn "Welcome to the Haskell To-Do List Manager"  -- Imprime un mensaje de bienvenida
    mainMenu tasks                               -- Llama a la función mainMenu para mostrar el menú principal, pasando la lista de tareas cargadas

