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


-- Definimos una función llamada parseDate que analiza una cadena de texto para extraer una fecha en formato YYYY-MM-DD 
-- y devuelve Just (year, month, day) si la cadena es válida o Nothing si no lo es
parseDate :: String -> Maybe (Int, Int, Int)
parseDate str =
    -- Llama a la función splitDate para dividir la cadena en partes (año, mes, día)
    case splitDate str of
        -- Si splitDate devuelve una lista con tres elementos (año, mes, día):
        [yearStr, monthStr, dayStr] ->
            -- Intenta convertir cada parte de la fecha en un entero usando readMaybe
            case (readMaybe yearStr, readMaybe monthStr, readMaybe dayStr) of
                -- Si todas las conversiones son exitosas, devuelve Just (year, month, day)
                (Just year, Just month, Just day) -> Just (year, month, day)
                -- Si alguna conversión falla, devuelve Nothing
                _ -> Nothing
        -- Si splitDate no devuelve una lista con tres elementos, devuelve Nothing
        _ -> Nothing
  where
    -- Definimos una función interna llamada splitDate que divide una cadena en partes usando el guion '-' como delimitador
    splitDate :: String -> [String]
    -- Caso base: si la cadena está vacía, devuelve una lista vacía
    splitDate [] = []
    splitDate s@(c:cs)
        -- Si el primer carácter es un guion, llama recursivamente a splitDate con el resto de la cadena
        | c == '-' = splitDate cs
        -- Si el primer carácter no es un guion, usa break para dividir la cadena en dos partes: antes y después del primer guion encontrado
        | otherwise = let (token, rest) = break (== '-') s in
                          -- Devuelve una lista con la primera parte (token) y llama recursivamente a splitDate con la segunda parte (rest)
                          token : splitDate rest

-- La función readMaybe intenta convertir una cadena de texto en un valor de un tipo que implemente la clase de tipos Read
-- y devuelve Just valor si la conversión es exitosa o Nothing si no lo es
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    -- Si reads devuelve una lista con un solo par (valor, cadena vacía), devuelve Just valor
    [(val, "")] -> Just val
    -- Si reads devuelve cualquier otra cosa, devuelve Nothing
    _ -> Nothing

-- Definimos una función llamada generateTaskId que genera un nuevo ID de tarea basado en la lista de tareas actual
generateTaskId :: TaskList -> Int
generateTaskId tasks = if null tasks then 1 else (maximum (map taskId tasks) + 1)
-- Si la lista de tareas está vacía, devuelve 1 como el nuevo ID
-- Si la lista de tareas no está vacía, encuentra el ID máximo en la lista y devuelve ese ID incrementado en 1


-- Definimos una función llamada deleteTask que permite al usuario eliminar una tarea de la lista
deleteTask :: TaskList -> IO ()
deleteTask tasks = do
    putStrLn "Enter the task ID to delete:"  -- Solicita al usuario que ingrese el ID de la tarea a eliminar
    idStr <- getLine                          -- Lee el ID ingresado por el usuario
    case readMaybe idStr of                   -- Intenta convertir el ID ingresado en un entero
        Just id -> do
            let filteredTasks = filter ((/= id) . taskId) tasks  -- Filtra la lista de tareas para eliminar la tarea con el ID especificado
            putStrLn "Task deleted successfully." -- Imprime un mensaje indicando que la tarea ha sido eliminada con éxito
            mainMenu filteredTasks            -- Retorna al menú principal con la lista actualizada de tareas
        Nothing -> do
            putStrLn "Invalid task ID. Please enter a valid number."  -- Si el ID ingresado no es válido, imprime un mensaje de error
            deleteTask tasks                  -- Llama recursivamente a deleteTask para solicitar nuevamente al usuario que ingrese el ID de la tarea a eliminar


-- Definimos una función llamada listTasks que imprime todas las tareas de la lista
listTasks :: TaskList -> IO ()
listTasks tasks = do
    mapM_ printTask tasks  -- Itera sobre la lista de tareas y llama a printTask para imprimir cada tarea
    mainMenu tasks         -- Retorna al menú principal

-- Definimos una función llamada printTask que imprime una tarea
printTask :: Task -> IO ()
printTask task = putStrLn $ show task -- Convierte la tarea en una cadena de texto usando show y la imprime


-- Definimos una función llamada saveTasks que guarda la lista de tareas en un archivo llamado "tasks.txt"
saveTasks :: TaskList -> IO ()
saveTasks tasks = do
    handle <- openFile "tasks.txt" WriteMode  -- Abre el archivo "tasks.txt" en modo de escritura
    hPrint handle tasks                       -- Escribe la lista de tareas en el archivo
    hClose handle                             -- Cierra el archivo
    putStrLn "Tasks saved. Goodbye!"          -- Imprime un mensaje indicando que las tareas han sido guardadas y el programa va a finalizar


-- Definimos una función llamada loadTasks que carga la lista de tareas desde un archivo llamado "tasks.txt"
loadTasks :: IO TaskList
loadTasks = do
    result <- try (readFile "tasks.txt") :: IO (Either SomeException String)
    case result of
        Right content -> return (read content)  -- Si la lectura del archivo es exitosa, convierte el contenido del archivo en una lista de tareas y la devuelve
        Left _ -> return []                     -- Si ocurre un error al leer el archivo, devuelve una lista vacía de tareas


-- Función principal del programa
main :: IO ()
main = do
    tasks <- loadTasks  -- Carga la lista de tareas desde el archivo "tasks.txt"
    mainMenu tasks      -- Llama al menú principal con la lista de tareas cargada

