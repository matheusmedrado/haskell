import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import System.Exit (exitSuccess)

data Student = Student {
    studentId :: Int,
    firstName :: String,
    lastName :: String,
    age :: Int
}

type Database = [Student]

initialDatabase :: Database
initialDatabase = []

addStudent :: Database -> Student -> Database
addStudent db student = student : db

filterStudentById :: Database -> Int -> [Student]
filterStudentById db id = filter (\student -> studentId student == id) db

updateStudent :: Database -> Int -> Student -> Database
updateStudent db id updatedStudent = map (\student -> if studentId student == id then updatedStudent else student) db

showStudent :: Student -> String
showStudent (Student id firstName lastName age) = "Estudante {ID do estudante = " ++ show id ++ ", Primeiro nome = " ++ firstName ++ ", Sobrenome = " ++ lastName ++ ", Idade = " ++ show age ++ "}"

menu :: Database -> IO ()
menu db = do
    putStrLn "\n1 ➤ Adicionar estudante"
    putStrLn "2 ➤ Recuperar estudante por ID"
    putStrLn "3 ➤ Atualizar informações do estudante"
    putStrLn "4 ➤ Sair"
    putStr "Escolha uma opção: "
    option <- getLine
    case option of
        "1" -> do
            putStr "ID: "
            id <- readLn
            putStr "Nome: "
            firstName <- getLine
            putStr "Sobrenome: "
            lastName <- getLine
            putStr "Idade: "
            age <- readLn
            let student = Student id firstName lastName age
            let updatedDb = addStudent db student
            putStrLn "\n⌨ Estudante adicionado:"
            putStrLn (showStudent student)
            menu updatedDb
        "2" -> do
            putStr "ID: "
            id <- readLn
            let students = filterStudentById db id
            putStrLn " 🔍 Estudante encontrado:"
            mapM_ (putStrLn . showStudent) students
            menu db
        "3" -> do
            putStr "ID: "
            id <- readLn
            putStr "Novo nome: "
            firstName <- getLine
            putStr "Novo sobrenome: "
            lastName <- getLine
            putStr "Nova idade: "
            age <- readLn
            let updatedStudent = Student id firstName lastName age
            let updatedDb = updateStudent db id updatedStudent
            putStrLn " ☝ Informações do estudante atualizadas:"
            putStrLn (showStudent updatedStudent)
            menu updatedDb
        "4" -> do
            putStrLn " ⍈ Encerrando o programa..."
            exitSuccess
        _ -> do
            putStrLn " ☒ Opção inválida!"
            menu db

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    menu initialDatabase
