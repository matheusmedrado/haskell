import System.IO
import System.Random

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    highScoreFile <- readFile "highscore.txt"
    let highScore = read highScoreFile
    putStrLn "\nEntão você decidiu se aventurar no Jogo de Advinhação?!"
    putStrLn ("🏆 O recorde atual é de: " ++ show highScore ++ " 🏆")
    putStrLn ("\nVOCÊ ACHA QUE CONSEGUE BATER ESSE RECORDE E LEVAR O 🏆 PRA CASA???")
    gameLoop highScore

gameLoop :: Int -> IO ()
gameLoop highScore = do
    num <- randomRIO (1, 100)
    putStrLn ("\nEntão vamos começar!\nAdivinhe o número entre 1 e 100")
    attempts <- guessLoop num 0
    putStrLn ("\nAh droga, você me derrotou!! Você conseguiu advinhar em " ++ show attempts ++ " tentativas.")
    let newHighScore = min highScore attempts
    if newHighScore < highScore then do
        putStrLn "\nParabéns! Você bateu o recorde e se provou digno de levar o 🏆 para casa!"
        writeFile "highscore.txt" (show newHighScore)
    else return ()
    putStrLn "Deseja jogar novamente? (s/n)"
    playAgain <- getLine
    if playAgain == "s" then gameLoop newHighScore else return ()

guessLoop :: Int -> Int -> IO Int
guessLoop num attempts = do
    putStrLn ("\nTentativa #" ++ show (attempts + 1)) 
    putStr "-> Me dê o seu melhor palpite: "
    guessStr <- getLine
    let guess = read guessStr
    if guess < num then do
        putStrLn "\nHmmm isso não estã certo! Você foi mais BAIXO do que deveria, tente de novo:"
        guessLoop num (attempts + 1)
    else if guess > num then do
        putStrLn "\nHmmm isso não estã certo! Você foi mais ALTO do que deveria, tente de novo:"
        guessLoop num (attempts + 1)
    else return (attempts + 1)