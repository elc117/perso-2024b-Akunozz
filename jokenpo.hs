import System.Random (randomRIO)

data Escolha = Pedra | Papel | Tesoura deriving (Show, Eq)

-- Função para converter número em escolha
escolha :: Int -> Maybe Escolha
escolha 1 = Just Pedra
escolha 2 = Just Papel
escolha 3 = Just Tesoura
escolha 9 = Nothing
escolha _ = Nothing

-- Função para o modo Player vs Player, agora com contadores
playerVSplayer :: Int -> Int -> IO ()
playerVSplayer vitoriasJogador1 vitoriasJogador2 = do
    putStrLn "Jogador 1: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    input1 <- getLine
    let escolhaJogador1 = escolha (read input1 :: Int)

    case escolhaJogador1 of
        Nothing -> jogar vitoriasJogador1 vitoriasJogador2 0
        Just jogador1 -> do
            putStrLn "Jogador 2: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
            input2 <- getLine
            let escolhaJogador2 = escolha (read input2 :: Int)
            case escolhaJogador2 of
                Nothing -> jogar vitoriasJogador1 vitoriasJogador2 0
                Just jogador2 -> do
                    let resultado = determinarVencedor jogador1 jogador2
                    putStrLn resultado
                    let (v1, v2) = atualizarVitorias resultado vitoriasJogador1 vitoriasJogador2
                    putStrLn $ "Vitórias Jogador 1: " ++ show v1 ++ ", Vitórias Jogador 2: " ++ show v2
                    playerVSplayer v1 v2

-- Função para determinar o vencedor no modo Player vs Player
determinarVencedor :: Escolha -> Escolha -> String
determinarVencedor jogador1 jogador2 = 
    "Jogador 1 escolheu: " ++ show jogador1 ++ "\n" ++
    "Jogador 2 escolheu: " ++ show jogador2 ++ "\n" ++
    resultado
  where
    resultado
      | jogador1 == jogador2 = "Empate!"
      | (jogador1 == Pedra && jogador2 == Tesoura) ||
        (jogador1 == Papel && jogador2 == Pedra) ||
        (jogador1 == Tesoura && jogador2 == Papel) = "Jogador 1 venceu!"
      | otherwise = "Jogador 2 venceu!"

-- Função para o modo Player vs Computador, agora com contador para o jogador e computador
playerVScomputador :: Int -> Int -> IO ()
playerVScomputador vitoriasJogador vitoriasComputador = do
    putStrLn "Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    input <- getLine
    let escolhaJogador = escolha (read input :: Int)

    case escolhaJogador of
        Nothing -> jogar vitoriasJogador 0 vitoriasComputador
        Just jogador -> do
            computador <- escolhaComputador
            let resultado = determinarVencedorComp jogador computador
            putStrLn resultado
            let (vJogador, vComp) = atualizarVitoriasComp resultado vitoriasJogador vitoriasComputador
            putStrLn $ "Vitórias Jogador: " ++ show vJogador ++ ", Vitórias Computador: " ++ show vComp
            playerVScomputador vJogador vComp

-- Função para determinar o vencedor no modo Player vs Computador
determinarVencedorComp :: Escolha -> Escolha -> String
determinarVencedorComp jogador1 jogador2 = 
    "Jogador 1 escolheu: " ++ show jogador1 ++ "\n" ++
    "Computador escolheu: " ++ show jogador2 ++ "\n" ++
    resultado
  where
    resultado
      | jogador1 == jogador2 = "Empate!"
      | (jogador1 == Pedra && jogador2 == Tesoura) ||
        (jogador1 == Papel && jogador2 == Pedra) ||
        (jogador1 == Tesoura && jogador2 == Papel) = "Jogador 1 venceu!"
      | otherwise = "Computador venceu!"

-- Função para escolha aleatória do computador
escolhaComputador :: IO Escolha
escolhaComputador = do
    num <- randomRIO (1, 3) :: IO Int
    return $ case num of
        1 -> Pedra
        2 -> Papel
        3 -> Tesoura

-- Função para atualizar o contador de vitórias no modo Player vs Player
atualizarVitorias :: String -> Int -> Int -> (Int, Int)
atualizarVitorias resultado vitoriasJogador1 vitoriasJogador2
    | "Jogador 1 venceu!" `elem` words resultado = (vitoriasJogador1 + 1, vitoriasJogador2)
    | "Jogador 2 venceu!" `elem` words resultado = (vitoriasJogador1, vitoriasJogador2 + 1)
    | otherwise = (vitoriasJogador1, vitoriasJogador2)

-- Função para atualizar o contador de vitórias no modo Player vs Computador
atualizarVitoriasComp :: String -> Int -> Int -> (Int, Int)
atualizarVitoriasComp resultado vitoriasJogador vitoriasComputador
    | "Jogador 1 venceu!" `elem` words resultado = (vitoriasJogador + 1, vitoriasComputador)
    | "Computador venceu!" `elem` words resultado = (vitoriasJogador, vitoriasComputador + 1)
    | otherwise = (vitoriasJogador, vitoriasComputador)

-- Nova função para exibir as vitórias no menu
mostrarVitorias :: Int -> Int -> Int -> IO ()
mostrarVitorias vitoriasJogador1 vitoriasJogador2 vitoriasComputador = do
    putStrLn $ "Vitórias Jogador 1: " ++ show vitoriasJogador1
    putStrLn $ "Vitórias Jogador 2: " ++ show vitoriasJogador2
    putStrLn $ "Vitórias Computador: " ++ show vitoriasComputador
    jogar vitoriasJogador1 vitoriasJogador2 vitoriasComputador

-- Função principal para iniciar o jogo com os contadores de vitórias
jogar :: Int -> Int -> Int -> IO ()
jogar vitoriasJogador1 vitoriasJogador2 vitoriasComputador = do
    putStrLn "Escolha o modo de jogo:"
    putStrLn "[1] Jogar Player vs Player"
    putStrLn "[2] Jogar Player vs Computador"
    putStrLn "[4] Mostrar contagem de vitórias"
    putStrLn "[0] Encerrar o programa"
    input <- getLine
    let modo = read input :: Int
    case modo of
        1 -> playerVSplayer vitoriasJogador1 vitoriasJogador2
        2 -> playerVScomputador vitoriasJogador1 vitoriasComputador
        4 -> mostrarVitorias vitoriasJogador1 vitoriasJogador2 vitoriasComputador
        0 -> putStrLn "Programa encerrado!"
        _ -> do
            putStrLn "Escolha inválida, tente novamente."
            jogar vitoriasJogador1 vitoriasJogador2 vitoriasComputador
