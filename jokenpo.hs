import System.Random (randomRIO)
-- Tipo de dados representando as escolhas possíveis do jogo
data Escolha = Pedra | Papel | Tesoura deriving (Show, Eq)
-- Contadores para cada escolha: (Pedra, Papel, Tesoura)
type Contadores = (Int, Int, Int)

-- Converte um número inteiro para uma escolha de jogo
escolha :: Int -> Maybe Escolha
escolha 1 = Just Pedra
escolha 2 = Just Papel
escolha 3 = Just Tesoura
escolha 9 = Nothing
escolha _ = Nothing

-- Função para o modo de jogo Jogador vs Jogador
jogadorVsJogador :: Contadores -> IO Contadores
jogadorVsJogador contadores = do
    putStrLn "Jogador 1: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    entrada1 <- getLine
    let escolhaJogador1 = escolha (read entrada1 :: Int)

    case escolhaJogador1 of
        Nothing -> return contadores
        Just jogada1 -> do
            putStrLn "Jogador 2: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
            entrada2 <- getLine
            let escolhaJogador2 = escolha (read entrada2 :: Int)
            case escolhaJogador2 of
                Nothing -> return contadores
                Just jogada2 -> do
                    putStrLn $ determinarVencedor jogada1 jogada2
                    jogadorVsJogador (atualizarContadores jogada1 (atualizarContadores jogada2 contadores))

-- Função que determina quem venceu entre dois jogadores
determinarVencedor :: Escolha -> Escolha -> String
determinarVencedor jogada1 jogada2 = 
    "Jogador 1 escolheu: " ++ show jogada1 ++ "\n" ++
    "Jogador 2 escolheu: " ++ show jogada2 ++ "\n" ++
    resultado
  where
    resultado
      | jogada1 == jogada2 = "Empate!"
      | (jogada1 == Pedra && jogada2 == Tesoura) ||
        (jogada1 == Papel && jogada2 == Pedra) ||
        (jogada1 == Tesoura && jogada2 == Papel) = "Jogador 1 venceu!"
      | otherwise = "Jogador 2 venceu!"

-- Função para o modo de jogo Jogador vs Computador
jogadorVsComputador :: Contadores -> IO Contadores
jogadorVsComputador contadores = do
    putStrLn "Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    entrada <- getLine
    let escolhaJogador = escolha (read entrada :: Int)

    case escolhaJogador of
        Nothing -> return contadores
        Just jogada -> do
            jogadaComputador <- escolhaComputador
            putStrLn $ determinarVencedorComputador jogada jogadaComputador
            jogadorVsComputador (atualizarContadores jogada (atualizarContadores jogadaComputador contadores))

-- Função que determina o vencedor entre jogador e computador
determinarVencedorComputador :: Escolha -> Escolha -> String
determinarVencedorComputador jogada jogadaComputador = 
    "Jogador escolheu: " ++ show jogada ++ "\n" ++
    "Computador escolheu: " ++ show jogadaComputador ++ "\n" ++
    resultado
  where
    resultado
      | jogada == jogadaComputador = "Empate!"
      | (jogada == Pedra && jogadaComputador == Tesoura) ||
        (jogada == Papel && jogadaComputador == Pedra) ||
        (jogada == Tesoura && jogadaComputador == Papel) = "Jogador venceu!"
      | otherwise = "Computador venceu!"

-- Gera uma escolha aleatória para o computador (Pedra, Papel ou Tesoura)
escolhaComputador :: IO Escolha
escolhaComputador = do
    num <- randomRIO (1, 3) :: IO Int
    return $ case num of
        1 -> Pedra
        2 -> Papel
        3 -> Tesoura

-- Atualiza os contadores de acordo com a escolha realizada
atualizarContadores :: Escolha -> Contadores -> Contadores
atualizarContadores Pedra (pedra, papel, tesoura) = (pedra + 1, papel, tesoura)
atualizarContadores Papel (pedra, papel, tesoura) = (pedra, papel + 1, tesoura)
atualizarContadores Tesoura (pedra, papel, tesoura) = (pedra, papel, tesoura + 1)

-- Exibe o total de vezes que cada escolha foi feita
exibirContadores :: Contadores -> IO ()
exibirContadores (pedra, papel, tesoura) = do
    putStrLn $ "Número de vezes que Pedra foi jogada: " ++ show pedra
    putStrLn $ "Número de vezes que Papel foi jogado: " ++ show papel
    putStrLn $ "Número de vezes que Tesoura foi jogada: " ++ show tesoura

-- Função principal com menu de opções
menu :: Contadores -> IO ()
menu contadores = do
    putStrLn "Escolha o modo de jogo:"
    putStrLn "[1] Jogador vs Jogador"
    putStrLn "[2] Jogador vs Computador"
    putStrLn "[3] Exibir contadores de jogadas"
    putStrLn "[0] Encerrar o programa"
    entrada <- getLine
    let opcao = read entrada :: Int
    case opcao of
        1 -> jogadorVsJogador contadores >>= menu
        2 -> jogadorVsComputador contadores >>= menu
        3 -> do
            exibirContadores contadores
            menu contadores
        0 -> putStrLn "Programa encerrado!"
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            menu contadores

-- Função inicial que inicia o programa com contadores zerados
main :: IO ()
main = menu (0, 0, 0)
