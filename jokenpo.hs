import System.Random (randomRIO)

-- Define as opções do jogo: Pedra, Papel ou Tesoura
data Escolha = Pedra | Papel | Tesoura deriving (Show, Eq)

-- Contadores de escolhas e vitórias
type Contadores = (Int, Int, Int)  -- (Pedra, Papel, Tesoura)
type Vitorias = (Int, Int, Int, Int)  -- (Vitórias Jogador 1, Vitórias Jogador 2, Vitórias Computador, Empates)

-- Converte um número em uma escolha do jogo
escolha :: Int -> Maybe Escolha
escolha 1 = Just Pedra
escolha 2 = Just Papel
escolha 3 = Just Tesoura
escolha 9 = Nothing  -- 9 retorna ao menu
escolha _ = Nothing

-- Jogo entre dois jogadores
jogadorVsJogador :: Contadores -> Vitorias -> IO (Contadores, Vitorias)
jogadorVsJogador contadores vitorias = do
    putStrLn "Jogador 1: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    entrada1 <- getLine
    let escolhaJogador1 = escolha (read entrada1 :: Int)

    case escolhaJogador1 of
        Nothing -> return (contadores, vitorias)  -- Volta ao menu se a entrada for inválida
        Just jogada1 -> do
            putStrLn "Jogador 2: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
            entrada2 <- getLine
            let escolhaJogador2 = escolha (read entrada2 :: Int)
            case escolhaJogador2 of
                Nothing -> return (contadores, vitorias)  -- Volta ao menu se a entrada for inválida
                Just jogada2 -> do
                    -- Determina o vencedor e atualiza vitórias
                    let (resultado, novasVitorias) = determinarVencedor jogada1 jogada2 vitorias
                    putStrLn resultado
                    -- Atualiza os contadores das jogadas
                    let novosContadores = atualizarContadores [jogada1, jogada2] contadores
                    -- Chama a função recursivamente para continuar o jogo
                    jogadorVsJogador novosContadores novasVitorias

-- Determina o vencedor entre os jogadores e atualiza o número de vitórias e empates
determinarVencedor :: Escolha -> Escolha -> Vitorias -> (String, Vitorias)
determinarVencedor jogada1 jogada2 vitorias = 
    (resultado, novasVitorias)
  where
    resultado
      | jogada1 == jogada2 = "Empate!"  -- Se as jogadas forem iguais, é um empate
      | (jogada1 == Pedra && jogada2 == Tesoura) ||
        (jogada1 == Papel && jogada2 == Pedra) ||
        (jogada1 == Tesoura && jogada2 == Papel) = "Jogador 1 venceu!"  -- Jogador 1 vence
      | otherwise = "Jogador 2 venceu!"  -- Jogador 2 vence
    
    -- Atualiza as vitórias e empates de acordo com o resultado
    novasVitorias
      | resultado == "Empate!" = (vitoriaJog1, vitoriaJog2, vitoriaComp, empates + 1)
      | resultado == "Jogador 1 venceu!" = (vitoriaJog1 + 1, vitoriaJog2, vitoriaComp, empates)
      | otherwise = (vitoriaJog1, vitoriaJog2 + 1, vitoriaComp, empates)
    
    -- Extrai as contagens atuais de vitórias e empates
    (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = vitorias

-- Jogo entre o jogador e o computador
jogadorVsComputador :: Contadores -> Vitorias -> IO (Contadores, Vitorias)
jogadorVsComputador contadores vitorias = do
    putStrLn "Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
    entrada <- getLine
    let escolhaJogador = escolha (read entrada :: Int)

    case escolhaJogador of
        Nothing -> return (contadores, vitorias)  -- Volta ao menu se a entrada for inválida
        Just jogada -> do
            -- Gera uma escolha aleatória para o computador
            jogadaComputador <- escolhaComputador
            -- Determina o vencedor e atualiza as vitórias
            let (resultado, novasVitorias) = determinarVencedorComputador jogada jogadaComputador vitorias
            putStrLn resultado
            -- Atualiza os contadores das jogadas
            let novosContadores = atualizarContadores [jogada, jogadaComputador] contadores
            -- Chama a função recursivamente para continuar o jogo
            jogadorVsComputador novosContadores novasVitorias

-- Determina o vencedor entre o jogador e o computador
determinarVencedorComputador :: Escolha -> Escolha -> Vitorias -> (String, Vitorias)
determinarVencedorComputador jogada jogadaComputador vitorias = 
    (resultado, novasVitorias)
  where
    resultado
      | jogada == jogadaComputador = "Empate!"  -- Se as jogadas forem iguais, é um empate
      | (jogada == Pedra && jogadaComputador == Tesoura) ||
        (jogada == Papel && jogadaComputador == Pedra) ||
        (jogada == Tesoura && jogadaComputador == Papel) = "Jogador venceu!"  -- Jogador vence
      | otherwise = "Computador venceu!"  -- Computador vence
    
    -- Atualiza as vitórias e empates de acordo com o resultado
    novasVitorias
      | resultado == "Empate!" = (vitoriaJog1, vitoriaJog2, vitoriaComp, empates + 1)
      | resultado == "Jogador venceu!" = (vitoriaJog1 + 1, vitoriaJog2, vitoriaComp, empates)
      | otherwise = (vitoriaJog1, vitoriaJog2, vitoriaComp + 1, empates)
    
    -- Extrai as contagens atuais de vitórias e empates
    (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = vitorias

-- Gera uma escolha aleatória para o computador (Pedra, Papel ou Tesoura)
escolhaComputador :: IO Escolha
escolhaComputador = do
    num <- randomRIO (1, 3) :: IO Int
    return $ case num of
        1 -> Pedra
        2 -> Papel
        3 -> Tesoura

-- Atualiza os contadores de acordo com as escolhas feitas
atualizarContadores :: [Escolha] -> Contadores -> Contadores
atualizarContadores escolhas (pedra, papel, tesoura) = foldl atualizar (pedra, papel, tesoura) escolhas
  where
    atualizar (p, pp, t) Pedra = (p + 1, pp, t)  -- Incrementa o contador de Pedra
    atualizar (p, pp, t) Papel = (p, pp + 1, t)  -- Incrementa o contador de Papel
    atualizar (p, pp, t) Tesoura = (p, pp, t + 1)  -- Incrementa o contador de Tesoura

-- Exibe quantas vezes cada escolha foi feita
exibirContadores :: Contadores -> IO ()
exibirContadores (pedra, papel, tesoura) = do
    putStrLn $ "Número de vezes que Pedra foi jogada: " ++ show pedra
    putStrLn $ "Número de vezes que Papel foi jogado: " ++ show papel
    putStrLn $ "Número de vezes que Tesoura foi jogada: " ++ show tesoura

-- Exibe o total de vitórias e empates de cada jogador
exibirVitorias :: Vitorias -> IO ()
exibirVitorias (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = do
    putStrLn $ "Vitórias do Jogador 1: " ++ show vitoriaJog1
    putStrLn $ "Vitórias do Jogador 2: " ++ show vitoriaJog2
    putStrLn $ "Vitórias do Computador: " ++ show vitoriaComp
    putStrLn $ "Número de empates: " ++ show empates

-- Função principal com menu de opções
menu :: Contadores -> Vitorias -> IO ()
menu contadores vitorias = do
    putStrLn "Escolha o modo de jogo:"
    putStrLn "[1] Jogador vs Jogador"
    putStrLn "[2] Jogador vs Computador"
    putStrLn "[3] Exibir contadores de jogadas"
    putStrLn "[4] Exibir contadores de vitórias"
    putStrLn "[0] Encerrar o programa"
    entrada <- getLine
    let opcao = read entrada :: Int
    case opcao of
        1 -> jogadorVsJogador contadores vitorias >>= uncurry menu
        2 -> jogadorVsComputador contadores vitorias >>= uncurry menu
        3 -> do
            exibirContadores contadores
            menu contadores vitorias
        4 -> do
            exibirVitorias vitorias
            menu contadores vitorias
        0 -> putStrLn "Programa encerrado!"
        _ -> do
            putStrLn "Escolha inválida, tente novamente."
            menu contadores vitorias

-- Função principal que inicia o programa
main :: IO ()
main = do
    putStrLn "Bem-vindo ao jogo Pedra, Papel e Tesoura!"
    menu (0, 0, 0) (0, 0, 0, 0)  -- Inicializa contadores e vitórias
