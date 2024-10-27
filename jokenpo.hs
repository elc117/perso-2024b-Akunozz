import System.Random (randomRIO)
data Escolha = Pedra | Papel | Tesoura deriving (Show, Eq)
type Contadores = (Int, Int, Int)
type Vitorias = (Int, Int, Int, Int)

escolha :: Int -> Maybe Escolha
escolha 1 = Just Pedra
escolha 2 = Just Papel
escolha 3 = Just Tesoura
escolha 9 = Nothing
escolha _ = Nothing



-- Função do jogo jogador1 vs 2
jogadorVsJogador :: Contadores -> Vitorias -> IO (Contadores, Vitorias)
jogadorVsJogador contadores vitorias = do
  putStrLn "Jogador 1: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
  entrada1 <- getLine
  let escolhaJogador1 = escolha (read entrada1 :: Int)
  case escolhaJogador1 of
    Nothing -> return (contadores, vitorias)
    Just jogada1 -> do
      putStrLn "Jogador 2: Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
      entrada2 <- getLine
      let escolhaJogador2 = escolha (read entrada2 :: Int)
      case escolhaJogador2 of
        Nothing -> return (contadores, vitorias)
        Just jogada2 -> do
          putStrLn $ "Jogador 1 escolheu: " ++ show jogada1
          putStrLn $ "Jogador 2 escolheu: " ++ show jogada2
          let (resultado, novasVitorias) = determinarVencedor jogada1 jogada2 vitorias
          putStrLn resultado
          let novosContadores = atualizarContadores [jogada1, jogada2] contadores
          jogadorVsJogador novosContadores novasVitorias

-- Ve quem ganha no PvP
determinarVencedor :: Escolha -> Escolha -> Vitorias -> (String, Vitorias)
determinarVencedor jogada1 jogada2 vitorias =
  (resultado, novasVitorias)
  where
    resultado
      | jogada1 == jogada2 = "Empate!"
      | (jogada1 == Pedra && jogada2 == Tesoura)
          || (jogada1 == Papel && jogada2 == Pedra)
          || (jogada1 == Tesoura && jogada2 == Papel) =
          "Jogador 1 venceu!"
      | otherwise = "Jogador 2 venceu!"

    novasVitorias
      | resultado == "Empate!" = (vitoriaJog1, vitoriaJog2, vitoriaComp, empates + 1)
      | resultado == "Jogador 1 venceu!" = (vitoriaJog1 + 1, vitoriaJog2, vitoriaComp, empates)
      | otherwise = (vitoriaJog1, vitoriaJog2 + 1, vitoriaComp, empates)
    (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = vitorias

--------------------------------------------------------------------------------------------------
-- Função do jogo contra o pc
jogadorVsComputador :: Contadores -> Vitorias -> IO (Contadores, Vitorias)
jogadorVsComputador contadores vitorias = do
  putStrLn "Digite [1] para Pedra, [2] para Papel, [3] para Tesoura (ou [9] para voltar ao menu):"
  entrada <- getLine
  let escolhaJogador = escolha (read entrada :: Int)
  case escolhaJogador of
    Nothing -> return (contadores, vitorias)
    Just jogada -> do
      jogadaComputador <- escolhaComputador
      putStrLn $ "Jogador escolheu: " ++ show jogada
      putStrLn $ "Computador escolheu: " ++ show jogadaComputador
      let (resultado, novasVitorias) = determinarVencedorComputador jogada jogadaComputador vitorias
      putStrLn resultado
      let novosContadores = atualizarContadores [jogada, jogadaComputador] contadores
      jogadorVsComputador novosContadores novasVitorias

-- Ve quem ganha vs computador
determinarVencedorComputador :: Escolha -> Escolha -> Vitorias -> (String, Vitorias)
determinarVencedorComputador jogada jogadaComputador vitorias =
  (resultado, novasVitorias)
  where
    resultado
      | jogada == jogadaComputador = "Empate!"
      | (jogada == Pedra && jogadaComputador == Tesoura)
          || (jogada == Papel && jogadaComputador == Pedra)
          || (jogada == Tesoura && jogadaComputador == Papel) =
          "Jogador venceu!"
      | otherwise = "Computador venceu!"

    novasVitorias
      | resultado == "Empate!" = (vitoriaJog1, vitoriaJog2, vitoriaComp, empates + 1)
      | resultado == "Jogador venceu!" = (vitoriaJog1 + 1, vitoriaJog2, vitoriaComp, empates)
      | otherwise = (vitoriaJog1, vitoriaJog2, vitoriaComp + 1, empates)
    (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = vitorias

-- Gera escolha aleatória do computador
escolhaComputador :: IO Escolha
escolhaComputador = do
  num <- randomRIO (1, 3) :: IO Int
  return $ case num of
    1 -> Pedra
    2 -> Papel
    3 -> Tesoura

--------------------------------------------------------------------------------------------------
-- Atualiza os contadores
atualizarContadores :: [Escolha] -> Contadores -> Contadores
atualizarContadores escolhas (pedra, papel, tesoura) = foldl atualizar (pedra, papel, tesoura) escolhas
  where
    atualizar (p, pp, t) Pedra = (p + 1, pp, t) -- Incrementa Pedra
    atualizar (p, pp, t) Papel = (p, pp + 1, t) -- Incrementa Papel
    atualizar (p, pp, t) Tesoura = (p, pp, t + 1) -- Incrementa Tesoura

-- Exibe contador de jogadas
exibirContadores :: Contadores -> IO ()
exibirContadores (pedra, papel, tesoura) = do
  let (percPedra, percPapel, percTesoura) = calcularPorcentagens (pedra, papel, tesoura)
  putStrLn $ "Número de vezes que Pedra foi jogada: " ++ show pedra ++ " (" ++ show percPedra ++ "%)"
  putStrLn $ "Número de vezes que Papel foi jogado: " ++ show papel ++ " (" ++ show percPapel ++ "%)"
  putStrLn $ "Número de vezes que Tesoura foi jogada: " ++ show tesoura ++ " (" ++ show percTesoura ++ "%)"

-- Exibe contador de vitórias e empates
exibirVitorias :: Vitorias -> IO ()
exibirVitorias (vitoriaJog1, vitoriaJog2, vitoriaComp, empates) = do
  putStrLn $ "Vitórias do Jogador 1: " ++ show vitoriaJog1
  putStrLn $ "Vitórias do Jogador 2: " ++ show vitoriaJog2
  putStrLn $ "Vitórias do Computador: " ++ show vitoriaComp
  putStrLn $ "Número de empates: " ++ show empates


-- Calcula a porcentagem das escolha
calcularPorcentagens :: Contadores -> (Float, Float, Float)
calcularPorcentagens (pedra, papel, tesoura) =
  if total == 0 then (0, 0, 0) else (percPedra, percPapel, percTesoura)
  where
    total = fromIntegral (pedra + papel + tesoura)
    percPedra = (fromIntegral pedra / total) * 100
    percPapel = (fromIntegral papel / total) * 100
    percTesoura = (fromIntegral tesoura / total) * 100

--------------------------------------------------------------------------------------------------
-- Menu
menu :: Contadores -> Vitorias -> IO ()
menu contadores vitorias = do
  putStrLn "Escolha o modo de jogo:"
  putStrLn "[1] Jogador vs Jogador"
  putStrLn "[2] Jogador vs Computador"
  putStrLn "[3] Exibir quantidade de jogadas"
  putStrLn "[4] Exibir quantidade de vitórias/empates"
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
    0 -> putStrLn "Programa encerrado."
    _ -> do
      putStrLn "Escolha inválida, tente novamente."
      menu contadores vitorias

-- Função main
main :: IO ()
main = do
  putStrLn "Pedra, Papel ou Tesoura!"
  menu (0, 0, 0) (0, 0, 0, 0)
