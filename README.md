<h1>Pedra, Papel e Tesoura em Haskell</h1>

Nome: Breno Rosa</br>
Curso: Sistemas de Informação</br>

<h2>Como rodar pelo VScode:</h2>
1. Tenha instalado o Haskell em seu ambiente.</br>
2. Digite "ghci" no terminal.</br>
3. No ghci carregue o programa com ":l jokenpo.hs".</br>
4. Depois de carregado é só digitar "main".</br>
5. Escolha as opções do menu para jogar.</br>

<h2>Objetivo:</h2>
Implementar um jogo de Pedra, Papel ou Tesoura com opção de jogar contra o computador e jogador vs jogador. A cada turno, o jogador e o computador fazem suas jogadas e o programa registra quem ganhou. Guarda os resultados e, depois de um certo número de jogadas, mostrar algumas estatísticas gerais.</br>

<h2>Processo de Desenvolvimento</h2>
Inicialmente, foi implementado as funções para permitir o jogo entre dois jogadores e entre jogador e computador. Além disso, também foram elaboradas as funções que avaliam as jogadas e definem o vencedor (Após muita dor de cabeça para entender como funcionaria o jogo). Para finalizar, foi feito a adição dos contadores que se incrementam a cada escolha realizada.</br>
Referências Consultadas: Documentação oficial do Haskell, vídeos no YouTube sobre Haskell, exemplos de códigos da internet.</br>

<h2>Resultado Final</h2>
Este projeto é um jogo de Pedra, Papel e Tesoura desenvolvido em Haskell. O programa oferece um menu com as opções:</br>
<li>Jogador vs Jogador</li>
<li>Jogador vs Computador</li>
<li>Exibir quantidade de jogadas</li>
<li>Exibir quantidade de vitórias/empates</li>
<li>Encerrar o programa</li>
Escolhida a opção "Jogador vs Jogador", o programa solicita para o jogador1 e jogador2 fazerem a escolha entre pedra [1], papel [2] ou tesoura [3], respectivamente. Realizadas as jogadas, o programa printa a escolha dos dois e mostra o vencedor ou printa "Empate" em caso de empate.</br>
Na escolha contra o computador ocorre o mesmo processo, porém com uma função do System.Random o computador gera aleatóriamente a sua escolha.</br>
Na opção de Exibir quantidade de jogadas o programa mostra a quantidade de vezes em que foi lançada cada escolha e a sua porcentagem em relação a quantidade total de opções escolhidas.</</br>
Por último, na opção de Exibir quantidade de vitórias/empates o programa mostra a quantidade de empates do jogo inteiro e a quantas vezes cada jogador ganhou, sendo o jogador1 contando tanto para o jogo contra o jogador2 quanto para contra o computador.</br>
Para demonstrar a execução do programa, aqui está um link para um vídeo curto no Youtube:</br>
https://youtu.be/wsl2NEeOSDc </br>

<h2>Referências e Créditos</h2>
https://haskell.pesquisa.ufabc.edu.br/haskell/</br>
https://www.youtube.com/playlist?list=PLYItvall0TqJ25sVTLcMhxsE0Hci58mpQ</br>
https://www.youtube.com/watch?v=fP0srOQVGB8</br>
https://www.haskell.org/documentation/</br>
https://www.youtube.com/watch?v=kpBJ-FwKNyE</br>
Prompts:</br>
Em geral foi usado recursos de IA para ter ideias do como fazer uma certa função e também para interpretar vários erros no ghci, além de melhorar algumas lógicas do programa.</br>
