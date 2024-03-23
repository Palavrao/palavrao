# Palavrão

Palavrão! é um jogo baseado no jogo popular [“Scrabble”](https://users.cs.northwestern.edu/~robby/uc-courses/22001-2008-winter/scrabble.html) de dois jogadores baseado na formação de palavras em um tabuleiro matricial. Para iniciar o jogo cada jogador recebe 7 letras do saco de letras, o primeiro jogador coloca ao menos duas letras passando pelo quadrado central do tabuleiro, o outro deve adicionar letras adjacentes às que foram colocadas anteriormente de forma a gerar novas palavras no sentido vertical e horizontal. Em seguida as rodadas se alternam com um limite de tempo para cada jogada.
Cada peça de letra tem um valor diferente, algumas tendo valores especiais (incluindo dois coringas) e as células do tabuleiro também podem ter pontuações especiais, como pontos extras por formar duas palavras ou palavras com determinado número de letras. Cada jogador em sua rodada pode ou adicionar letras ao tabuleiro para formar palavras, ou trocar uma ou mais letras com letras do saquinho, ou pular sua rodada.
O objetivo do jogo é formar palavras e assim ganhar pontos, ganha quem tiver mais pontos. O jogo termina quando não houverem mais peças no saquinho ou ambos os jogadores realizarem 4 trocas de peças seguidas.
No jogo original, a incidência de cada letra do saquinho é determinada com base na ocorrência de letras na língua inglesa, em nossa versão, essa quantidade será adaptada à [língua portuguesa](https://pt.wikipedia.org/wiki/Scrabble), assim como o [banco de palavras](https://www.ime.usp.br/~pf/dicios/).

## Funcionalidades

* Do jogador:
  * Montar palavras;
  * Se está na primeira jogada, montar unicamente com suas letras (no mínimo duas);
  * Se não está na primeira jogada, montar a partir das letras já existentes no tabuleiro e suas letras.
  * Ver somente suas peças;
  * Ver sua pontuação e do jogador adversário;
  * Ver o tabuleiro e as palavras já montadas;
  * Decidir letra da peça curinga;
  * Trocar peça da mão por uma aleatória do saquinho de letras porém perdendo a jogada;
  * Sistema de login.

* Gerais do jogo:
  * Aleatorização de letras dadas a cada jogador baseada na quantidade de letras predefinidas;
  * Sistema de pontuação baseado nas células de pontuação bônus e nas pontuações predefinidas das letras;
  * Sistema de validação de palavras através de busca em banco de dados;
  * Interface gráfica para o tabuleiro e peças do jogador(Uma matriz representando o tabuleiro, as peças do jogador, seu nome/nick e um contador indicando o tempo para acabar a jogada);
  * Contagem regressiva de tempo a cada jogada;
  * Interface de pontuação;
  * Customização de nomes de jogadores (por meio do login);
  * Banco de dados com palavras aceitas;
  * Acabar o jogo quando acaba a quantidade de jogadas;
  * Ranking de usuários;
  * Persistência do estado do jogo, salvando como o jogo está para depois voltar ao jogo no mesmo estado;
  * Cálculo de pontuação.

## Sistema de Pontuação e Recorrência das Letras

|Pontuação |Letra |
|-----|--------|
|**0** pontos  |**<** (Curinga) ×3|
|**1** ponto   |**A** ×14, **E** ×11, **I** ×10, **O** ×10, **S** ×8, **U** ×7, **M** ×6, **R** ×6, **T** x5|
|**2** pontos  |**D** ×5, **L** ×5, **C** ×4, **P** ×4|
|**3** pontos  |**N** ×4, **B** ×3, **Ç** ×2|
|**4** pontos  |**F** ×2, **G** ×2, **H** ×2, **V** ×2|
|**5** pontos  |**J** ×2|
|**6** pontos  |**Q** ×1|
|**7** pontos  |**X** ×1, **Z** ×1|

## Pontuações

* A pontuação para cada rodada é a soma dos valores das letras em cada palavra formada ou modificada naquela rodada, mais os pontos adicionais obtidos colocando letras em células especiais.
* Células de letra especiais (azul claro e escuro): um quadrado azul claro dobra o valor da letra sobre ele e um quadrado azul escuro triplica o valor da letra sobre ele.
* Células de palavras especiais (rosa e vermelho): a pontuação da palavra inteira é dobrada se uma de suas letras cai sobre uma célula rosa e triplicada se cai sobre uma vermelha. Inclui todas as pontuações das letras especiais antes de dobrar.
* Se uma palavra cobre dois quadrados de palavras especiais, o valor pode ser dobrado ou triplicado novamente.
* Célula especiais só tem efeito na rodada em que são ocupadas, nas rodadas seguintes elas têm valor normal.
* Peças brancas também podem ativar células especiais.
* Se um jogador joga 7 peças de uma vez ele recebe 50 pontos adicionais além do ponto da palavra.
* Em caso de empate, o jogador com menos letras sobrando vence.

### Contribuidores

* [Samuel Lucas](https://github.com/SamuelLucasVM)
* [Helena Sátyro](https://github.com/helenasatyro)
* [Rayanne Macêdo](https://github.com/raiaiaia)
* [Paulo Ricardo](https://github.com/paulorpn)
* [Eliane Tamara](https://github.com/elianetamara)