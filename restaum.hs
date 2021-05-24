-- Vinícius Renato Rocha Geraldo

module RestaUm where

import System.IO

-- Tabuleiro do jogo:
type GBoard = [[Char]]
type Teste = [[Int]]

-- Tabuleiro Inicial do jogo: 7x7 
gBoard :: GBoard
gBoard = [[' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          ['*','*','*',' ','*','*','*'],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*','*','*',' ',' '],
          [' ',' ','*','*','*',' ',' ']]

-- outro exemplo de tabuleiro de jogo que pode ser usado para testes.
-- notem que nesse tabuleiro as peças estão presas e nenhuma pode se mover, ou seja,
-- é um exemplo de fim de jogo:
tBoard :: GBoard
tBoard = [[' ',' ','*',' ','*',' ',' '],
          [' ',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*',' ','*',' ',' '],
          ['*','*','*','*','*','*','*'],
          [' ',' ','*',' ','*',' ',' '],
          [' ',' ','*',' ','*',' ',' ']]

-- este exemplo é o resultado de mover a peça (1,3) no tabuleiro inicial gBoard:

eBoard :: GBoard
eBoard = ["  ***  ",
          "  * *  ",
          "*** ***",
          "*******",
          "*******",
          "  ***  ",
          "  ***  "] 

test1 :: Teste
test1 = [[2,6,8,10,1,3,22],
          [15,33,11,44,55,20,36],
          [34,67,19,27,23,12,14]]


-----------------------------------------------
---------------------------------------------
-- A ideia das próximas funções é permitir que a gente acesse uma lista usando um indice,
-- como se fosse um vetor
-- Vetores e matrizes começam na posição zero
-----------------------------------------

-- gArr (get array): recebe uma posicao (p) e uma lista (vetor) e devolve o elemento
-- na posição p do vetor

-- gArr :: Int -> [t] -> t
gArr :: Int -> [t] -> t
gArr p [] = error "lista vazia"
gArr 0 (t:ts) = t
gArr p (t:ts) = gArr (p-1) ts

getTam :: [t] -> Int
getTam [] = 0
getTam (t:ts) = 1 + getTam ts

-- uArr (update array): recebe uma posição (p), um novo valor (v), e uma lista (vetor) e devolve um
-- novo vetor com o valor v na posição p, substituindo o valor que estava nessa posição 
-- uArr :: Int -> a -> [a] -> [a]
uArr :: Int -> a -> [a] -> [a]
uArr p v [] = error "Posição inválida (uArr)"
uArr 0 v (a:as) = v : as
uArr p v (a:as) = a : uArr (p-1) v as

-- Agora, usando as operações anteriores, podemos criar funções para acessar o tabuleiro, como 
-- se ele fosse uma matriz:

-- gPos (get position) recebe linha (l), coluna (c) (não precisa validar) e um tabuleiro. Devolve o elemento na posicao
-- tabuleiro[l,c]. Usar gArr na implementação

gPos :: (Int,Int) -> [[a]] -> a
gPos (l,c) tabuleiro = gArr c (gArr l tabuleiro)
-- uPos (update position):  recebe uma posição no tabuleiro (linha e coluna), um novo valor e um tabuleiro. Devolve 
-- o tabuleiro modificado com o novo valor na posiçao l x c


-- uPos :: (Int,Int) ->  a -> [[a]] -> [[a]]
uPos :: (Int, Int) -> a -> [[a]] -> [[a]]
uPos (l,c) v [] = error "Posição no tabuleiro Inválida"
uPos (0,c) v (x:xs) = uArr c v x : xs
uPos (l,c) v (x:xs) = x : uPos (l-1, c) v xs
------------------------
-- Funções relativas  a lógica do jogo
--
-- ---------------------------------

-- isValidPos: recebe uma linha e uma coluna e retorna um booleano dizendo
-- se essa posição é uma posição válida no tabuleiro. Uma posição válida é uma
-- posição dentro da cruz do tabuleiro de inicio de jogo. por exemplo, (0,0) e (-4,-20) são
-- posições inválidas, (3,3), (6,3) e (3,6) são posições válidas 
-- Exemplos:
-- *RestaUm> isValidPos (3,3)
-- True
-- *RestaUm> isValidPos (-4,20)
-- False

-- isValidPos :: (Int,Int) -> Bool
isValidPos :: (Int, Int) -> Bool
isValidPos (x,y)
  | ((x == 1 || x == 0) && (y == 1 || y == 0)) = False
  | ((x == 5 || x == 6) && (y == 1 || y == 0)) = False
  | ((x == 1 || x == 0) && (y == 5 || y == 6)) = False
  | ((x == 5 || x == 6) && (y == 5 || y == 6)) = False
  | (x==3 && y==3) = True
  | (x>=0 && x<7 && y>=0 && y<7) = True
  | otherwise = False

-- moves: essa função recebe uma linha e uma coluna, e calcula os quatro movimentos possíveis partindo
-- dessa linha e coluna
-- Para uma determinada posição, existem quatro movimentos: para cima, para baixo, esquerda
-- e direita. Cada movimento, envolve duas posições. Por exemplo:
--                              (1,3)
--                              (2,3) 
--                                ^
--                                | 
--               (3,1) (3,2) <- (3,3) -> (3,4) (3,5)
--                                |
--                              (4,3)
--                              (5,3)
-- 
-- *RestaUm> moves 3 3 
-- [[(3,2),(3,1)],[(3,4),(3,5)],[(4,3),(5,3)],[(2,3),(1,3)]]

-- A função moves, não verifica se as coordenadas recebidas ou geradas são válidas, apenas
-- faz o cálculo dos movimentos:
--
-- *RestaUm> moves 0 0
-- [[(0,-1),(0,-2)],[(0,1),(0,2)],[(1,0),(2,0)],[(-1,0),(-2,0)]]
--

--moves :: Int -> Int -> [[(Int,Int)]]
moves :: Int -> Int -> [[(Int, Int)]]
moves x y = [[(x, y-1), (x, y-2)], [(x, y+1), (x, y+2)], [(x+1, y), (x+2, y)], [(x-1, y), (x-2, y)]]

-- isValidPosMove, recebe um movimento (lista de duas posições no tabuleiro), e verifica se as
-- posições nesse movimento são válidas. Aqui dá pra usar duas aplicações da função isValidPos.

-- *RestaUm> isValidPosMove [(4,3),(5,3)]
-- True
-- *RestaUm> isValidPosMove [(0,-1),(0,-2)]
-- False


-- isValidPosMove :: [(Int,Int)] -> Bool
isValidPosMove :: [(Int, Int)] -> Bool
isValidPosMove [] = True
isValidPosMove ((x,y):xs) = isValidPos (x,y) && isValidPosMove xs

-- isEmpty: recebe uma linha, uma coluna e um tabuleiro e diz se a posição correspondente
-- está vazia (contém espaço) ou não. As posições recebidas como entrada são sempre válidas

-- *RestaUm> isEmpty (4,3) gBoard 
-- False
-- *RestaUm> isEmpty (3,3) gBoard 
-- True

-- isEmpty :: (Int,Int) -> GBoard -> Bool
isEmpty :: (Int, Int) -> GBoard -> Bool
isEmpty (x,y) tabuleiro 
     | (gPos (x,y) tabuleiro /= ' ') = False
     | otherwise = True
     
-- isValidMove: Recebe o tabuleiro corrente do jogo e um movimento (lista com duas posições no tabuleiro)
-- e verifica se o movimento pode ser feito no tabuleiro, ou seja, a primeira posição deve conter uma peça
-- e a segunda deve estar vazia. Aqui pode-se usar a função isEmpty definida anteriormente.
-- Essa função não precisa validar nenhuma das posições.

-- *RestaUm> isValidMove gBoard [(2,3),(3,3)]
-- True
-- *RestaUm> isValidMove gBoard [(2,2),(2,1)]
-- False

-- isValidMove :: GBoard -> [(Int,Int)] -> Bool
isValidMove :: GBoard -> [(Int, Int)] -> Bool
isValidMove tabuleiro [(x1,y1),(x2,y2)] = not(isEmpty (x1,y1) tabuleiro) && isEmpty (x2,y2) tabuleiro

-- validMoves
-- Recebe um tabuleiro, uma posição e gera uma lista de movimentos válidos dentro do tabuleiro.
-- Essa função pode ser implementada usando moves para gerar todos os movimentos possíveis, logo
-- após, a lista resultante pode ser filtrada, primeiramente usando isValidPosMove, e depois usando
-- isValidMove. Se a posição de entrada da função não for uma posição válida no tabueiro, validMoves
-- retorna uma lista vazia

-- *RestaUm> validMoves gBoard (0,0)
-- []
-- *RestaUm> validMoves gBoard (1,3)
-- [[(2,3),(3,3)]]

-- validMoves :: GBoard -> (Int, Int) -> [[(Int,Int)]]
{--}
validMoves :: GBoard -> (Int, Int) -> [[(Int, Int)]]
validMoves tabuleiro (x,y)
    | isValidPos(x,y) == False || isEmpty (x,y) tabuleiro == True = []
    | otherwise = filter (isValidMove tabuleiro) (filter (isValidPosMove) (moves x y))

--- move
--- recebe uma posiçao inicial, os movimentos a serem feitos partindo dessa posição,
--- o tabuleiro do jogo, e devolve o tabuleiro modificado com esse movimento, ou seja
-- a posição de partida deve ser esvaziada, a primeira posição do movimento deve
-- ser esvaziada e a segunda posição do movimento deve agora conter uma peça. As posições
-- recebidas e o movimento são sempre válidos
-- 
-- move (1,3) [(2,3),(3,3)] gBoard 
--["  ***  ",
-- "  * *  ",
-- "*** ***",
-- "*******",
-- "*******",
-- "  ***  ",
-- "  ***  "]

-- move :: (Int,Int) -> [(Int,Int)] -> GBoard -> GBoard
move :: (Int, Int) -> [(Int, Int)] -> GBoard -> GBoard
move (x, y) [(x1,y1),(x2,y2)] tabuleiro = uPos (x2,y2) '*' (uPos (x1,y1) ' ' (uPos (x,y) ' ' tabuleiro))


-- genTabPositions, recebe um tabuleiro e gera uma matriz com todas as posições
-- desse tabuleiro. O tabuleiro de entrada só é usado para se saber quantas linhas
-- e colunas devem ser geradas. Essa função pode ser quebrada em outras funções, por exemplo,
-- para gerar as linhas

-- *RestaUm> genTabPositions gBoard 
-- [[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6)],
-- [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6)],
-- [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6)],
-- [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6)],
-- [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6)],
-- [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6)],
-- [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]]
--
--

-- genTabPositions :: GBoard -> [[(Int,Int)]]
genTabPositions :: GBoard -> [[(Int, Int)]]
genTabPositions [] = []
genTabPositions (x:xs) = linha (tam (x:xs), tam x) 0
  where
    tam :: [t] -> Int
    tam [] = 0
    tam (x:xs) = 1 + tam xs
    linha :: (Int,Int) -> Int -> [[(Int,Int)]]
    linha (x,y) n
      | (x == n) = []
      | otherwise = coluna (n, x) 0 : linha (x,y) (n+1)
    coluna :: (Int,Int) -> Int -> [(Int,Int)]
    coluna (x,y) n
      | (y == n) = []
      | otherwise = (x, n) : coluna (x,y) (n+1)


-- canMove
-- Essa função é usada para vericar se é final de jogo, ou seja se alguma peça ainda pode ser movida
-- Pode ser implementada da seguinte forma: gera o tabuleiro de posições, e para cada uma
-- das posições, tenta aplicar a função validMoves. Se validMoves gerar uma lista vazia
-- para todas as posições do tabuleiro, então significa que é final de jogo, ou seja, 
-- canMove retorna False. Se para pelo menos uma das posições a função validMoves
-- gerar uma sequência de jogo, então canMove retorna true

-- *RestaUm> canMove gBoard 
-- True
-- *RestaUm> canMove tBoard 
-- False


-- canMove :: GBoard -> Bool
canMove :: GBoard -> Bool
canMove tabuleiro = linha (genTabPositions tabuleiro)
  where
    linha :: [[(Int,Int)]] -> Bool
    linha [] = False
    linha (x:xs)
      | coluna x == False = linha xs
      | otherwise = True
    coluna :: [(Int,Int)] -> Bool
    coluna [] = False
    coluna (x:xs)
      | validMoves tabuleiro x == [] = coluna xs
      | otherwise = True
-- restaUm, recebe o tabuleiro de jogo e verifica se existe apenas uma peça, ou não
-- 
-- *RestaUm> restaUm gBoard 
-- False

-- restaUm :: GBoard -> Bool
restaUm :: GBoard -> Bool
restaUm tabuleiro
  | linha (genTabPositions tabuleiro) 0 == 1 = True
  | otherwise = False

    where 
      linha :: [[(Int,Int)]] -> Int -> Int
      linha [] tam = tam
      linha (x:xs) tam = linha xs (tam + coluna x 0)
      coluna :: [(Int,Int)] -> Int -> Int
      coluna [] tam = tam
      coluna (x:xs) tam
        | (isEmpty x tabuleiro) == False = coluna xs (tam+1)
        | otherwise = coluna xs tam

-- Recebe o tabuleiro do jogo e devolve uma string que é a representação visual desse tabuleiro
-- Usar como referencia de implementacao o video sobre tabela de vendas (Aula 06)


-- printBoard :: GBoard -> String
printBoard :: GBoard -> String
printBoard tabuleiro = cabecalho ++ geraPosicoes (genTabPositions tabuleiro) 0
   where
     geraPosicoes :: [[(Int,Int)]] -> Int -> String
     geraPosicoes [] n = " "
     geraPosicoes (x:xs) n = show n ++ geraLinha x ++ geraPosicoes xs (n + 1)
     geraLinha :: [(Int,Int)] -> String
     geraLinha [] = "\n"
     geraLinha (x:xs)
       | (isEmpty x tabuleiro) == False = " *" ++ geraLinha xs
       | otherwise = "  " ++ geraLinha xs

cabecalho :: String 
cabecalho = "\n  0 1 2 3 4 5 6\n"

-------------------------------------------
--
-- MOTOR DO JOGO. 
-- SOMENTE DESCOMENTAR ESSE CÓDIGO, QUANDO TODAS AS OUTRAS FUNÇÕES
-- ESTIVEREM IMPLEMENTADAS
--
-------------------------------------------
main :: IO ()
main = do
   gameLoop gBoard

gameLoop :: GBoard -> IO ()
gameLoop gb = do
   if (not (canMove gb))
   then do
        if (restaUm gb) then do
          putStr (printBoard gb)
          print "Voce Venceu!!!"
          else do
            putStr (printBoard gb)
            print "Nao existem pecas para mover! Voce perdeu!!"
   else do
      putStr (printBoard gb)
      print "Qual peca voce quer mover?"
      putStr "Digite uma linha: "
      l <- getLine
      putStr "Digite uma coluna: "
      c <- getLine
      let linha = read l
      let coluna = read c
      if isValidPos (linha,coluna)
      then case validMoves  gb (linha, coluna) of
           []  -> do
             print "NAO Eh POSSIVEL MOVER ESTA PECA"
             gameLoop gb
           [l] ->do

             gameLoop (move (linha,coluna) l gb)
           l   -> opcoes linha coluna l gb
      else do
           print "!!!!!POSICAO INVALIDA!!!!!"
           gameLoop gb

opcoes linha coluna l gb = do
            print "Essa peca pode se mover para:"
            resp <- printOpcoes 1 l
            if (resp>=1 && resp <= length l)
              then gameLoop (move (linha,coluna) (gArr (resp-1) l) gb)
              else do
                print "Opcao Invalida!!"
                opcoes linha coluna l gb

printOpcoes n [] = do
                 putStr "Digite a opção: "
                 op <- getLine
                 let opcao = read op
                 return opcao

printOpcoes n (x:xs) = do
                     print (show n ++ ". "++ (show ((head . tail) x)))
                     printOpcoes (n+1) xs

