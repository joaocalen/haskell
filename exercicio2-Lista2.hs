verificarIntervaloAberto x a b d1 d2 = 
    if (a > b) then
        verificarIntervaloAberto x b a d1 d2
    else
        if (((x > a) && (x < b)) && (rem x d1 == 0) && (rem x d2 == 0) ) then
            "nois"
        else
            "nao eh nois"

verificarIntervaloAberto :: Integer -> Integer -> Integer -> Integer -> Integer -> String
    

oux a b = ((a || b) && not(a && b))

oux :: Bool -> Bool -> Bool

-- Imaginando todos os meses com 30 dias e o ano com 360 dias

tempoProjeto di mi ai df mf af = ((af - ai) * 360) + ((mf - mi) * 30) + (df - di) 

isSquare l1 l2 l3 = (l1 + l2 > l3) && (l1 + l3 > l2) && (l2 + l3 > l1) 

pertinenciaSquare x y ex ey dx dy = (((x >= ex) && (x <= dx)) || ((x >= dx) && (x <= ex))) && (((y >= ey) && (y <= dy)) || ((y >= dy) && (y <= ey)))

distancia2pontos ax ay bx by = sqrt((ax - bx)^2 + (ay - by)^2)

pertinenciaCircunferencia x y cx cy r =  distancia2pontos x y cx cy <= r

lucroCerto e f = 
    if (pontos > 20) then
        if (pontos > 30) then
            if (pontos > 40) then
                500
            else
                400
        else
            300
    else
        if (pontos > 10) then
            200
        else
            if (pontos > 1) then
                100
            else
                0
    where
    pontos = e - (2/3)*f

movimentarXadrez n c1 c2 =
    if (pertTab) then 
        show ("A peca " ++ retornaString(moveUp) ++ " cima, " ++ retornaString(moveDown) ++ " baixo, " ++
         retornaString(moveRight) ++ " direita, " ++ retornaString(moveLeft) ++ " esquerda, " ++ retornaString(moveDiagUR) ++ " diagonal superior direta, "
         ++ retornaString(moveDiagUL) ++ " diagonal superior esquerda, " ++ retornaString(moveDiagDL) ++ " diagonal inferior esquerda e "
         ++ retornaString(moveDiagDR) ++ " diagonal inferior direita")
    else
        "Escreve as coordenadas direito seu animal"
    where
    pertTab = (signum c1 == signum c2) && not(signum c1 == -1) && (c1 <= n) && (c2 <= n)
    moveUp = (c2 + 1 <= n)
    moveDown = (c2 - 1 >= 0)
    moveRight = (c1 + 1 <= n) 
    moveLeft = (c1 - 1 >= 0)
    moveDiagUR = moveUp && moveRight
    moveDiagUL = moveUp && moveLeft
    moveDiagDL = moveDown && moveLeft
    moveDiagDR = moveDown && moveRight
    retornaString a =
        if (a) then
            "pode se movimentar para"
        else
            "nao pode se movimentar para"

precoFinal idade preco | idade >= 60 = preco * 0.6
                       | idade >= 2 && idade <= 10 = preco * 0.5
                       | otherwise  = preco * 0.1
