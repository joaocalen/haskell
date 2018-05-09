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


