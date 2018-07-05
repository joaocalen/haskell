
-- retorna False
foo0 = compare 2 (mod 17 2) == EQ


{--
A forma expressada no pdf retornará erro, pois a função atribui valor ao y só depois do y ser acessado, ou seja, quando
o mesmo for acessado ainda não haverá valor ao y, resultando em erro.
 --}
foo1 = x + y
    where y = 2
          x = y


{--
    Dois where e fora de ordem
 --}

foo2 = x + y
    where y = 2
          x = y


foo3 x =
    if( x <= 30 ) then 'D'
        else if( x <= 50 ) then 'C'
        else if( x <= 80 ) then 'B'
    else 'A'

{--

2-

I)

a) [5,10..79]



--}


{-- verificaOrdem xs = verifica xs
                    where
                        i <- [0.. length xs -2]
                        verifica xs = xs!!i <= xs!!(i + 1)

--}
--verificaOrdem2 xs = [xs!!i | i <- [0.. length xs -2], xs!!i > xs!!(i+1)] == []

-- letra b
maioreMenor xs = (menor xs, maior xs)
                where
                  maiores x xs = [y | y <- xs, y > x]
                  menores x xs = [y | y <- xs, y < x]
                  maior xs = head [x | x <- xs, maiores x xs == []]
                  menor xs = head [x | x <- xs, menores x xs == []]

-- letra c
multiplos n lim = [1*n,2*n..lim]

-- letra d

divideLista xs = ([xs!!i | i <- [0.. (div (length xs) 2)-1]], [xs!!j | j <- [div (length xs) 2 .. length xs -1]])

-- letra e

duplicaLista xs = duplicaLista2 xs [] (length (xs) -1) 1

duplicaLista2 xs ys indice x | indice < 0 = ys
                             | x == 1 = duplicaLista2 xs ((xs!!indice):ys) (indice) 2
                             | otherwise = duplicaLista2 xs ((xs!!indice):ys) (indice-1) 1

-- letra g

intersecao xs ys = [x | x <- xs, y <- ys, x==y]

-- letra f
uniao xs ys = intersecao xs ys ++ diferenca ys xs ++ diferenca xs ys
              where
                diferenca ws zs = [w | w <- ws, not(elem w (intersecao ws zs))]

-- letra i
alternaLista xs | length (pares xs) == length (impares xs) = [x | i <- [0 .. length (pares xs) -1], x <- [pares xs!!i, impares xs!!i]]
                | otherwise = []
                    where
                      pares xs = [x | x <-xs, even x]
                      impares xs = [x | x <-xs, odd x]

-- letra j
take' x xs = [xs!!i | i <- [0..x-1]]

drop' x xs = [xs!!i | i <- [x..length xs -1]]

-- letra m


verificaPalavra xs = [x | x <- xs, not(verificaCaracter x)] == []
                where
                  verificaCaracter x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
