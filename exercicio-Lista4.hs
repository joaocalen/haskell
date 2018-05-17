
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