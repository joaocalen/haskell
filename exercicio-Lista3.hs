type Ponto = (Int, Int)



determinarTriangulo (ax,ay) (bx,by) (cx,cy) = 
    if (isTriangulo) then
        verificarTriangulo
    else
        "Nao eh triangulo"
    where
    isTriangulo = not((ax,ay) == (bx,by) || (bx,by) == (cx,cy) || (ax,ay) == (cx,cy)) && not (determinante == 0)
    verificarTriangulo = 
        if (distancia (ax,ay) (cx,cy) == distancia (bx,by) (cx,cy)) then
            if(distancia (bx,by) (cx,cy) == distancia (bx,by) (ax,ay)) then
                "Equilatero"
            else
                "Isoceles"
        else
            if(distancia (ax,ay) (bx,by) == distancia (bx,by) (cx,cy)) then
                "Isoceles"
            else
                "Escaleno"
    
    
    determinante = (ax * by) + (ay * cx) + (bx * cy) - ((by * cx) + (ax * cy) + (ay * bx))

distancia (dx, dy) (ex, ey) = round(sqrt(((dx - ex)^2 + (dy - ey)^2)))
