main :: IO ()
main = return ()


numCedulas valor = div valor 500 + div (mod valor 500) 100 + div (mod valor 100) 50 + div (mod valor 50) 10 + div (mod valor 10) 5 + mod valor 5  

areaRetangulo x y = x * y

areaCirculo r = pi * (r^2)

distancia x1 y1 x2 y2 = sqrt(((x2 - x1)^2) + ((y2 - y1)^2))

raizP a b c = (-b + sqrt(b^2 -4*a*c))/2*a

raizN a b c = (-b - sqrt(b^2 -4*a*c))/2*a

raizes a b c = (raizP a b c, raizN a b c)


potencia a b = 
		if (b > 0) then
		   potenciaP a b
		else
		 		if((b==0) && (not(a==0))) then
		   			1
		 		else
				 	if (b < 0) then
		   				potenciaN a b
		 			else					 	
		   				 imprimirErro 
	where
	potenciaN a b = 1 / (a ^ (-b))
	potenciaP a b = a ^ b
	imprimirErro =  0

potencia :: Float -> Integer -> Float


celsiusToFarenheit c = (c*1.8) + 32

lucroAnual v = (v * 1.005^12)

areaCinza r = (areaCirculo (3*r/2)) - (areaCirculo r)

verificarQuadrante x y = 
	if ((x >= 0) && (y >= 0)) then
		"Primeiro Quadrante"
	else
		if ((x < 0) && (y >= 0)) then
			"Segundo quadrante"
		else
			if ((x < 0) && (y < 0)) then
				"Terceiro Quadrante"
			else
				"Quarto quadrante"

verificarIntervalo x a b = 
	if(a > b) then
		verificarIntervalo x b a 
	else
		if((x >= a) && (x <= b)) then
			"Pertence ao intervalo"
		else
			"Nao pertence ao intervalo"
		
