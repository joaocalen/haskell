numCedulas valor = div valor 500 + div (mod valor 500) 100 + div (mod valor 100) 50 + div (mod valor 50) 10 + div (mod valor 10) 5 + mod valor 5  

areaRetangulo x y = x * y

areaCirculo r = pi * (r^2)

distancia x1 y1 x2 y2 = sqrt(((x2 - x1)^2) + ((y2 - y1)^2))

raizP a b c = (-b + sqrt(b^2 -4*a*c))/2*a

raizN a b c = (-b - sqrt(b^2 -4*a*c))/2*a

raizes a b c = (raizP a b c, raizN a b c)

potenciaN a b = 1 / (a ^ (-b))

potencia a b = 
		 if (b > 0) then
		   a ^ b
		 else
		 		 if((b==0) && (not(a==0))) then
		   		1
		 			else
					 	 if (b < 0) then
		   				1/(a ^ (-b))
		 				 else
		   				"Potencia nao pode ser realizada" 


