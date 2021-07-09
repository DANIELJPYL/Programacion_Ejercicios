#1. Calcula los valores n�mericos aproximados de:#
#a.
m<-(0.3*0.15)/(0.3*0.15+0.2*0.8+0.5*0.12)
round(m, 2)
#b
p<-(5**6)/factorial(6)*exp(-5)
round(p, 2)
#c
comb <- factorial(20) / (factorial(7) * (factorial(20-7)))
pot <- (0.4^7)*(0.6^13)
c <- comb * pot
round(c, 2)

#2. Realizar la siguiente suma
#a. 1+2+3+4+...+1000
Sx<-function(a1, r, n){
  a1*(r**n-1)/(r-1)
}
Sx(1, 2, 11)

#3. El vector "grupo" representa el grupo al que pertenece una serie de alumnos
#a. Cuantos elementos tiene?
length(grupo)

#b. En que posiciones del vector esta la letra "A"?
which(grupo == "A")

#4. El vector "nota" representa la nota de un examen de los alumnos que est�n en los grupos del vector "grupo"
nota
#a. Cuanto suman todas las notas?
sum(nota)

#b. Cual es la media aritmetica de todas las notas?
mean(nota)

#c. En que posiciones estan las notas mayores de 7.0?
which(nota > 7.0)

#d. Visualiza las notas ordenadas de mayor a menor
sort(decreasing = T, nota)

#e. En que posicion esta la nota maxima?
which.max(nota)

#5. A partir de los vectores "grupo" y nota "definidos".
#a. Suma las notas de los 10 primeros alumnos del vector
sum(nota[1:10])

#b. Cuantos alumnos hay del grupo C?
sum(grupo == "C")

#c. Cuantos alumnos han aprobado?
sum(nota>=5.5)

#d. Cuantos alumnos del grupo B han aprobado?
length(grupo[(grupo == "B" & nota >=5.5)])
#e. Que porcentaje de alumnos del grupo C han aprobado?
x<-
#f. De que grupos son la maxima y minima notas de toda la muestra?

#g. Nota media de los alumnos de grupo A y B, juntos, considerando solo a los que han aprobado.
