#PARTE 1
#1. Calcula los valores numericos aproximados de
#a.
m<-(0.3*0.15)/(0.3*0.15+0.2*0.8+0.5*0.12)
round(m, 2)

#b
p<-(5**6)/factorial(6)*exp(-5)
round(p, 2)

#c
comb <- factorial(20) / (factorial(7) * (factorial(20-7)))
pot <- (0.4**7)*(0.6**13)
c <- comb * pot
round(c, 2)

#2. Realizar la siguiente suma
#a. 1+2+3+4+...+1000
Sn<-function(n){
  n*(n+1)/2
}
Sn(1000)

#b. 1+2+4+8+16+...1024
Sx<-function(a1, r, n){
  a1*(r**n-1)/(r-1)
}
Sx(1, 2, 11)

#3. El vector "grupo" representa el grupo al que pertenece una serie de alumnos
#a. Cuantos elementos tiene?
length(grupo)

#b. En que posiciones del vector esta la letra "A"?
which(grupo == "A")

#4. El vector "nota" representa la nota de un examen de los alumnos que 
#estan en los grupos del vector "grupo"
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
grupo[grupo == "C"] %>% length()

#c. Cuantos alumnos han aprobado?
nota[nota>=5.5] %>% length()

#d. Cuantos alumnos del grupo B han aprobado?
grupo[(grupo == "B" & nota >=5.5)] %>% length()

#e. Que porcentaje de alumnos del grupo C han aprobado?
x<- grupo[grupo == "C" & nota >=5.5] %>% length()
y<- grupo[grupo == "C"] %>% length
(x/y*100)

#f. De que grupos son la maxima y minima notas de toda la muestra?
names(nota) <- grupo
which(nota == min(nota))
which(nota == max(nota))

#g. Nota media de los alumnos de grupo A y B, juntos, considerando solo a los 
#   que han aprobado.
mean(nota[grupo %in% c("A", "B") & nota>=5.5])

#6. Calcula el percentil 66 de las notas de todos los alumnos, 
#   y tambien de los alumnos del grupo C.
quantile(nota, 0.66)
quantile(nota[grupo == "C"], 0.66) 

#7. Un alumno tiene una nota de 4.9. ¿Qué porcentaje, del total de alumnos, tiene una nota
#   menor o igual que la suya? Y que porcentaje tiene una nota mayor o igual que la suya?

length(nota[nota <= 4.9])/length(nota)*100
length(nota[nota >= 4.9])/length(nota)*100

#8. Realiza el grafico de diagramas de caja de las notas de cada grupo, 
#   para poder comparar el nivel de cada uno de ellos
boxplot(nota[grupo == "A"])
boxplot(nota[grupo == "B"])
boxplot(nota[grupo == "C"])
boxplot(nota[grupo == "D"])
boxplot(nota[grupo == "E"])

#9. Si la variable "conc" recoge la concentracion de plomo (en ppm) en el aire 
#   de cierta zona durante un dia completo
#a. Cual ha sido la concentracion maxima?
max(conc)

#b. En cuantos de los muestreos se ha superado la concentracion de 40.0 ppm?
conc[conc>40] %>% length()

#c. Cual ha sido la concentracion media del dia?
mean(conc)

#d. Cuales fueron las 10 mediciones mas bajas del dia?
sort(conc)[1:10]

#e. Si la primera medida fue a las 00:00. A que hora del dia se alcanzo la concentracion maxima?
hora <- which.max(conc)/length(conc)*24
min <- (hora-floor(hora))*60
paste(floor(hora), ":", min)

#PARTE 2 
#1. Graficar los puntos
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
b <- c(1, 4, 6, 8, 25, 36, 49, 61, 81, 100)

plot(a, b)

#2.
A<-cbind(seq(1, 4), seq(2, 8, by=2), seq(3, 12, by = 3))
A

#3.
diag(1, 3, 3)

#4. 
MatrizN <- function(k) {
  nula <- diag(k);
  for (i in 1:k) {
    nula[i, i] = 0
  };
  return(nula)
}
MatrizN(4)

#5.
B<-diag(c(0, 2, 3, 4))
B

#6.
T_A <- t(A)
T_A

#7.
#A+B
#A-B
#3*B
3*B
#A*B


#8.
P<-matrix(c(1, -2, 1, 2, 4, 0, 3, -2, 1), nrow = 3, ncol =3)
P
P**6
Potencia <- function(n) {
  P**n
}
Potencia(6)

#9. 
matriz01 <- matrix(c(3,9,3,-1,-2,1,1,1,-2), nrow = 3, ncol =3 )
matriz01
matriz02<- matrix(c(-1, -9, -9), nrow = 3, ncol = 1)
matriz02
Solve(matriz01, matriz02)
 
#10.
#det() = nos calcula el determinante de una matriz cuadrada
#eigen()


#11.
MzB <- cbind(seq(1,10), seq(2,20,2), seq(3,30,3), seq(4,40,4), seq(5,50,5))
MzB
MzA <- matrix(c(0,1,0,0,1,1,0,1,1,0,0,1,0,0,1,1,0,1,0,1,0,1,0,1,0), nrow=5, ncol=5)
MzA
T_MzB <- t(MzB) 
T_MzB


#12.

#13.
data(co2)
means = aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2 = as.vector(means)

co2
lag(co2)
dif <- co2 - lag(co2)
dif

dif_2020_2019 <- 2.64
year_2020 <- 2020

plot(x = year, y = dif, 
     type = "b", 
     xlab = "a?o", 
     ylab = "CO2 aumento por a?o",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7))
points(x = year_2020, y = dif_2020_2019, pch = 4, col = "red")


#14.

