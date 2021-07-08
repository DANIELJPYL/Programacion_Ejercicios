#1. Calcula los valores númericos aproximados de:#
(0.3*0.15)/(0.3*0.15+0.2*0.8+0.5*0.12)
(5**6)/factorial(6)*exp(-5)

#2. Realizar la siguiente suma
#a. 1+2+3+4+...+1000
n<-1000
a<- seq(1, n)
sum(a)

#b. 1+2+4+8+16+...+1024
Sx<-function(a1, r, n){
  a1*(r**n-1)/(r-1)
}
Sx(1, 2, 11)
