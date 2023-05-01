#Codigo para el reporte de procesos Gaussianos
#necesitamos el paquete MASS para simular de una normal multivariada
install.packages('MASS')
library(MASS) #para simular de la normal multivariada
library(latex2exp) #para notacion con latex
library(patchwork) #para sumar objetos de ggplot
s=seq(-5,10,length.out=100)
length(s)
?mvrnorm
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
#generar n realizaciones de una normal bivariada con media cero y 
#matriz de varianzas covarianzas Sigma

mvrnorm(n = 1000, rep(0, 2), Sigma)

#auxiliary function: a distance function
d=function(s,t){
  return(abs(s-t))
}
#ejemplin
d(2,5)
d(5,1)



sigma_2=1



#phi=0.01
phi=0.01
#definimos una funcion que calcule covarianza exponencial
covexp=function(phi,s,t){
  sigma_2*exp(-phi*d(s,t))
}
covexp(phi, -4,3)
#definimos una funcion que calcule covarianza Gaussiana
covGauss=function(phi,s,t){
  sigma_2*exp(-(phi^2)*d(s,t)^2)
}
covGauss(phi, -4,3)
trayectoriasExp=matrix(nrow=100,ncol=10) #matriz para guardar 100 trayectorias simuladas
#con funcion de covarianza exponencial
trayectoriasGauss=matrix(nrow=100,ncol=10) #matriz para guardar 100 trayectorias simuladas
#con funcion de covarianza Gaussiana
s[100]
s[1]
s[2]-s[1]
s[5]-s[4]
#generamos 10 trayectorias con cada funcion de covarianza


#funcion que calcule matriz de varianzas covarianzas bajo una funcion exponencial
Sigma_exp=function(phi){
  m=matrix(nrow=100, ncol=100)
  for(i in 1:100){
    for(j in 1:100){
      m[i,j]=covexp(phi,s[i], s[j])
    }
  }
  return(m)
}

#funcion que calcule matriz de varianzas covarianzas bajo una funcion Gaussiana
Sigma_Gauss=function(phi){
  m=matrix(nrow=100, ncol=100)
  for(i in 1:100){
    for(j in 1:100){
      m[i,j]=covGauss(phi,s[i], s[j])
    }
  }
  return(m)
}
Sigma_Gauss(.5)
#tomamos media cero para todas las localizaciones y en todas las iteraciones
set.seed(1) #FIJAMOS SEMILLA PARA TENER REPRODUCIBIIDAD
for(i in 1:10){
  #generamos una trayectoria bajo funcion covarianza Exponencial
  Y_exp=mvrnorm(n = 1, rep(0,100), Sigma_exp(phi))
  #generamos una trayectoria con funcionnd e covarianza Gaussiana
  Y_Gauss=mvrnorm(n = 1, rep(0, 100), Sigma_Gauss(phi))
  #las almacenamos en sus correspondientes matrices
  trayectoriasExp[,i]=Y_exp
  trayectoriasGauss[,i]=Y_Gauss
}
head(trayectoriasGauss)
plot(s, trayectoriasExp[,1], type='l', lwd=2)
lines(s,trayectoriasExp[,2], type='l', col='purple', lwd=2)
lines(s, trayectoriasExp[,3], type='l', col='tomato', lwd=3)

trayectoriasExp_aux=cbind(trayectoriasExp, s)
trayectoriasExp_aux
head(trayectoriasExp_aux)
trayectoriasExp_aux=as.data.frame(trayectoriasExp_aux)
head(trayectoriasExp_aux)
library(ggplot2)
#ggplot recibe como input un data frame. Por eso necesitabamos convertir
#la matriz a data frame
p1=ggplot(data=trayectoriasExp_aux, aes(x=s))+
  geom_line(aes(y=V1), color = "orange", lwd=2)+
  geom_line(aes(y=V2), color = "green4", lwd=2)+
  geom_line(aes(y=V3), color = "gold", lwd=2)+
  geom_line(aes(y=V3), color = "purple", lwd=2)+
  geom_line(aes(y=V4), color = "pink", lwd=2)+
  geom_line(aes(y=V5), color = "blue", lwd=2)+
  geom_line(aes(y=V6), color = "red", lwd=2)+
  geom_line(aes(y=V7), color = "magenta", lwd=2)+
  geom_line(aes(y=V8), color = "black", lwd=2)+
  geom_line(aes(y=V9), color = "tomato", lwd=2)+
  geom_line(aes(y=V10), color = "royalblue", lwd=2)+
  labs(x='s', y=TeX(r"(\textbf{$Y(s)$})"), title=TeX(r"(\textbf{Simulaciones bajo una función de covarianza exponencial con  $\phi=0.01$})"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-3,3)
  
p1
trayectoriasGauss_aux=cbind(trayectoriasGauss, s)
trayectoriasGauss_aux
head(trayectoriasGauss_aux)
trayectoriasGauss_aux=as.data.frame(trayectoriasGauss_aux)
#alpha sirve para tonalidad/transparencia. Mas cercano a.1 es menos efecto de alpha
#alpha=0 hace quie la trayectoria sea invisible.
p2=ggplot(data=trayectoriasGauss_aux, aes(x=s))+
  geom_line(aes(y=V1), color = "orange", lwd=2, alpha=.6)+
  geom_line(aes(y=V2), color = "green4", lwd=2, alpha=.6)+
  geom_line(aes(y=V3), color = "gold", lwd=2, alpha=.6)+
  geom_line(aes(y=V3), color = "purple", lwd=2, alpha=.6)+
  geom_line(aes(y=V4), color = "pink", lwd=2, alpha=.6)+
  geom_line(aes(y=V5), color = "blue", lwd=2, alpha=.6)+
  geom_line(aes(y=V6), color = "red", lwd=2, alpha=.6)+
  geom_line(aes(y=V7), color = "magenta", lwd=2, alpha=.6)+
  geom_line(aes(y=V8), color = "black", lwd=2, alpha=.6)+
  geom_line(aes(y=V9), color = "tomato", lwd=2, alpha=.6)+
  geom_line(aes(y=V10), color = "royalblue", lwd=2, alpha=.6)+
  labs(x='s', y=TeX(r"(\textbf{$Y(s)$})"), title=TeX(r"(\textbf{Simulaciones bajo una función de covarianza Gaussiana con  $\phi=0.01$})"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-3,3)
p2
p1+p2


#replicamos este procedimiento para otros valores de phi
phi=2
trayectoriasExp=matrix(nrow=100,ncol=10) #matriz para guardar 100 trayectorias simuladas
#con funcion de covarianza exponencial
trayectoriasGauss=matrix(nrow=100,ncol=10)
#generamos 10 trayectorias con cada funcion de covarianza
#funcion que calcule matriz de varianzas covarianzas bajo una funcion exponencial
set.seed(1) #FIJAMOS SEMILLA PARA TENER REPRODUCIBIIDAD
for(i in 1:10){
  #generamos una trayectoria bajo funcion covarianza Exponencial
  Y_exp=mvrnorm(n = 1, rep(0,100), Sigma_exp(phi))
  #generamos una trayectoria con funcionnd e covarianza Gaussiana
  Y_Gauss=mvrnorm(n = 1, rep(0, 100), Sigma_Gauss(phi))
  #las almacenamos en sus correspondientes matrices
  trayectoriasExp[,i]=Y_exp
  trayectoriasGauss[,i]=Y_Gauss
}
head(trayectoriasGauss)
plot(s, trayectoriasExp[,1], type='l', lwd=2)
lines(s,trayectoriasExp[,2], type='l', col='purple', lwd=2)
lines(s, trayectoriasExp[,3], type='l', col='tomato', lwd=3)

trayectoriasExp_aux=cbind(trayectoriasExp, s)
trayectoriasExp_aux=as.data.frame(trayectoriasExp_aux)
head(trayectoriasExp_aux)


p1=ggplot(data=trayectoriasExp_aux, aes(x=s))+
  geom_line(aes(y=V1), color = "orange", lwd=2)+
  geom_line(aes(y=V2), color = "green4", lwd=2)+
  geom_line(aes(y=V3), color = "gold", lwd=2)+
  geom_line(aes(y=V3), color = "purple", lwd=2)+
  geom_line(aes(y=V4), color = "pink", lwd=2)+
  geom_line(aes(y=V5), color = "blue", lwd=2)+
  geom_line(aes(y=V6), color = "red", lwd=2)+
  geom_line(aes(y=V7), color = "magenta", lwd=2)+
  geom_line(aes(y=V8), color = "black", lwd=2)+
  geom_line(aes(y=V9), color = "tomato", lwd=2)+
  geom_line(aes(y=V10), color = "royalblue", lwd=2)+
  labs(x='s', y=TeX(r"(\textbf{$Y(s)$})"), title=TeX(r"(\textbf{Simulaciones bajo una función de covarianza exponencial con  $\phi=2$})"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-4,4)
p1
trayectoriasGauss_aux=cbind(trayectoriasGauss, s)
trayectoriasGauss_aux
head(trayectoriasGauss_aux)
trayectoriasGauss_aux=as.data.frame(trayectoriasGauss_aux)
p2=ggplot(data=trayectoriasGauss_aux, aes(x=s))+
  geom_line(aes(y=V1), color = "orange", lwd=2)+
  geom_line(aes(y=V2), color = "green4", lwd=2)+
  geom_line(aes(y=V3), color = "gold", lwd=2)+
  geom_line(aes(y=V3), color = "purple", lwd=2)+
  geom_line(aes(y=V4), color = "pink", lwd=2)+
  geom_line(aes(y=V5), color = "blue", lwd=2)+
  geom_line(aes(y=V6), color = "red", lwd=2)+
  geom_line(aes(y=V7), color = "magenta", lwd=2)+
  geom_line(aes(y=V8), color = "black", lwd=2)+
  geom_line(aes(y=V9), color = "tomato", lwd=2)+
  geom_line(aes(y=V10), color = "royalblue", lwd=2)+
  labs(x='s', y=TeX(r"(\textbf{$Y(s)$})"), title=TeX(r"(\textbf{Simulaciones bajo una función de covarianza Gaussiana con  $\phi=2$})"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-4,4)
p2
p1+p2
