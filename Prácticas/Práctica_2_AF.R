
# En esta practica vamos a identificar las variables que corresponden a cada
# uno de los cinco aspectos de la personalidad de un individuo.
# Para ello cargamos el conjunto de datos bfi de la librer?a psyh.
# Este conjunto de datos contiene 2800 observaciones con 28 variables, con 25
# Items de una prueba de personalidad (MBI: Maslach Burnout Inventory.
# Las cinco caracter?sticas que definen la personalidad de un individuo son:
# A - Amabilidad; C - Conciencia; E - Extraversi?n; N - Neuroticismo y Ap - Apertura
# Instalamos las librer?as a utilizar si no est?n ya instaladas
#install.packages("psych")
#install.packages("polycor")
#install.packages("ggcorrplot")
#install.packages("corrr")
# Cargamos las librer?as que vamos a utilizar
library(psych)
library(polycor)
library(ggcorrplot)
library(corrplot)
library(corrr)
# Cargamos los datos mencionados anteriormente, en este caso
# nos quedamos con 200 observaciones de los 28 items
bfi_s <- bfi[1:200,1:25] # subconjunto de datos


# PASO 1: ?tiene sentido un AF?
# --------------------------------------
# Para responder a esta pregunta se comprueba si existe correlaci?n entre las
# variables con la funci?n "cor" del paquete base, que proporciona la matriz
# de correlaciones R
# Hay muchos valores perdidos asi que arreglamos la base de datos primero
not_available<-function(data,na.rm=F){
  data[is.na(data)]<-mean(data,na.rm=T)
  data
}
bfi_s<-as.data.frame(apply(bfi_s,2,not_available))




# Ahora calculamos la matriz de correlaciones para ver si est?n correlaciondas
# Vamos a utilizar distintas representaciones de esta informaci?n
# En primer lugar la matriz de correlaciones cl?sica
cor(bfi_s)

# En segundo lugar podemos tener una representaci?n visual de las correlaciones
# Calculamos la matriz de correlaciones polic?rica
# Las funciones hetcor y ggcorrplot est?nn en los paquetes polycor y ggcoorplot
poly_cor<-hetcor(bfi_s)$correlations
ggcorrplot(poly_cor, type="lower",hc.order=T)

# Otra representaci?n visual interesante es la siguiente (de la librer?a "corrplot")
corrplot(cor(bfi_s), order = "hclust", tl.col='black', tl.cex=1)

# O esta otra opci?n tambi?n es muy visual (de la librer?a "corrr")
bfi_s_correlaciones <- correlate(bfi_s)  #C?lculo de un objeto de correlaciones
rplot(bfi_s_correlaciones, legend = TRUE, colours = c("firebrick1", "black","darkcyan"), print_cor = TRUE) 

# El an?lisis de outliers se deja al gusto del lector, aunque en este ejemplo no 
# los hay

# El contraste de esfericidad de Bartlett permite comprobar si las correlaciones
# son distintas de 0 de modo significativo. La hip?tesis nula es que det(R)=1
# Se normalizan los datos
datos_normalizados<-scale(bfi_s)
# Se hace el test de esfericidad
cortest.bartlett(cor(datos_normalizados))

# ----------- #
# Paso 3: AF  #
# ----------- #
# Hay que escoger un m?todo para extraer los factores, ACP, verosimilitud, etc.
# La funci?n fa() implementa hasta 6 m?todos distintos
# Vamos a comparar las salidas con el m?todo del factor principal y con el de 
# m?xima verosimilitud.

### prueba de dos modelos con tres factores
modelo1<-fa(poly_cor,
            nfactors = 3,
            rotate = "none",
            fm="mle") # modelo m?xima verosimilitud

modelo2<-fa(poly_cor,
            nfactors = 3,
            rotate = "none",
            fm="minres") # modelo m?nimo residuo
# comparando las comunalidades
sort(modelo1$communality,decreasing = T)->c1
sort(modelo2$communality,decreasing = T)->c2
head(cbind(c1,c2))

# comparacion de las unicidades, es decir la proporción de varianza
# que no ha sido explicada por el factor (1-comunalidad)
sort(modelo1$uniquenesses,decreasing = T)->u1
sort(modelo2$uniquenesses,decreasing = T)->u2
head(cbind(u1,u2))

# Determinemos ahora el n?mero ?ptimo de factores
# Hay diferentes criterios, entre los que destacan el Scree plot (Cattel 1966)
# y el an?lisis paralelo (Horn 1965). Ejercicio: buscar sus interpretaciones
scree(poly_cor)
fa.parallel(poly_cor,n.obs=200,fa="fa",fm="minres")
# Se deduce que el n?mero ?ptimo de Factores es 5

# Estimamos el modelo factorial con 5 factores implementando una rotaci?n
# tipo varimax para buscar una interpretaci?n m?s simple.
modelo_varimax<-fa(poly_cor,nfactors = 5,rotate = "varimax",
                   fa="mle")

# Mostramos la matriz de pesos factorial rotada
print(modelo_varimax$loadings,cut=0) 
# Visualmente podr?amos hacer el esfuerzo de ver con qu? variables correlacionan cada 
# uno de los factores, pero es muy tedioso de modo que utilizamos la siguiente
# representaci?n
fa.diagram(modelo_varimax)
# En este diagrama, entre otras cosas se ve que el primer factor esta asociado 
# con los items E1, E2, E3, E4, E5 y N4, que son los items del cuestioario que 
# tratan de identificar la cualidad de la extraversi?n



# ----------------------------------------------------
# Otra forma de hacerlo, con test de hip?tesis al final
# que contrasta si el numero de factores es suficiente 
library(stats)
factanal(bfi_s,factors=9, rotation="none")

