Laboratorio 5
================
Óscar Agüero Rodriguez
7/11/2020

  - [Laboratorio 5 Autocorrelación
    Espacial](#laboratorio-5-autocorrelación-espacial)
      - [Introducción](#introducción)
      - [Respuesta](#respuesta)
      - [1.](#section)
      - [2.](#section-1)
      - [3.](#section-2)
      - [4.](#section-3)

# Laboratorio 5 Autocorrelación Espacial

## Introducción

La geoestadística es la rama de la estadística que se encarga del
análisis de fenómenos espaciales que exhiben un comportamiento
estructural. Parte de los mecanismos que están asociados a la
geoestadística se encuentra la correlación espacial, la cual es un
proceso que mide la autocorrelación espacial en función de las
ubicaciones de entidades y los valores de atributo mediante la
estadística de I de moran global.

Su uso propiamente, evidencia una correlación positiva cuando el I de
Moran está cercano a +1, lo cual significa que los valores se agrupan
dado similaridades identificadas entro los puntos auto correlacionados.
En caso de tener un I de Moran de -1, se dice que se tiene
autocorrelación negativa, dado que no hay similitud entre valores
cercanos. En resumen, la autocorrelación espacial indica si hay un
agrupamiento o dispersión en un mapa, mientras que el I de Moran
positivo indica que los datos están agrupados, en caso de ser negativo
implica datos dispersos.

El presente trabajo trata de una explicación detallada de cómo se
construye la autocorrelación espacial, para la cual se debe realizar
primero una construcción de la autocorrelación temporal y luego entender
la autocorrelación espacial.

## Respuesta

## 1\.

Explique las primeras 5 líneas devueltas por el comando str(w)

El significado de las primeras 5 líneas corresponde a los polígonos
adyacentes con respecto a un polígono especifico, del 1:3 se tienen 3
polígonos adyacentes o 3 vecinos, de 1:4 tiene 4 polígonos adyacentes,
de 1:2 tiene 2 polígonos adyacentes, de 1:2 se tienen así mismo y de 1:3
se tienen 3 polígonos adyacentes.

## 2\.

¿Cómo interpreta los resultados de las pruebas de significancia?

``` r
library(sp)
library(raster)
library(spData)
library(sf)
library(spdep)

p <- shapefile(system.file("external/lux.shp", package="raster"))
p <- p[p$NAME_1=="Diekirch", ]
p$value <- c(10, 6, 4, 11, 6)
data.frame(p)
```

    ##   ID_1   NAME_1 ID_2   NAME_2 AREA value
    ## 0    1 Diekirch    1 Clervaux  312    10
    ## 1    1 Diekirch    2 Diekirch  218     6
    ## 2    1 Diekirch    3  Redange  259     4
    ## 3    1 Diekirch    4  Vianden   76    11
    ## 4    1 Diekirch    5    Wiltz  263     6

``` r
xy <- coordinates(p)
w <- poly2nb(p, row.names=p$Id)
ww <-  nb2listw(w, style='B')
```

Moran Test

H0: No hay autocorrelación espacial H1: Hay autocorrelación espacial.

``` r
moran.test(p$value, ww, randomisation = F)
```

    ## 
    ##  Moran I test under normality
    ## 
    ## data:  p$value  
    ## weights: ww    
    ## 
    ## Moran I statistic standard deviate = 2.3372, p-value = 0.009714
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##         0.1728896        -0.2500000         0.0327381

Con un alpha = 5% y un p-value = 0.009 se tiene suficiente evidencia
estadística para rechazar H0, con lo que se está en presencia de
autocorrelación espacial.

Moran test (con simulación de Monte Carlo)

H0: No hay autocorrelación espacial H1: Hay autocorrelación espacial.

``` r
moran.mc(p$value, ww, nsim = 99)
```

    ## 
    ##  Monte-Carlo simulation of Moran I
    ## 
    ## data:  p$value 
    ## weights: ww  
    ## number of simulations + 1: 100 
    ## 
    ## statistic = 0.17289, observed rank = 97, p-value = 0.03
    ## alternative hypothesis: greater

con un alpha = 5% y un p-value = 0,02 se tiene suficiente evidencia
estadística para rechazar H0, con lo que se está en presencia de
autocorrelación espacial.

Probando código adicional

``` r
#moran.mc(p$value, ww, nsim = 999)
```

Genera un error debido a que el número de permutaciones es muy alto.

## 3\.

¿Cuál es el valor máximo que podemos usar en nsim?

Realizando un proceso de busqueda, sabiendo que con un nsim = 99 la
prueba funciona y que da problemas con un nsim = 999, se procede a
realizar una busqueda del máximo valor de nsim antes de generar error
por tamaño del valor en nsim.

Se determina que nsim = 120 es el máximo valor en la prueba antes de
generar problemas por tamaño.

``` r
moran.mc(p$value, ww, nsim = 120)
```

    ## 
    ##  Monte-Carlo simulation of Moran I
    ## 
    ## data:  p$value 
    ## weights: ww  
    ## number of simulations + 1: 121 
    ## 
    ## statistic = 0.17289, observed rank = 118.5, p-value = 0.02066
    ## alternative hypothesis: greater

## 4\.

Muestre como usar la función de “Greary” para calcular la C de Greary?

``` r
n <- length(p) 
x <- p$value
xbar <- mean(x)

#Formula de Greary
# help(geary)
# Parte del denominador (segunda parte de la formula)
dx <- (x-xbar)
# Parte del numerador (segunda parte de la formula)
xi <- rep(x, each = n)
xj <- rep(x)
xixj <- xi-xj
# Matriz de (x_i - x_j) que multiplica a la matrix w_ij 
pm <- matrix(xixj, ncol=n)
# Matriz w_ij 
wm <- nb2mat(w, style = 'B')
# Multiplicación de w_ij*(x_i-x_j)^2 (numerador de la segunda parte de la formula)
pmw <- (wm) * (pm)^2
# Sumatoria de la multiplicación (Numerador de la segunda parte de la formula)
spmw <- sum(pmw)
# Sumatoria de (x_i-mean(x_i))^2 (Denominador de la segunda parte de la formula)
smw <- (sum(dx))^2
# Segunda parte de la formula (sum(w_ij*(x_i-x_j)^2))/sum(x_i-mean(x_i))^2
sw <- spmw/smw
# Primera parte de la formula (n-1)/(2*sum(w_ij))
vr <- (n-1)/(2*sum(wm))
# Multiplicando las formulas (n-1)/(2*sum(w_ij)) * (sum(w_ij*(x_i-x_j)^2))/sum(x_i-mean(x_i))^2
GI <- vr*sw
# C de Greary
GI
```

    ## [1] 5.976067e+30
