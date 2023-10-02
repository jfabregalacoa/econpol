# Clase práctica de teoría espacial del voto
rm(list=ls())
require(here)
aqui <- here()

library(dplyr)
library(ggplot2)

#------------------------------------------------------------------------------
# ejemplo del mecanismo
#------------------------------------------------------------------------------

n <- 10 # votantes

# Generamos datos ficticios de un cuerpo colegiado de n personas que 
# han votado 50 veces
set.seed(123) 
puntos_ideals <- seq(-10, 10, length.out = n)
vote_positions <- sample(-10:10, 50, replace = TRUE)
votacion <- expand.grid(Person_ID = 1:10, Vote_ID = 1:50)
votacion$Vote_Result <- ifelse(abs(puntos_ideals[votacion$Person_ID] - 
                                     vote_positions[votacion$Vote_ID]) <= 2, 
                               1, 
                               0)

# Creamos un dataframe para almacenar los resultados de las iteraciones
ideals_df <- data.frame(Iteration = integer(), 
                        Person_ID = integer(), 
                        Ideal = numeric())

# Función para calcular la prob. de un voto dado un punto ideal y una 
# posición de votación
calc_prob <- function(ideal, position) {
  1 / (1 + exp(-abs(ideal - position)))
}

# Aquí vamos a hacer f iteraciones
f <- 3 # rango de variacion inicial para simular iteraciones para ir viendo el 
#proceso de convergencia

# Iteración 1
# 1. Inicialización: Estimación inicial de los puntos ideales y parámetros 
ideals_est <- puntos_ideals
positions_est <- vote_positions

# 2. Cálculo de Probabilidades
votacion$Prob <- mapply(calc_prob, 
                        ideals_est[votacion$Person_ID], 
                        positions_est[votacion$Vote_ID])

# Almacenar los resultados de la iteración 1
ideals_df <- rbind(ideals_df, 
                   data.frame(Iteration = 1, 
                              Person_ID = 1:n, 
                              Ideal = ideals_est))

# 3. Aquí debería ir la Maximización de la Verosimilitud de NOMINATE, pero 
# hagamos una versión simplificada en la que incrementamos (sin justificación 
# alguna) en 0.1 los puntos ideales y la posición estimada.
ideals_est <- ideals_est + runif(1, min=-f, max=f)
positions_est <- positions_est + + runif(1, min=-0.1, max=0.1)

it <- 120 # número de iteraciones

for(j in 1:it){
  # 1. Cálculo de Probabilidades considerando que ideals_est ha variado
  votacion$Prob <- mapply(calc_prob, 
                          ideals_est[votacion$Person_ID], 
                          positions_est[votacion$Vote_ID])
  ideals_df <- rbind(ideals_df, 
                     data.frame(Iteration = j, 
                                Person_ID = 1:n, 
                                Ideal = ideals_est))
  
  # 2. Simulación de la Maximización de Verosimilitud
  ideals_est <- ideals_est + runif(1, min=-f, max=f)/j
  positions_est <- positions_est + runif(1, min=-0.1, max=0.1)/j
}

# Gráfico resumen
ggplot(ideals_df, aes(x = Iteration, y = Ideal, group = Person_ID, 
                      color = as.factor(Person_ID))) +
  geom_line() +
  geom_point() +
  labs(title = "Cambios en los Puntos Ideales Estimados",
       x = "Iteración",
       y = "Punto Ideal Estimado",
       color = "ID de la Persona") +
  theme_minimal()

#------------------------------------------------------------------------------
# wnominate
#------------------------------------------------------------------------------
# Cargar librerías necesarias para la manipulación de datos y análisis
library(foreign)
library(gdata)

# Cargar librerías para análisis paramétrico y no paramétrico de datos 
# en formatos rollcall
library(wnominate)

# Leer datos de un archivo CSV que contiene votaciones de las Naciones Unidas
UN <- read.csv(paste0(aqui,"/un.csv"), header=FALSE, strip.white=TRUE)

# Preparar la base de datos
# Extraer la columna de países y atributos
pais <- UN[,1]
atributos <- matrix(UN[,2], length(UN[,2]), 1)
colnames(atributos) <- "party"

# Eliminar las columnas de país y atributos para quedarse solo con los votos
UN <- UN[,-c(1,2)]

# Crear un objeto de clase rollcall para el análisis con wnominate
rc <- rollcall(UN,             
               yea=c(1,2,3),
               nay=c(4,5,6),
               missing=c(7,8,9),
               notInLegis=0,
               legis.names=pais,
               legis.data=atributos,
               desc="UN 31 to 33")

# Ejecutar el análisis wnominate
result <- wnominate(rc, dims=2, polarity=c(3,3))

# Mostrar un resumen de los resultados
summary(result)


# Explorar los componentes del objeto 'result'
names(result)
head(result$legislators)
head(result$rollcalls)
result$dimensions
head(result$eigenvalues)
result$beta
result$weights
result$fits

# Calcular el peso relativo de la segunda dimensión
WEIGHT = (result$weights[2]) / (result$weights[1])

# Extraer las coordenadas de las dimensiones
X1 <- result$legislators$coord1D
X2 <- (result$legislators$coord2D) * WEIGHT # (su peso es menor)

# Crear un gráfico de dispersión para visualizar las dimensiones
plot(X1, X2, type="n", asp=1,
     xlab="1a dimension",
     ylab="2a dimension",
     xlim=c(-1.0, 1.0), ylim=c(-1.0, 1.0), font=2, cex=1.2)
mtext("Naciones Unidas: 31 - 33", side=3, line=1.50, cex=1.2, font=2)
points(X1[result$legislators$party == "Other"], 
       X2[result$legislators$party == "Other"], pch=16, col="blue", font=2)
points(X1[result$legislators$party == "WP"], 
       X2[result$legislators$party == "WP"], pch=16, col="red", font=2)



# Gráficos para visualizar los resultados de wnominate
plot.coords(result)  # Gráfico de coordenadas separado por grupos
plot.scree(result)  # Gráfico para mostrar la importancia de cada eigenvalue
plot.cutlines(result, lines=5)  # Gráfico de líneas de corte para cada proyecto
plot.angles(result)  # Histograma de ángulos de corte


#------------------------------------------------------------------------------
# wnominate - ejemplo Senado Chile
#------------------------------------------------------------------------------

# Leer los datos del archivo CSV
senado <- read.csv(paste0(aqui,"/votos2014_2016_procesado.csv"), sep=";")
# Extraer los nombres de los legisladores
nombres <- senado[,1]
# Eliminar la columna de nombres para quedarse solo con los votos
senado <- senado[,2:NCOL(senado)]

# Crear un objeto de clase rollcall para el análisis con wnominate
rc_senado <- rollcall(senado,             
                      yea=c(1),
                      nay=c(-1),
                      missing=c(0),
                      notInLegis=NULL,
                      legis.names=nombres,
                      legis.data=NULL,
                      desc="Senado 2014-15")

# Ejecutar el análisis wnominate
result_senado <- wnominate(rc_senado, dims=2, polarity=c(35,35))

# Mostrar un resumen de los resultados
summary(result_senado)

# Gráfico de los resultados
plot(result_senado)

# Calcular el peso relativo de la segunda dimensión
WEIGHT = (result_senado$weights[2]) / (result_senado$weights[1])

# Extraer las coordenadas de las dimensiones
X1 <- result_senado$legislators$coord1D
X2 <- (result_senado$legislators$coord2D) * WEIGHT

# Análisis detallado de un proyecto específico (proyecto número 27)
proyecto <- 27
# Extraer los votos para el proyecto específico
voto <- as.integer(rc_senado$votes[,proyecto])

# Extraer parámetros del rollcall para el proyecto
DL1 <- result_senado$rollcalls[proyecto,7]
DL2 <- result_senado$rollcalls[proyecto,8]
ZM1 <- result_senado$rollcalls[proyecto,9]
ZM2 <- result_senado$rollcalls[proyecto,10]

# Cálculos adicionales para el gráfico y análisis
YEA1 <- ZM1 - DL1
YEA2W <- (ZM2 - DL2) * WEIGHT
NAY1 <- ZM1 + DL1
NAY2W <- (ZM2 + DL2) * WEIGHT
A1 <- NAY1 - YEA1
A2 <- NAY2W - YEA2W
ALENGTH <- sqrt(A1 * A1 + A2 * A2)
N1W <- A1 / ALENGTH
N2W <- A2 / ALENGTH

# Asegurarse de que las distancias en eje X sean positivas
if (N1W < 0){
  N1W <- -N1W
  N2W <- -N2W
}

# Ponderación de pesos
ws <- N1W * ZM1 + N2W * ZM2 * WEIGHT
xws <- ws * N1W
yws <- ws * N2W

# Gráfico de las dimensiones ideológicas estimadas con WNOMINATE
plot(X1, X2,
     xlab="1ra dimensión (izquierda-derecha)",
     ylab="2da Dimensión",
     pch=16,
     main="Senado - WNOMINATE - Proyecto de ley")
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, col="black", lwd=2)

############### 
# Bootstrap para estimar la incertidumbre en WNOMINATE
library(ellipse)
result_senado_boot <- wnominate(rc_senado,
                                ubeta=15,  # Valor por defecto para el parámetro beta
                                uweights=0.5,  # Valor por defecto para los pesos
                                dims=2,  # Número de dimensiones
                                minvotes=10,  # Núm mínimo para incluir en el análisis
                                lop=0.025,  # Parámetro para el método de optimización
                                trials=3,  # Número de iteraciones de bootstrap
                                # esto debería ser 10000 o más, pero es lento
                                polarity=c(1,5),  # Polaridad para las dimensiones
                                verbose=FALSE)  # No mostrar mensajes durante la ejec.

# # Extraer las desviaciones estándar de las dimensiones
std1 <- result_senado_boot$legislators$se1D
std2 <- result_senado_boot$legislators$se2D * WEIGHT
corr12 <- result_senado_boot$legislators$corr.1

# # Gráfico de las dimensiones con intervalos de confianza
plot(X1, X2,
     xlab="1ra dimensión (izquierda-derecha)",
     ylab="2da Dimensión",
     pch=16,
     main="Ejemplo - WNOMINATE",
     type="n")

# Dibujar elipses para representar los intervalos de confianza
for (i in 1:nrow(result_senado_boot$legislators)){
  if(!is.na(corr12[i])){
    lines(c(X1[i],X1[i]),
          c(X2[i]- 1.96*std2[i], X2[i] + 1.96*std2[i]),
          col="gray")
    lines(c(X1[i] - 1.96*std1[i], X1[i] + 1.96*std1[i]),
          c(X2[i], X2[i]),
          col="gray")
    if (abs(corr12[i]) > .30){
      lines(ellipse(x=corr12[i],
                    scale=c(std1[i],std2[i]),
                    centre=c(X1[i] ,X2[i])),
            col="gray")
    }
  }
}
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, lwd=2, col="black")
polarity <- X1*N1W + X2*N2W - ws



###################33
# Ejecutar el modelo MCMC
mcmc_e <- ideal(rc, codes = rc$codes,
                maxiter = 8000,  # Número máximo de iteraciones
                burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
                thin = 500,      # Intervalo para guardar muestras
                normalize = T)   # Normalizar los datos (ver documentación para más detalles)

# Comparar resultados entre la estimación Bayesiana y W-NOMINATE
plot(-mcmc_e$xbar[,1], result$legislators$coord1D, 
     xlab="Estimación Bayesiana (1ª Dimensión)", 
     ylab="W-NOMINATE (1ª Dimensión)")
# Nota: Se multiplica por -1 para alinear las escalas. Esto es válido ya que 
# estamos interesados en la relación, no en la magnitud.

# Obtener y visualizar los márgenes de error
mcmc.df <- as.data.frame(mcmc_e$x)
hist(mcmc.df[,1])  # Histograma para la primera dimensión
plot(density(mcmc.df[,1]), xlim=c(-1,1))  # Densidad para EEUU
lines(density(mcmc.df[,2]), col="red")  # Densidad para Canada
lines(density(mcmc.df[,3]), col="green")  # Densidad para Cuba
lines(density(mcmc.df[,20]), col="blue")  # Densidad para Chile



# --------------------------------------------------------------
# Estimación de Ideología Política a partir de Encuestas - tscores
# --------------------------------------------------------------

# Importar la biblioteca necesaria
library(basicspace)

# Leer los datos desde un archivo CSV
base <- read.csv(paste0(aqui,"/ejemplo_basicspace.csv"))

# Tomar una muestra aleatoria de 1500 filas (necesario debido a 
# limitaciones del algoritmo)
muestra <- sample(row.names(base), 1500)
base <- base[row.names(base) %in% muestra,]

# Preparar los datos para el análisis de tscores
# Convertir las columnas relevantes a numéricas
tscores <- cbind(as.numeric(base$t_allende),
                 as.numeric(base$t_pinochet),
                 as.numeric(base$t_aylwin),
                 as.numeric(base$t_frei),
                 as.numeric(base$t_lagos),
                 as.numeric(base$t_bachelet1),
                 as.numeric(base$t_pinera),
                 as.numeric(base$t_bachelet2))

# Asignar nombres de columnas y convertir a matriz
colnames(tscores) <- c("Allende","Pinochet","Aylwin","Frei","Lagos",
                       "Bachelet1","Pinera","Bachelet2")
tscores <- as.matrix(tscores)

# Ejecutar el modelo Blackbox para estimar la ideología
results_bb <- blackbox(tscores, 
                       missing=c(8,9), 
                       verbose=T, 
                       dim=2, 
                       minscale=3)

# Visualizar las estimaciones de ideología para los individuos
d1 <- unlist(results_bb$individuals[1])
plot(density(d1, na.rm = T), main="Distribución Ideológica de los Encuestados")

# Incorporar los resultados en el conjunto de datos original
psi.hat <- -results_bb$individuals[[1]]$c1
psi.hat.df <- as.data.frame(psi.hat)
colnames(psi.hat.df) <- c("ideologia")
base$ideologia <- unlist(psi.hat.df)

# Estimar la posición ideológica de los tscores
results_bb.t <- blackbox_transpose(tscores, dims=1, minscale = 3)
ideologia_tscores <- -results_bb.t$stimuli[[1]][2]

# Visualizar las estimaciones de ideología para los tscores
# Los puntos representan diferentes tscores y los colores pueden ser ajustados 
# para representar diferentes categorías
plot(density(d1, na.rm = T), main="Distribución Ideológica de los Encuestados")
points(ideologia_tscores[1,1],0,col="red", type='p',pch=19)


# --------------------------------------------------------------
# Estimación de Ideología Dinámica
# --------------------------------------------------------------

# Importar la biblioteca necesaria
require(emIRT)

# Leer los datos desde un archivo CSV
archivo <- paste0(aqui,"/20170127data_integrado_revisado_solovotosefectivos.csv")
data <- read.csv(archivo)

# Preparar los datos para el análisis
votes <- data[,2:NCOL(data)]
votes <- as.matrix(votes)
J <- NCOL(votes)

#################################
# para estimacion dinamica necesitamos cuatro set de datos
# data: votos, 
#       periodo de inicio (starts),
#       periodo de termino (ends),
#       numero de fallo (bill.session)
#       numero de fallos/periodo (asumiremos cada fallo es un periodo)

startlegis <- matrix(0,nrow=NROW(votes), ncol=1)
bill.session <- seq(1:NCOL(votes))
bill.session <- bill.session - 1 
bill.session <- as.matrix(bill.session)

endlegis <- matrix(NCOL(votes)-1,nrow=NROW(votes), ncol=1)

panel.data <- list(rc = votes,
                   startlegis = startlegis,
                   endlegis = endlegis, 
                   bill.session = bill.session,
                   T = NCOL(votes)) # cambiar por s

# starts:
# alpha: parametro de dificultad de orden J*1
# beta: parametro de discriminacion de orden J*1
# x: matriz de N*T

alpha <- matrix(0.1,nrow=NCOL(votes), ncol=1) # 0.1 arbitrario
beta <- matrix(-0.1,nrow=NCOL(votes), ncol=1) # -0.1 arbitrario
#x <- matrix(0,nrow=NROW(votes), ncol=s)
x <- matrix(0,nrow=NROW(votes), ncol=NCOL(votes))
starts.points <- list(alpha = alpha, beta = beta, x = x)

# priors:
# x.mu0: promedio inicial para ideal points de cada legislador de orden N*1
# x.sigma0: varianza inicial para cada estimacion
# beta.mu: promedio inicial para todos los bills, de orden 2*1
# beta.sigma: promedio inicial de varianza para todos los bills, de orden 2*2
# omega2: evolucion de la varianza por legislador

#x.mu0 <- result.w$legislators$coord1D # estimacion wnominate como punto de partida
x.mu0 <- matrix(0,nrow=NROW(votes),ncol=1) # siguiendo ejemplo en paquete emIRT
x.sigma0 <- matrix(1, nrow=NROW(votes),ncol=1) # arbitrariamente prior en 1 
beta.mu <- matrix(NA,nrow=2,ncol=1)
beta.mu[1,1] <- 0
beta.mu[2,1] <- 0
beta.sigma <- matrix(NA,nrow=2,ncol=2)
beta.sigma[1,1] <- 1
beta.sigma[2,1] <- 0
beta.sigma[1,2] <- 0
beta.sigma[2,2] <- 1
omega2 <- matrix(0.1,nrow=NROW(votes),1) # 0.1 siguiendo ejemplo en paquete

priors.points <- list(x.mu0 = x.mu0,
                      x.sigma0 = x.sigma0,
                      beta.mu = beta.mu,
                      beta.sigma = beta.sigma, 
                      omega2 = omega2)

base <- list(base.data = panel.data,
             base.cur = starts.points, 
             base.priors = priors.points)

result <- dynIRT(.data = base$base.data,
                 .starts = base$base.cur,
                 .priors = base$base.priors)

ideology <- result$means$x
row.names(ideology) <- data[,1]

# Veamos el resultado
ej1 <- 1
ej2 <- 36
ej3 <- 5
ej4 <- 25
ej5 <- 15

plot(ideology[ej1,],
     ylim=c(-1,18),
     col="blue",
     type="l")
text(100,8,row.names(ideology)[ej1], col="blue", cex = 0.75)
lines(ideology[ej2,],
      col="red")
text(100,7,row.names(ideology)[ej2], col="red", cex = 0.75)
lines(ideology[ej3,],
      col="green")
text(100,6,row.names(ideology)[ej3], col="green", cex = 0.75)
lines(ideology[ej4,],
      col="black")
text(100,5,row.names(ideology)[ej4], col="black", cex = 0.75)
lines(ideology[ej5,],
      col="grey")
text(100,4,row.names(ideology)[ej5], col="grey", cex = 0.75)



# Estimación con textos - Wordfish

library(quanteda)
library(quanteda.textmodels)
library(stopwords)
library(ggplot2)

# Luis Larrain (LyD)
autor1 <- c("Gabriel Boric tenía en la conmemoración del 11 de septiembre una carta valiosa para sacar a su gobierno del sello de derrota. Un presupuesto millonario (aún no hay claridad sobre su monto) y una predisposición positiva de la oposición a ser parte de una reconciliación nacional mirando al futuro. Pero como otras veces, nuestro novel gobernante desperdició su carta por exceso de ambición y escasez de visión.

En lugar de buscar acuerdo en la condena a la violación de los derechos humanos durante el gobierno militar y un compromiso por la democracia y renuncia al uso de violencia en política, que era posible, Boric intentó una suerte de canonización de Salvador Allende y la condena unánime y oficial al Golpe de Estado, en la que el país no está de acuerdo. En otras palabras, quiso transformar la conmemoración en una oportunidad para obtener una ventaja política para la izquierda.

Con esa decisión, Boric mostró la madera de la que está hecho, no la de un estadista sino la de un líder partisano. Se excedió: lo transformó en un espectáculo, no una reflexión; en una producción de eventos, no en actos solemnes de unidad entre los chilenos. Abundó el merchandising, anteojos de Allende, sus zapatos; como si se tratara de un rockstar y no un Presidente de Chile cuyo gobierno inspiró a muchos, pero defraudó e hizo sufrir a más. Obnubilado por el mito de Allende en el extranjero, pretendió imponerlo al interior de Chile, sin entender que quienes vivieron la Unidad Popular ya tienen su veredicto y los más jóvenes tuvieron la posibilidad de ver una sinopsis de la UP y sus consecuencias en el estallido de octubre de 2019. El resultado, según revelan las encuestas, es que algo más de la mitad de los chilenos opina hoy que el Golpe fue inevitable. La responsabilidad que se le atribuye a Allende y su gobierno es tanta o más de la que se carga a los jefes militares. Una derrota completa para Boric y su objetivo de sacar ventaja de esta conmemoración. Adicionalmente, empeoró el clima político al enfrentar a la oposición y también, por ende, las posibilidades de aprobar sus reformas. Debió escuchar a mentes más preclaras que la suya en la izquierda. A Óscar Guillermo Garretón que condena el Golpe con fuerza y defiende a Allende por cómo lo enfrentó, aunque admite que su gobierno fracasó por errores propios. Al no hacerse esta distinción, el socialismo renunció a su mayor activo: su renovación y éxito durante los treinta años. A Sergio Muñoz Riveros, sobreviviente del 11 de septiembre, que acepta que no debe haber relatos oficiales y reconoce que hubo dictadura porque hubo un gobierno de Allende.

El Senado, convocado por el senador Coloma, sí fue capaz de acordar una declaración sobre el 11 de septiembre. La prueba que faltaba para demostrar que Boric es un liderazgo que divide.")

# Francisco HUenchumilla (senador DC)
autor2 <- c("A 50 años del Golpe de 1973, este trágico acontecimiento de nuestra historia sigue dividiendo al país. No hemos podido encontrar la serenidad necesaria para tener una explicación compartida.

Probablemente no la tengamos nunca, a menos que el transcurso del tiempo devenga en nuevas generaciones, sin las heridas del pasado. Hubiese esperado un mínimo común, pero nada de eso se vislumbra. Nos quedamos paralizados y sin capacidad de reacción; al contrario de los uruguayos, con sus presidentes, que han depuesto viejas cuentas y hoy están unidos por un destino común.

El Golpe ocurrió en un escenario político distinto al que vivimos en el siglo XXI. El mundo estaba dividido entre dos polos, dirigidos cada uno por un imperio, y disputándose una hegemonía mundial. Pero esta historia partió hace unos 300 años atrás, cuando comenzó la modernidad con la Revolución Industrial, y la sociedad se organizó en torno al capitalismo y la democracia liberal representativa. Desde entonces hubo una sola manera de organizar la sociedad humana, que campeó sin competencia concreta; hasta el siglo XX, en que la Revolución Rusa de octubre de 1917 clavó en la historia la consigna de que había una manera distinta de organizar la sociedad. Desde entonces el mundo se polarizó, en torno a estos dos modelos diametralmente diferentes de organizar la economía y la política.

Esta polarización se acentuó, y se consolidó, después de la Segunda Guerra Mundial, donde surgieron como potencias nucleares Estados Unidos y la Unión Soviética. La competencia por uno de estos dos modelos se tomó la discusión política en el mundo, y cada uno fue alineándose conforme a las distintas ideologías y tendencias. Cada una de estas potencias, con sus zonas de influencia, en una carrera por el dominio del mundo.

Chile, no obstante su lejanía del centro, no fue ajeno a esta polarización.

Así fue como en la década del 30 nació la Falange Nacional –antecesora de la Democracia Cristiana, probablemente el partido político más importante del siglo 20– con el discurso de superar el capitalismo y el socialismo y situándose por encima, como una tercera vía. Este proceso culminó con la abierta competencia, en la década del sesenta, entre la Revolución en Libertad y el socialismo que planteaba la Unidad Popular. Todos conocemos el desenlace que tuvieron ambos experimentos.

Está claro –aunque no sé si todos han logrado comprenderlo– que ninguna de las potencias hegemónicas iba permitir que surgieran, en sus respectivas zonas de influencia, experimentos que tuvieran como como objetivo el cambio del modelo. No lo permitió la Unión Soviética, cuando invadió Hungría y Checoslovaquia; y no lo permitió Estados Unidos, con sus numerosas intervenciones en América Central y en otras partes del mundo, y de manera especial en la Crisis de los Misiles en Cuba –junto a toda su política con este país.

¿Podría pensarse que Estados Unidos iba a permanecer indiferente, o neutral, frente al cambio de modelo planteado por la Unidad Popular, cuando tres de sus principales partidos –el PC, el PS y el MAPU– se declaraban marxistas leninistas? Estos partidos eran tributarios, en efecto, de la doctrina que inspiraba a la Unión Soviética, su competidor por la hegemonía mundial, y el paradigma de esa forma alternativa de organizar la sociedad. Por supuesto que Estados Unidos, conforme a sus propios objetivos de defensa de sus intereses estratégicos en el mundo, desde el día uno –y con la complicidad de distintos sectores al interior de la sociedad chilena– empezó a mover las piezas del tablero de ajedrez para impedir el acceso al poder de Salvador Allende. Y después, a desestabilizar su gobierno y contribuir con su estrepitosa caída.

Pero a fines de la década de los 80 desapareció la Unión Soviética, y con ella, el modelo alternativo que desafió al capitalismo y a la democracia liberal. Fukuyama declaró el fin de la historia. Estaba equivocado. La historia sigue su curso.

Mientras tanto, en el Chile de hoy, la centroizquierda parece no encontrar su destino, debatiéndose en luchas intestinas por el simple control del poder gubernamental, y sin encontrar un horizonte estratégico. No quiere aceptar que hemos entrado a otro escenario mundial, donde el capitalismo campea triunfante. No lo digo yo, sino Zizek, el principal teórico actual del marxismo.

Ello no significa, sin embargo, la aceptación del capitalismo como una organización individualista, abusiva, clasista y racista; los sueños de una sociedad justa siguen vigentes, si bien ya no hay modelo. Hoy, simplemente, todos somos socialdemócratas. Esto debería ser un potente incentivo para la unidad, pero el control del poder parece ser más fuerte.

En el horizonte se asoma un nuevo mundo, con otros modelos; y donde seguramente, cuando se cumplan 100 años de aquel Golpe que fracturó a la sociedad chilena, nuestros nietos harán un esfuerzo para emular a nuestros amigos los uruguayos.

Mientras tanto, China avanza con un capitalismo de Estado y un centralismo político, que auguran un nuevo modelo para disputar la hegemonía mundial. La Inteligencia Artificial, por su parte, cambiará todos los paradigmas.

Pero en este rincón del mundo, la ultraderecha se apresta a obtener una victoria pírrica, reviviendo una sociedad conservadora y clasista.

¿Será capaz la centroizquierda de estar a la altura de estos desafíos, dejando atrás las peleas cortas? ¿Será capaz de levantar una alternativa que, de acuerdo con el avance del mundo, le haga sentido a las chilenas y chilenos para construir los viejos y permanentes sueños de una sociedad más justa?")

# Jorge Gómez (FPP)
autor3 <- c("Eduardo Frei Montalva, en el prólogo al libro de Genaro Arriagada De la vía chilena a la vía insurreccional, publicado en 1974, plantea que: «El violentismo es una forma mesiánica de los que sabiéndose minoría sin destino se auto estiman portadores de la “verdad” por encima de la voluntad del pueblo. Ellos constituyen una nueva forma de una plutocracia mental que cree pensar por el hombre, al cual consideran en el fondo incapaz de expresarse y conquistar su propio destino. Ellos creen saber lo que conviene y lo que es útil destruir para conseguir sus objetivos. Ellos jamás podrán construir en la paz y en la solidaridad, sin lo cual ninguna forma social es humana y creadora».

Lo que describe Frei Montalva es lo que Raymond Aron denominaba como ingenieros de almas. Aquellos fanáticos que presumiéndose del lado correcto de la Historia o de la verdad, presumen tener permiso para recurrir a cualquier tipo de medio en nombre de un fin superior. Un fanatismo que, a inicios de los años 60, ya advertía como problemático un pensador agudo como Jorge Millas, quien decía que ahí yace la problemática asimilación de que el fin justifica los medios. Millas, advertía que a partir de eso surge la figura del inquisidor o el comisario, que son dos formas de aberración espiritual. De ahí a validar la violencia como medio para acabar con los revisionistas o los herejes, hay un paso.

El fanatismo es la extrema moralización de la política donde unos son consideramos como santos y otros como demonios. Si además tomamos en cuenta los efectos perlocutivos del lenguaje, es esa ilusión, de una especie de redención a manos de quienes se presumen santos, la que se instala en desmedro del pluralismo político y marca el posterior desmoronamiento del plano político, jurídico y democrático en Chile. El lenguaje hace manifiestas intencionalidades, que pueden cumplirse o no, pero que en un contexto político moralizado y polarizado por el dogmatismo como lo era el contexto chileno de esos años, puede tener mayores efectos en otros.

¿Qué sucede en una sociedad donde retóricamente se asume que es posible aniquilar a otros? ¿Qué tipo de política se constituye a partir del imperio del dogmatismo? En ese sentido ¿cuánto contribuyeron las posturas fanáticas a sembrar el desdén por la vida ajena y el decisionismo político, en el Chile de los años 60, 70 y 80?

Extrañamente, la reflexión del ex presidente Frei Montalva es eludida o ha sido eludida en las discusiones actuales a propósito de los 50 años del quiebre democrático. En parte es comprensible porque nadie quiere asumir la carga del fanatismo criminal que se esconde cuando alguien vindica la violencia como medio para cumplir un fin. Es también probable que muchos intenten eludirlo porque es también el fanatismo el que alimenta ese mesianismo desde el cual algunos han presumido tener una moral superior en pleno siglo XXI. Es el fanatismo el que también alimenta el sectarismo. Y peor aún, el que alimenta la deshumanización de quienes disienten. ¿Cuánto se ha reflexionado al respecto? Quizás poco en realidad. Lo que actualmente se plantea como una reflexión respecto al quiebre democrático ocurrido hace cincuenta años más bien parece esas discusiones infantiles donde las partes se acusan mutuamente sin lograr dirimir nada. En el fondo se busca eludir algo que los actores de la época asumían como opción, abierta o soterradamente: aniquilar a otros. Ahí, en esa concepción anti política, en esa distorsión respecto a los medios, subyace el problema fundamental del período en cuestión. Porque la retórica de la violencia opera como una fuerza cuya inercia adquiere cursos inesperados e impredecibles. Se devora a víctimas y victimarios. Por eso debe estar bien encerrada bajo marcos normativos legales y éticos.

Jorge Millas y Eduardo Frei Montalva no sólo tuvieron una postura crítica al fanatismo revolucionario que subyacía a la Unidad Popular, sino también frente a la dictadura de Pinochet. Como bien planteaba Millas en su discurso en el Caupolicán en 1980, los defectos de la democracia «se corrigen en virtud de su propio dinamismo, porque su esencia está en el anti-dogmatismo, el anti-mesianismo, el anti-personalismo». Eso implicaba defender el libre examen y la búsqueda de lo razonable, algo que se había perdido para Millas bajo la dictadura y las pasiones desenfrenadas.

Quizás la lección primordial es que la violencia, en cualquier de sus formas, en una democracia, no es un medio aceptable. Por imperfecta que se considere esa democracia. Que el violentismo surja no significa darle una connotación ética como medio a la violencia. Como decía Jorge Millas en su discurso donde vindica la democracia frente al autoritarismo: «no hay gracia alguna en proteger la democracia desnaturalizándola». Por aquí debería surgir cualquier acuerdo a propósito de los 50 años del golpe.")

docs <- quanteda::corpus(c(doc1=autor1,doc2=autor2, doc3=autor3))
dtm <- tokens(docs, remove_punct = TRUE, remove_numbers = TRUE) %>% tokens_remove(stopwords("spanish"))
dtm <- dfm(dtm)
wf_result <- textmodel_wordfish(dtm)

# Extraer los resultados
nombres <-rbind("Larrain","Huenchumilla","Gómez")
ideol <- wf_result$theta
wf_data <- as.data.frame(cbind(nombres,ideol))
colnames(wf_data) <- c("Autor", "Posicion_Ideologica")

ggplot(wf_data, aes(x = Posicion_Ideologica, y = 0)) +
  geom_point(aes(color = Autor), size = 5) +
  geom_text(aes(label = Autor, color = Autor), vjust = -1.5) +
  scale_y_continuous(limits = c(-1, 1), breaks = NULL) +
  ggtitle("Estimación de Posiciones Ideológicas con Wordfish") +
  xlab("Posición Ideológica") +
  ylab("") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")  # Línea que representa el eje ideológico

palabras_ideol <- cbind(wf_result$features,wf_result$psi)
palabras_ideol <- as.data.frame(palabras_ideol)
palabras_ideol$V2 <- as.numeric(palabras_ideol$V2)
palabras_ideol <- palabras_ideol[order(palabras_ideol$V2, decreasing = T),]
n2 <- NROW(palabras_ideol)
n1 <- NROW(palabras_ideol)-10
palabras_ideol[c(1:10,n1:n2),]


# Estimación con textos: wordscore

word_loadings <- as.data.frame(cbind(wf_result$features,wf_result$psi))
word_data <- data.frame(Palabra = word_loadings$V1,
                        Carga_Ideologica = as.numeric(word_loadings$V2))
palabras_seleccionadas <- word_data[order(word_data$Carga_Ideologica), ][c(1,10,15, (nrow(word_data) - 20), (nrow(word_data) - 10),nrow(word_data)), ]

plot(density(word_data$Carga_Ideologica))

ggplot(palabras_seleccionadas, aes(x = Carga_Ideologica, y=0)) +
  geom_point(aes(color = Carga_Ideologica), size = 5) +
  geom_text(aes(label = Palabra, color = Carga_Ideologica), vjust = -0.5, angle=90) +
  ggtitle("Carga Ideológica de Palabras Específicas") +
  xlab("Palabras") +
  ylab("Carga Ideológica") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")  # Línea que representa el eje ideológico

library(knitr)
library(kableExtra)
library(magrittr)
tabla <- c("","Wordfish","Wordscore",
           "Modelo Estadístico","Conteo Poisson para estimar posiciones","Cálculo de simulitudes",
           "Supervisión","No supervisado","Supervisado",
           "Dimensiones","Una dimensión ideológica, aunque se pueden extender a más dimensiones","Múltiples dimensiones si se dispone de datos de entrenamiento para cada una",
           "Parámetros Estimados","Estima la posición ideológica de cada documento y la 'carga' ideológica de cada palabra en el vocabulario","Estima la posición ideológica de cada documento en función de su similitud con los documentos de entrenamiento",
           "Interpretación","Las estimaciones son relativas entre sí y se centran en la variación a lo largo de una dimensión","Las estimaciones se basan en la similitud con los documentos de referencia y, por lo tanto, son absolutas en la escala ideológica del conjunto de entrenamiento",
           "Uso","Adecuado para comparar documentos dentro de un único corpus","Adecuado para comparar documentos entre diferentes corpus cuando hay conjunto de entrenamiento adecuado"
)
tabla <- matrix(tabla,nrow=7,ncol=3, byrow=TRUE)
kable(tabla,align = "lll") %>% kable_classic(full_width = F, html_font = "Cambria")

library(manifestoR)
mp_setapikey(paste0(aqui,"/docs/keyManifestoR.txt"))
mpds <- mp_maindataset()
micorpus <- mp_corpus(subset(mp_maindataset(), countryname == "Chile"))

mp_view_originals(party == 155021 & date == 198912) #Programa gobierno Concertacion 1989
#mp_view_originals(party == 155021 & date == 199312) #Programa gobierno Concertacion 1993
# mp_view_originals(party == 155021 & date == 199912) #Programa gobierno Concertacion 1999
# mp_view_originals(party == 155021 & date == 200512) #Programa gobierno Concertacion 2005
# mp_view_originals(party == 155021 & date == 200912) #Programa gobierno Concertacion 2009
# mp_view_originals(party == 155021 & date == 201311) #Programa gobierno Nva Mayoria 2013
# mp_view_originals(party == 155021 & date == 201711) #Programa gobierno Nva Mayoria 2017
head(content(micorpus[[1]]))

mp_describe_code("503")
mp_describe_code("504")
mp_describe_code("505")

texto <-  micorpus[["155021_198912"]]
doc_subcodes <- subset(texto, codes(texto) %in% c(503, 504, 505))
length(doc_subcodes)

# ejemplo: texto y codificación
doc_subcodes$content$text[[1]]
doc_subcodes$content$cmp_code[[1]]

ideol <- rile(subset(mpds, countryname == "Chile"))
textos <- matrix(NA,nrow=length(micorpus),ncol=2)
textos <- as.data.frame(textos)

for(j in 1:length(micorpus)){
  doc <-  micorpus$content[j]
  textos[j,1] <- paste(doc[[1]]$content$text, collapse = " ")
  textos[j,2] <- ideol[j]
}

# textos supervisados
textos_s <- c(textos$V1[1],textos$V1[2])  
docnames_supervisados <- c("Aylwin","Buchi")
scores_supervisados <- c(textos$V2[1],textos$V2[2])

corpus_supervisado <- corpus(textos_s, docnames = docnames_supervisados)
docvars(corpus_supervisado, "score") <- scores_supervisados

# documentos a evaluar
textos_ns <- c(autor1,autor2,autor3)  
docnames_no_supervisados <- nombres
corpus_no_supervisados <- corpus(textos_ns, docnames = docnames_no_supervisados)

# Creamos DTM para ambos conjuntos de textos supervisados y no supervisados
dtm_supervised <- dfm(textos_s)
dtm_unsupervised <- dfm(textos_ns)

# Aplicamos el modelo de Wordscores
model_wordscores <- textmodel_wordscores(dtm_supervised, docvars(corpus_supervisado, "score"))

# Aplicar el modelo a textos no supervisados
predicted_scores <- predict(model_wordscores, newdata = dtm_unsupervised)

# ahora rescatamos los valores predichos en un nuevo dataframe
df <- data.frame(DocName = names(predicted_scores),
                 Score = as.numeric(predicted_scores))
df <- df[order(df$Score), ]

# y visualizamos
ggplot(df, aes(x = Score, y = 0)) +
  geom_point(aes(color = nombres), size = 5) +
  geom_text(aes(label = nombres, color = nombres), vjust = -1.5) +
  scale_y_continuous(limits = c(0, 5), breaks = NULL) +
  ggtitle("Posiciones Ideológicas Estimadas de Documentos No Supervisados") +
  xlab("Posición Ideológica") +
  ylab("") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")


