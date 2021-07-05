## 0 ## -----------------------------------------------------------------------
# Se escogen 5 valores al azar
set.seed(603)
idx <- sort(as.character(sample(1:10, size = 5, replace = F)), decreasing = F)

VAR <- vector(mode = "list", length = length(idx))
for (i in idx){ # Bucle de lectura de los archivos; se almacenan en lista
  VAR[[which(idx==i)]] <- read.table(paste0("VAR", i,".txt"), header = T)
  rm(i)
}

## 1 ## -----------------------------------------------------------------------
# Tabla I: Estadísticos
library(tidyverse)

tabla_i <- data.frame()
for (d in 1:length(VAR)){
  day <- unique(VAR[[d]]$day)
  pre_subtabla_i <- data.frame()
  for (t in day){
    VAR_day <- VAR[[d]] %>%
      filter(day == t) %>%
      mutate(dqmp = qmp - lag(qmp))
    row = data.frame(QC = sum(table(VAR_day$dqmp[VAR_day$dqmp != 0])),
                     MV = sum(VAR_day$size[VAR_day$size != 0]))
    pre_subtabla_i <- rbind(pre_subtabla_i, row)
  }
  pre_subtabla_i <- pre_subtabla_i %>%
    summarise(QC = mean(QC, na.rm = T),
              MV = mean(MV, na.rm = T)) %>%
    # NT son las transacciones del mes, no la media diaria.
    # Como NT, MS se calcula sobe todo el mes.
    mutate(NT = sum(table(VAR[[d]]$sign[VAR[[d]]$sign != 0])),
           MS = mean(VAR_day$size[VAR_day$size != 0], na.rm = T))
  tabla_i <- rbind(tabla_i, pre_subtabla_i)
  rm(d, t, row, pre_subtabla_i, VAR_day)
}
rownames(tabla_i) <- paste0("VAR",idx)

## 2 ## -----------------------------------------------------------------------
library(vars)
# Tabla II: VAR estructural de Hasbrouck (1991a) en 2 versiones diferentes para
# representar la negociación:
# a. Utilizando el signo de las transacciones
# b. Utilizando el tamaño de las transacciones con su correspondiente signo.

# Los cambios en el punto medio deben calcularse con respecto a la observación
# anterior y en logaritmos neperianos (rentabilidades en capitalización
# compuesta continua): Deltaq = ln(qt) - ln(qt-1)

# El número de retardos en el modelo debe obtenerse de forma óptima con AIC.
# Como máximo: 10 retardos.

# Parece que NO hay que filtrar por día. Es decir, solo cabe un modelo VAR por
# activo.


## a ## VAR utilizando el signo de las transacciones

# Test
VAR[[1]]$dqmp = ifelse(VAR[[1]]$day == lag(VAR[[1]]$day), log(VAR[[1]]$qmp) - log(lag(VAR[[1]]$qmp)), NA)
VAR2VAR <- subset(VAR[[1]], select = c(4, 6))
var.ord = VARselect(na.omit(VAR2VAR), lag.max = 10, type = "none")
as.numeric(var.ord$selection[1])

# Ahora se eliminan las 'as.numeric(var.ord$selection[1])' primeras
# observaciones de VAR de cada día.

VAR2VARclean <- data.frame()
for (d in day){
  VAR2_perday <- VAR[[1]] %>%
    filter(day == d) # Esto no evita dependencia interdía.
  VAR2VARclean <- rbind(VAR2VARclean, VAR2_perday[1+as.numeric(var.ord$selection[1]):nrow(VAR2_perday),])
}
VAR2VARclean <- subset(VAR2VARclean, select = c(6, 4))
model = VAR(na.omit(VAR2VARclean), type = "none", lag.max = 10)

Amat = diag(2); Amat[1,2] = NA
Amat

modelS = SVAR(model, Amat = Amat, max.iter = 500, lrtest = F)
str(modelS)

irf_sign2dqmp = irf(modelS, impulse = "sign", response = "dqmp", n.ahead = 20)


plot(irf_sign2dqmp)

# El bucle para hacerlo todo debe contener la generación del dqmp, VARselect y
# la selección para cada activo. Una vez que tenemos el orden se eliminan tantas
# observaciones al comienzo de cada día como 






# iVAR: a program for imputing missing data in multivariate time series using vector autoregressive models