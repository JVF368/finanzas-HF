## 0 ## ------------------------------------------------------------------

# Se escogen 3 acciones de cada grupo (al azar)
set.seed(603)
idx <- as.character(sample(1:5, size = 3, replace = F))
idx <- c(paste0("", idx), paste0("10", idx), paste0("20", idx))
idx2 <- list(idx[1:3], idx[4:6], idx[7:9])

LOB <- vector(mode = "list", length = length(idx))
for (i in idx){ # Bucle de lectura de los archivos; se almacenan en lista
  LOB[[which(idx==i)]] <- read.table(paste0("Stock", i, "LOB.txt"), header = T)
  rm(i)
}

## 1 ## -----------------------------------------------------------------
require(tidyverse)
require(lubridate)

# Calular las medidas de liquidez siguientes:
# 1
# Horquilla relativa media ponderada en puntos básicos para un round-trip de 100
# Horquilla relativa media ponderada en puntos básicos para un round-trip de 500
# Horquilla relativa media ponderada en puntos básicos para un round-trip de 1000
# A partir de la profundidad total (mostrada y oculta); por tanto lo que se
# pide es la horquilla efectiva relativa media ponderada
# Si la profundidad fuese insuficiente para los round-trip, eliminar el registro.
# 2
# Profundidad cotizada acumulada en Euros EN EL LADO ASK a estas distancias:
# 1 tick del punto medio (0.01 dolares)
# 5 ticks del punto medio (0.05 dolares)
# 10 ticks del punto medio (0.10 dolares)
# 20 ticks del punto medio (0.20 dolares)
# Tomar logaritmos neperianos sobre la magnitud obtenida

# Subtabla 1 Horquilla, tabla_i_ext (9 subtablas)
tabla_i_ext <- vector(mode = "list", length = length(LOB))
# Subtabla 2 Profunidad, tabla_i_ext2 (9 subtablas)
tabla_i_ext2 <- vector(mode = "list", length = length(LOB))

for (d in 1:length(LOB)){
  LOB[[d]] <- LOB[[d]] %>%
    mutate(vol = hvol + dvol, # se suman el volumen mostrado y el oculto
           datime = ymd(date) + hms("09:30:00") + time - 34200) # genera fecha y hora
  minute <- unique(LOB[[d]]$datime) # se extrae un vector con los minutos
  subtabla_i_ext <- data.frame() # se inicializan los data.frames depositarios
  subtabla_i_ext2 <- data.frame()
  for (t in 1:length(minute)){
    LOB_q <- LOB[[d]] %>%
      filter(datime == minute[t]) # filtra el libro de órdenes según minuto
    # Cálculo de los puntos medios:
    q <- (LOB_q$quote[LOB_q$sign == 1] + LOB_q$quote[LOB_q$sign == -1])/2
    ab <- function(part, rt_amt){
      # Función de cálculo del precio medio ask/bid
      # part debe tomar valores -1 (ask) o 1 (bid)
      # rt_amt es la cantidad de acciones del round trip.
      LOB_partmin <- LOB[[d]] %>%
        # filtra por minuto y signo de la transacción
        filter(datime == minute[t] & sign*part > 0) %>%
        arrange(abs(sign)) # ordena según prioridad de las operacions
      vol <- LOB_partmin$vol # se vectorizan los volúmenes...
      quote <- LOB_partmin$quote # ...y los precios
      # se inicializa un vector para almacenar los volúmenes del round-trip:
      vol_rt <- c() 
      for (i in 1:length(vol)){
        # El bucle almacena en el vector anterior la cantidad de acciones
        # del round-trip a cada precio diferente. El 0 en la func max
        # asegura que no tomará valores negativos cuando el round-trip se
        # haya completado.
        vol_rt[i] <- max(min(rt_amt-sum(vol[0:(i-1)]), vol[i]), 0)
      }
      # q_rt recoge el precio medio ask/bid del round-trip.
      # Si el round-trip no se ha completado devuelve NA.
      q_rt <- ifelse(sum(vol_rt) < rt_amt, NA, sum(vol_rt*quote)/sum(vol_rt))
      return(q_rt)
    }
    s <- function(rt_amt){
      # Función de cálculo de la horquilla
      # rt_amt es el mismo parámetro que en la func ab definida anteriormente.
      (log(ab(-1, rt_amt)) - log(ab(1, rt_amt)))*10000
    }
    a_tick <- function(part, ticks, tick){
      # part es el mismo parámetro que en ab; para ask es -1
      # ticks es el número de ticks
      # tick es la magnitud de cada tick (en USA 0.01 $)
      LOB_partmin <- LOB[[d]] %>%
        filter(datime == minute[t] & sign*part > 0) %>%
        arrange(abs(sign))
      vol <- LOB_partmin$vol
      quote <- LOB_partmin$quote # Estas 5 líneas son análogas en la func ab.
      vol_tick <- c() 
      for (i in 1:length(vol)){
        # El bucle busca todos los precios menores o iguales a q+(tick*ticks)
        # y devuelve los volúmenes asociados a estos. En caso de no existir
        # ningún volumen devuelve 0.
        vol_tick[i] <- ifelse(quote[i] <= q+(tick*ticks), vol[i], 0)
      }
      # La función devuelve el logaritmo de las unidades monetarias totales,
      # resultado de multiplicar el volumen de acciones hasta ese precio por
      # sus respectivos precios. El 0 en la func max censura los -Inf que 
      # resultan de aplicar logaritmos cuando no existe volumen a ese tick.
      return(max(log(sum(vol_tick*quote)), 0))
    }
    # en cell3 se almacenan los resultados de aplicar la función s de cálculo
    # de la horquilla con los parámetros deseados.
    cell3 <- data.frame(rt_100 = s(100), rt_500 = s(500), rt_1000 = s(1000))
    # Acumula los resultados por minuto:
    subtabla_i_ext <- rbind(subtabla_i_ext, cell3)
    # En cell4 se almacenan los resultados de la función a_tick de cálculo
    # de la profundidad con los parámetros deseados.
    cell4 <- data.frame(tick_1 = a_tick(-1, 1, .01),
                        tick_5 = a_tick(-1, 5, .01),
                        tick_10 = a_tick(-1, 10, .01),
                        tick_20 = a_tick(-1, 20, .01))
    cat(cell3[1,1], cell4[1,1]) # Muestra el progreso en pantalla.
    # Acumula los resultados por minuto:
    subtabla_i_ext2 <- rbind(subtabla_i_ext2, cell4)
  }
  # Se almacenan las tablas resultados en lista, convenientemente renombradas
  # las filas.
  tabla_i_ext[[d]] <- subtabla_i_ext
  rownames(tabla_i_ext[[d]]) <- minute
  tabla_i_ext2[[d]] <- subtabla_i_ext2
  rownames(tabla_i_ext2[[d]]) <- minute
  rm(d, t, ab, a_tick, s, q, minute, cell3, cell4, 
     subtabla_i_ext, subtabla_i_ext2, LOB_q)
}

# Tras efectuar los cálculos se han observado algunos valores negativos (o ceros)
# en las horquillas de algunos minutos. Buscando los datos originales se ha 
# observado que estas se correspondencon minutos en las que el mejor ask se situaba 
# por debajo del mejor bid (o al mismo nivel).
# Esto refleja una situación anómala en la que parece que no se han producido 
# transacciones a pesar de que los precios son convenientes para las partes.

# Sea por error de medición o recogida de los datos o bien por situaciones anómalas
# puntuales en la operatividad del sistema, se cree conveniente prescindir de la
# observación para ese minuto. Por practicidad se obvia el efecto que esto pudiera
# tener sobre la profundidad.
# En total se eliminan 4 minutos entre dos de los activos.
   
for (i in 1:length(tabla_i_ext)){
  # Con este bucle se limpia la tabla tabla_i_ext de todos aquellos registros
  # que contengan algún valor negativo de horquilla. Se conservan NA.
  tabla_i_ext[[i]] <- tabla_i_ext[[i]] %>% 
    filter((rt_100 > 0 & rt_500 > 0 & rt_1000 > 0) | 
             (rt_100 > 0 & rt_500 > 0 & is.na(rt_1000)) | 
             (rt_100 > 0 & is.na(rt_500) & rt_1000 > 0) | 
             (is.na(rt_100) & rt_500 > 0 & rt_1000 > 0) |
             (rt_100 > 0 & is.na(rt_500) & is.na(rt_1000)) |
             (is.na(rt_100) & rt_500 > 0 & is.na(rt_1000)) |
             (is.na(rt_100) & is.na(rt_500) & rt_1000 > 0) |
             (is.na(rt_100) & is.na(rt_500) & is.na(rt_1000)))
  rm(i)
}

## 2 ## -------------------------------------------------------------------

# 2
# Realizar un contraste de significatividad de las diferencias observadas entre
# las diferentes submuestras: peq-med, peq-gran, med-gran.
# Para ello será necesario:
# Las series diarias de la media en sección cruzada (para cada submuestra) de cada medida
# test estadístico de igualdad de medianas (rank-sum test de Wilcoxon)
# 3
# Analizar las diferencias entre la semana de crisis y las otras dos semanas
# Comentar e interpretar las diferencias.

# 1
# Para cada tabla en tabla_i_ext y tambla_i_ext2 tendré que:
# Convertir rownames en columna y, de esta, sacar año, mes y día (agregados)
# Agrupar según esta última agregación (group_by)
# Y hacer un summarise para sacar las medias para cada activo de las diferentes
# profundidades y horquillas relativas.
# Meter estos resúmenes en sendas listas de tablas resumen.
# 1. Para cada uno de estos resúmenes hacer la media de todos los días.
# 2. Agregar los resúmenes según grado de capitalización y hacer la media.
# Las dimensiones son: 9 activos + 3 grupos, 7 magnitudes
# La TABLA I presenta los resultados de esta forma:
# Tengo los 9 activos + 3 grupos en las filas
# Tengo las 7 magnitudes en las columnas

# En estas listas se almacenan las medias diarias de horquillas y profundidades.
tabla_i_sum <- vector(mode = "list", length = length(LOB))
tabla_i_sum2 <- vector(mode = "list", length = length(LOB))

for (i in 1:length(LOB)){
  # Se genera la variable date (fecha) para cada una de los activos.
  tabla_i_ext[[i]]$date <- as.Date(rownames(tabla_i_ext[[i]]))
  tabla_i_ext2[[i]]$date <- as.Date(rownames(tabla_i_ext2[[i]]))
  tabla_i_sum[[i]] <- tabla_i_ext[[i]] %>%
    group_by(date) %>% # agrupa por fecha (día) y genera las medias
    summarise(mean_s_rt_100 = mean(rt_100, na.rm = T),
              mean_s_rt_500 = mean(rt_500, na.rm = T),
              mean_s_rt_1000 = mean(rt_1000, na.rm = T))
  tabla_i_sum2[[i]] <- tabla_i_ext2[[i]] %>%
    group_by(date) %>% # idem.
    summarise(mean_tick_1 = mean(tick_1, na.rm = T),
              mean_tick_5 = mean(tick_5, na.rm = T),
              mean_tick_10 = mean(tick_10, na.rm = T),
              mean_tick_20 = mean(tick_20, na.rm = T))
  rm(i)
}

summarizer <- function(first, last){
  # Función que resume la información de las medias diarias:
  # INPUTS:
  # first: orden del primer activo a considerar
  # last: orden del último activo a considerar
  # Cuando first == last recoge solamente el de ese activo.
  # OUTPUTS:
  # sum: data.frame con las medias de las medidas de liquidez
  sum <- data.frame()
  for (i in first:last){
    summary <- merge(tabla_i_sum[[i]], tabla_i_sum2[[i]])
    sum <- rbind(sum, summary)
  }
  sum <- sum %>% # Se resume la información de los activos considerados
    summarise(mean_s_rt_100 = round(mean(mean_s_rt_100), 3),
              mean_s_rt_500 = round(mean(mean_s_rt_500), 3),
              mean_s_rt_1000 = round(mean(mean_s_rt_1000), 3),
              mean_tick_1 = round(mean(mean_tick_1), 3),
              mean_tick_5 = round(mean(mean_tick_5), 3),
              mean_tick_10 = round(mean(mean_tick_10), 3),
              mean_tick_20 = round(mean(mean_tick_20), 3))
  rownames(sum) <- ifelse(first == last, paste0("Stock",idx[first]), 
                          paste0("Stocks",idx[first],"-",idx[last]))
  return(sum)
}

tabla_i <- data.frame()
stops <- c(3, 6, 9)
for (i in 1:length(LOB)){ # Bucle para generar tabla_i
  tabla_i <- rbind(tabla_i, summarizer(i,i))
  if(i %in% stops){ 
    # Tras los activos #3, #6, #9 (últimos de su grado de capitalización)
    # el bucle genera, además, los estadísticos de los activos agregados
    # (por grado de capitalización)
    tabla_i <- rbind(tabla_i, summarizer(i-2,i))
  }
  rm(i)
}

# Por cuestiones de compilación la tabla (esta y las siguientes) se ha generado de forma separada,de acuerdo con el código que sigue. Este es un código que NO puede compilarse en Rmd.
library(officer)
library(flextable)
library(magrittr)
# Para exportar tablas a formato Word
ft <- flextable(data = tabla_i %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla I", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# 2
# Realizar un contraste de significatividad de las diferencias observadas entre
# las diferentes submuestras: peq-med, peq-gran, med-gran.
# Para ello será necesario:
# Las series diarias de la media en sección cruzada (para cada submuestra) de cada medida
# test estadístico de igualdad de medianas (rank-sum test de Wilcoxon)

# Se almacenan en lista los estadísticos de cada submuestra.
tabla_i_cs <- vector(mode = "list", length = length(LOB)/length(stops))

for (i in 1:length(idx2)){
  agg <- data.frame()
  agg2 <- data.frame()
  for (j in 1:length(idx2[[i]])){
    # El bucle agrega según la numeración de subconjuntos especificada en idx2
    agg <- rbind(agg, tabla_i_sum[[which(idx == idx2[[i]][j])]])
    agg2 <- rbind(agg2, tabla_i_sum2[[which(idx == idx2[[i]][j])]])
  }
  agg <- agg %>%
    group_by(date) %>%
    # una vez tiene agregados los subconjuntos agrupa por fecha (día) y calcula
    # las medias.
    summarise(mean_s_rt_100 = mean(mean_s_rt_100),
              mean_s_rt_500 = mean(mean_s_rt_500),
              mean_s_rt_1000 = mean(mean_s_rt_1000))
  agg2 <- agg2 %>% # idem
    group_by(date) %>%
    summarise(mean_tick_1 = mean(mean_tick_1),
              mean_tick_5 = mean(mean_tick_5),
              mean_tick_10 = mean(mean_tick_10),
              mean_tick_20 = mean(mean_tick_20))
  # se juntan las tablas de horquillas y profundidad:
  tabla_i_cs[[i]] <- merge(agg, agg2)
  rm(i, j, agg, agg2)
}

# Test de Wilcoxon
# Si RH0 las medias son diferentes.
var <- colnames(tabla_i_cs[[1]])[2:8]
# 3 combinaciones posibles de los 3 subconjuntos:
pairs <- list(c(1, 2), c(1, 3), c(2, 3))
cap <- c("Gran", "Med", "Peq")

# inicializa data.frame con una columna de 7 NAs (7 es el número de variables)
# Solo es necesaria para darle dimensión al data.frame.
Wilcox_p <- data.frame(rep(NA, 7)) 
for (i in 1:length(pairs)){
  Wilcox_sub <- data.frame()
  for (j in 1:length(var)){
    # Para cada variable de cada combinación de los 3 subconjuntos calcula el
    # p-valor según la prueba de Wilcoxon.
    x <- tabla_i_cs[[pairs[[i]][1]]][, var[j]]
    y <- tabla_i_cs[[pairs[[i]][2]]][, var[j]]
    pvalue <- wilcox.test(x, y, alternative = "two.sided", paired = F)$p.value
    pvalue <- data.frame(pvalue) # lo mete en data.frame.
    Wilcox_sub <- rbind(Wilcox_sub, pvalue) # agrega los valores
  }
  # Bautiza la combinación de subconjuntos:
  colnames(Wilcox_sub) <- paste0("Cap-", cap[pairs[[i]][1]], "-", cap[pairs[[i]][2]])
  Wilcox_p <- cbind(Wilcox_p, Wilcox_sub) # guarda los resultados.
  rm(i, j, x, y, Wilcox_sub, pvalue)
}
Wilcox_p <- Wilcox_p[,2:4] # descarta la columna accesoria de NAs 
# Se nombran las filas para conservar la correspondencia con los p-valores.
rownames(Wilcox_p) <- var 

# Para exportar tablas a formato Word
ft <- flextable(data = Wilcox_p %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla P - Grado de capitalización", 
                  style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# 3
# Analizar las diferencias entre la semana de crisis y las otras dos semanas
# Comentar e interpretar las diferencias.
# Deberá hacerse a partir de tabla_i_sum y tabla_i_sum2 (con las medias diarias)
# Hay que diferenciar entre los datos de la semana de la crisis y las otras dos

tabla_i_cs_week <- matrix(vector(mode = "list", length = 1), nrow = 3, ncol = 3)
# Esta vez se acumulan las tablas en una "matriz de listas de tablas"
# En las filas coloca los activos por capitalización
# En las columnas coloca los activos por semana
for (i in 1:length(tabla_i_cs)){
  # Se genera una variable para conocer la semana del registro.
  # Es conveniente usar isoweek para semanas lunes-domingo.
  # Se agrega el año al final; 38-2008 es la semana 38 de 2008.
  tabla_i_cs[[i]]$week <- paste0(isoweek(tabla_i_cs[[i]]$date), "-", year(tabla_i_cs[[i]]$date))
  weeks <- unique(tabla_i_cs[[i]]$week)
  for (w in 1:length(weeks)){
    # Básicamente convierte los 3 data.frames de las medias de los subconjuntos
    # en 9, dividiéndolos según semanas.
    group_week <- tabla_i_cs[[i]] %>%
      filter(week == weeks[w])
    tabla_i_cs_week[i,w][[1]] <- group_week
  }
  rm(i, w, group_week)
}

# Como acaba de hacerse la comparación entre grado de capitalización para 
# evaluar el efecto de la crisis se obvia esta dimensión. En cada una de las
# tablas de la lista se disponen los pares de semanas en columnas y las variables 
# en las filas. Los tres elementos de la lista se corresponden con los diferentes
# grados de capitalización.

tabla_i_week_wilcox <- vector(mode = "list", length = length(cap))

for (c in 1:length(cap)){ # Por cada grado de capitalización...
  subtabla_i_week_wilcox <- data.frame(rep(NA, 7))
  for (i in 1:length(pairs)){ # Por cada una de las parejas de semanas...
    Wilcox_sub <- data.frame()
    for (j in 1:length(var)){ # Por cada variable...
      # Análogo al bucle ante-anterior
      x <- tabla_i_cs_week[c, pairs[[i]][1]][[1]][, var[j]]
      y <- tabla_i_cs_week[c, pairs[[i]][2]][[1]][, var[j]]
      pvalue <- wilcox.test(x, y, alternative = "two.sided", paired = F)$p.value
      pvalue <- data.frame(pvalue)
      Wilcox_sub <- rbind(Wilcox_sub, pvalue)
    }
    colnames(Wilcox_sub) <- paste0(weeks[pairs[[i]][1]], "_vs_", weeks[pairs[[i]][2]])
    subtabla_i_week_wilcox <- cbind(subtabla_i_week_wilcox, Wilcox_sub)
  }
  subtabla_i_week_wilcox <- subtabla_i_week_wilcox[,2:4]
  rownames(subtabla_i_week_wilcox) <- var
  tabla_i_week_wilcox[[c]] <- subtabla_i_week_wilcox
  rm(c, i, j, x, y, pvalue, subtabla_i_week_wilcox, Wilcox_sub)
}

tabla_i_week_wilcox
# Para los activos de todo tipo de capitalización las semanas resultan muy
# diferentes. La semana de la crisis es muy diferente a las demás excepto
# para la profundidad de 20 ticks de los activos de capitalización
# pequeña y para la horquilla de 500 round-trips en los activos de gran
# capitalización.

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_i_week_wilcox[[1]] %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla Q.1 - Act. de Gran Capitalización", 
                  style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_i_week_wilcox[[2]] %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla Q.2 - Act. de Capitalización media", 
                  style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_i_week_wilcox[[3]] %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla Q.3 - Act. de Capitalización pequeña", 
                  style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

## 3 ## --------------------------------------------------------------------
# 1
# Dividir la sesión en intervalos regulares de 15 minutos.
# Para cada activo calcular e intervalo de 15 minutos calcular
# la horquilla relativa media para un round-trip de 100 y 1000 acciones 
# y la profundidad media en el lado del ask a 5 y 20 ticks del punto medio.
# Tras eso obtener la media diaria por activo e intervalo de tiempo.
# Representar gráficamente las medias por intervalo y activo.
# Discutir los patrones y las diferencias entre grupos de acciones por capitalización.
# 2
# Analizar las diferencias entre la semana de la crisis y las otras dos.

tabla_i_sumquart <- vector(mode = "list", length = length(tabla_i_ext))

for (i in 1:length(tabla_i_ext)){
  rt_15 <- tabla_i_ext[[i]] %>%
    mutate(ceil_date_15 = ceiling_date(ymd_hms(rownames(tabla_i_ext[[i]])), 
                                       unit = "15 minutes")) %>%
    group_by(ceil_date_15) %>%
    summarise(mean_rt_100 = mean(rt_100, na.rm = T),
              mean_rt_1000 = mean(rt_1000, na.rm = T))
  tick_15 <- tabla_i_ext2[[i]] %>%
    mutate(ceil_date_15 = ceiling_date(ymd_hms(rownames(tabla_i_ext2[[i]])), 
                                       unit = "15 minutes")) %>%
    group_by(ceil_date_15) %>%
    summarise(mean_tick_5 = mean(tick_5),
              mean_tick_20 = mean(tick_20))
  tabla_i_sumquart[[i]] <- merge(rt_15, tick_15)
  rm(i, rt_15, tick_15)
}

tabla_i_quartday <- vector(mode = "list", length = length(tabla_i_sumquart))

for (i in 1:length(tabla_i_sumquart)){
  resume <- tabla_i_sumquart[[i]] %>%
    mutate(ceil_quarter = hm(paste0(hour(ceil_date_15), ":", minute(ceil_date_15)))) %>%
    group_by(ceil_quarter) %>%
    summarise(horq_med100 = mean(mean_rt_100),
              horq_med1000 = mean(mean_rt_1000),
              profund_med5 = mean(mean_tick_5),
              profund_med20 = mean(mean_tick_20))
  tabla_i_quartday[[i]] <- resume
  rm(i, resume)
}

# Represetación gráfica
require(cowplot)
fill_color <- c("cornflowerblue", "dodgerblue4", "green3", "forestgreen")

for (i in 1:length(tabla_i_quartday)){
  tabla_i_quartday[[i]]$ceil_quarter <- as.numeric(tabla_i_quartday[[i]]$ceil_quarter, "hours")
  rm(i)
}

sub_graf <- function(data, trans){
  ggplot(data = data) +
    geom_line(aes(x = ceil_quarter, y = horq_med100, color = "horq_med100"), 
              alpha = .5, linetype = 1, size = .75) +
    geom_line(aes(x = ceil_quarter, y = horq_med1000, color = "horq_med1000"), 
              alpha = .5, linetype = 1, size = .75) +
    geom_line(aes(x = ceil_quarter, y = profund_med5/trans, color = "profund_med5"), 
              alpha = .5, linetype = 2, size = .75) +
    geom_line(aes(x = ceil_quarter, y = profund_med20/trans, color = "profund_med20"), 
              alpha = .5, linetype = 2, size = .75) +
    scale_colour_manual("", breaks = c("horq_med100", "horq_med1000", "profund_med5", "profund_med20"),
                        values = c(fill_color[1], fill_color[2], fill_color[3], fill_color[4])) +
    theme(plot.title = element_text(hjust = .5)) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.*trans)) +
    ylab("Horquilla") +
    xlab("Hora")
}

legend <- get_legend(sub_graf(tabla_i_quartday[[1]], 1) + theme(legend.position = "bottom"))

plot_grid(plot_grid(sub_graf(tabla_i_quartday[[1]], 2) + xlab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[1])), 
                    sub_graf(tabla_i_quartday[[2]], 2) + xlab("") + ylab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[2])), 
                    sub_graf(tabla_i_quartday[[3]], 1) + xlab("") + ylab("") + 
                      scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, name = "Profundidad")) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[3])),
                    sub_graf(tabla_i_quartday[[4]], .25) + xlab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[4])), 
                    sub_graf(tabla_i_quartday[[6]], .25) + xlab("") + ylab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[5])), 
                    sub_graf(tabla_i_quartday[[6]], .25) + xlab("") + ylab("") + 
                      scale_y_continuous(sec.axis = sec_axis(trans = ~.*.25, name = "Profundidad")) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[6])),
                    sub_graf(tabla_i_quartday[[7]], .05) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[7])), 
                    sub_graf(tabla_i_quartday[[8]], .05) + ylab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[8])), 
                    sub_graf(tabla_i_quartday[[9]], .05) + ylab("") + 
                      scale_y_continuous(sec.axis = sec_axis(trans = ~.*.05, name = "Profundidad")) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Stock",idx[9])), 
                    nrow = 3), legend, nrow = 2, rel_heights = c(0.95, 0.05))

# 2 Diferencias entre las semanas de la crisis (por hora)
# Convendria agrupar por capitalización para no tener que comparar los 9 activos:
# 3 grados de capitalización (filas) x 3 semanas (columnas). Esto está por hacer:

for (i in 1:length(tabla_i_sumquart)){ # Genera la var week para cada activo
  tabla_i_sumquart[[i]] <- tabla_i_sumquart[[i]] %>%
    mutate(week = paste0(isoweek(tabla_i_sumquart[[i]]$ceil_date_15), "-", 
                         year(tabla_i_sumquart[[i]]$ceil_date_15)),
           ceil_quarter = hm(paste0(hour(ceil_date_15), ":", minute(ceil_date_15))),
           ceil_quarter = as.numeric(ceil_quarter, "hours"))
  rm(i)
}

tabla_i_weekhour <- matrix(vector(mode = "list", length = 1), nrow = 3, ncol = 3)
# grados de capitalización en las filas y semanas en las columnas

for (i in 1:length(idx2)){
  for (w in 1:length(weeks)){
    agg <- data.frame()
    for (j in 1:length(idx2[[i]])){
      agg_med <- tabla_i_sumquart[[which(idx == idx2[[i]][j])]] %>%
        filter(week == weeks[w])
      agg <- rbind(agg, agg_med)
    }
    agg <- agg %>%
      group_by(ceil_quarter) %>%
      summarise(horq_med100 = mean(mean_rt_100),
                horq_med1000 = mean(mean_rt_1000),
                profund_med5 = mean(mean_tick_5),
                profund_med20 = mean(mean_tick_20))
    tabla_i_weekhour[i,w][[1]] <- agg
  }
  rm(i, j, w, agg_med, agg)
}


# Representación gráfica (usando la función anterior)
plot_grid(plot_grid(sub_graf(tabla_i_weekhour[1, 1][[1]], 2) + xlab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[1], ", ", weeks[1])), 
                    sub_graf(tabla_i_weekhour[1, 2][[1]], 1) + xlab("") + 
                      ylab("") + theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[1], ", ", weeks[2])), 
                    sub_graf(tabla_i_weekhour[1, 3][[1]], 2) + xlab("") + 
                      ylab("") + scale_y_continuous(sec.axis = sec_axis(trans = ~.*2, name = "Profundidad")) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[1], ", ", weeks[3])),
                    sub_graf(tabla_i_weekhour[2, 1][[1]], (1/3)) + xlab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[2], ", ", weeks[1])), 
                    sub_graf(tabla_i_weekhour[2, 2][[1]], (1/6)) + xlab("") + 
                      ylab("") + theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[2], ", ", weeks[2])), 
                    sub_graf(tabla_i_weekhour[2, 3][[1]], .75) + xlab("") + 
                      ylab("") + scale_y_continuous(sec.axis = sec_axis(trans = ~.*.75, name = "Profundidad")) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[2], ", ", weeks[3])),
                    sub_graf(tabla_i_weekhour[3, 1][[1]], .05) + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[3], ", ", weeks[1])), 
                    sub_graf(tabla_i_weekhour[3, 2][[1]], .025) + ylab("") + 
                      theme(legend.position = "none") + 
                      ggtitle(paste0("Cap. ", cap[3], ", ", weeks[2])), 
                    sub_graf(tabla_i_weekhour[3, 3][[1]], .05) + ylab("") + 
                      scale_y_continuous(sec.axis = sec_axis(trans = ~.*.05, name = "Profundidad")) + 
                      ggtitle(paste0("Cap. ", cap[3], ", ", weeks[3])) + theme(legend.position = "none"), 
                    nrow = 3), legend, nrow = 2, rel_heights = c(0.95, 0.05))

## 4 ## --------------------------------------------------------------------
# Evalúa la importancia relativa del volumen oculto en la provisión de liquidez
# de esta muestra de activos del NASDAQ. Hay que responder a las siguientes
# preguntas:
# 1 ¿Cuánto de la profundidad total del LOB está oculta?
# 2 ¿Hay más volumen oculto durante la semana de crisis?
# 3 ¿Hay diferencia en el uso de volumen oculto en función de la capitalización
# bursátil del activo?
# 4 ¿Tienden a ocultarse más o menos cerca del punto medio?
# Hay que resumir los estadísticos resumen en la tabla II.

# 1 Profundidad TOTAL (en general)
# Calcular la profundidad total en el ask y el bid con el volumen mostrado y con
# el oculto. Sacar el porcentaje al minuto, luego la media de todos los minutos.

# 2
# Para responder a 2 basta con sumar el volumen oculto de cada semana
# (en cualquier nivel del LOB) y también el volumen total y comparar.
# En las filas tendremos los activos y en las columnas las semanas.

# 3 Puede responderse sacando la media de las tres semanas en #2.

# 4
# Sacar las medidas del punto medio para cada LOB (minuto);
# Sacar los precios medios ponderados en el ask y el bid del volumen oculto.
# Sacar las medidas relativas. Así tendré datos por minuto.
# Las medias serán esas medidas.


# 1 Profundidad TOTAL (en general)
# Calcular la profundidad total en el ask y el bid con el volumen mostrado y con
# el oculto. Sacar el porcentaje al minuto, luego la media de todos los minutos.

# Subtabla Profunidad, tabla_ii_sub1 (9 subtablas)
tabla_ii_sub1 <- vector(mode = "list", length = length(LOB))

for (d in 1:length(LOB)){
  minute <- unique(LOB[[d]]$datime) # se extrae un vector con los minutos
  subtabla_ii_sub1 <- data.frame() # se inicializa data.frame depositario
  for (t in 1:length(minute)){
    total_deep <- function(part){
      # part es el parámetro para ask y bid; para ask es -1
      LOB_partmin <- LOB[[d]] %>%
        filter(datime == minute[t] & sign*part > 0)
      deep_v <- sum(LOB_partmin$vol*LOB_partmin$quote)
      deep_hv <- sum(LOB_partmin$hvol*LOB_partmin$quote)
      return(deep_hv/deep_v)
    }
    # En cell se almacenan los resultados de la función total_seep de cálculo
    # de la profundidad total.
    cell2 <- data.frame(deep_a = total_deep(-1), deep_b = total_deep(1))
    # Acumula los resultados por minuto:
    subtabla_ii_sub1 <- rbind(subtabla_ii_sub1, cell2)
  }
  # Se almacenan las tablas resultados en lista, convenientemente renombradas
  # las filas.
  tabla_ii_sub1[[d]] <- subtabla_ii_sub1
  rownames(tabla_ii_sub1[[d]]) <- minute
  cat(paste0("Stock",idx[d]," completado. ")) # Muestra el progreso en pantalla.
  rm(d, t, minute, cell2, subtabla_ii_sub1, total_deep)
}

# Resumen de las tablas de tabla_ii_sub1
tabla_ii_part1 <- data.frame()

for (d in 1:length(tabla_ii_sub1)){
  ask <- mean(tabla_ii_sub1[[d]]$deep_a)
  bid <- mean(tabla_ii_sub1[[d]]$deep_b)
  propf_oculta <- data.frame(propf_oculta = (ask+bid)/2)
  tabla_ii_part1 <- rbind(tabla_ii_part1, propf_oculta)
  rm(ask, bid, d, propf_oculta)
}
rownames(tabla_ii_part1) <- paste0("Stock", idx)

# 2
# Para responder basta con sumar el volumen oculto de cada semana
# (en cualquier nivel del LOB) y también el volumen total y comparar.
# En las filas tendremos los activos y en las columnas las semanas.

# Subtabla Profunidad, tabla_ii_sub1 (9 subtablas)
tabla_ii_sub2 <- vector(mode = "list", length = length(LOB))

for (d in 1:length(LOB)){
  minute <- unique(LOB[[d]]$datime) # se extrae un vector con los minutos
  subtabla_ii_sub2 <- data.frame() # se inicializa data.frame depositario
  for (t in 1:length(minute)){
    LOB_partmin <- LOB[[d]] %>%
      filter(datime == minute[t])
    occ_vol <- sum(LOB_partmin$hvol)/sum(LOB_partmin$vol)
    cell <- data.frame(Vol_ocult = occ_vol,
                       minute = minute[t])
    # Acumula los resultados por minuto:
    subtabla_ii_sub2 <- rbind(subtabla_ii_sub2, cell)
  }
  # Se almacenan las tablas resultados en lista, convenientemente renombradas
  # las filas.
  tabla_ii_sub2[[d]] <- subtabla_ii_sub2
  rownames(tabla_ii_sub2[[d]]) <- minute
  cat(paste0("Stock",idx[d]," completado. ")) # Muestra el progreso en pantalla.
  rm(d, t, minute, cell, occ_vol, subtabla_ii_sub2, LOB_partmin)
}

# Resumen de las tablas de tabla_ii_sub2
tabla_ii_part2 <- data.frame()

for (d in 1:length(tabla_ii_sub2)){
  tabla_ii_sub2[[d]]$week <- paste0(isoweek(tabla_ii_sub2[[d]]$minute), "-", 
                                    year(tabla_ii_sub2[[d]]$minute))
  weeks <- unique(tabla_ii_sub2[[d]]$week)
  subtabla_ii_sub3 <- data.frame(aux = rep(NA, 1))
  for (w in weeks){
    tabla_ii_sub_w <- tabla_ii_sub2[[d]] %>%
      filter(week == w)
    occ_vol <- mean(tabla_ii_sub_w$Vol_ocult)
    Vol_ocult <- data.frame(occ_vol)
    subtabla_ii_sub3 <- cbind(subtabla_ii_sub3, Vol_ocult)
  }
  tabla_ii_part2 <- rbind(tabla_ii_part2, subtabla_ii_sub3[,2:4])
  rm(d, occ_vol, Vol_ocult, subtabla_ii_sub3, tabla_ii_sub_w, w)
}
rownames(tabla_ii_part2) <- paste0("Stock", idx)
colnames(tabla_ii_part2) <- paste0("Semana_", weeks)

# ¿Hay más volumen oculto durante la semana de la crisis?
mean(tabla_ii_part2$`Semana_15-2008`)
mean(tabla_ii_part2$`Semana_38-2008`)
mean(tabla_ii_part2$`Semana_8-2010`)
# Parece que no cuando se compara con la primera semana pero sí es 
# significativamente mayor que la semana 8 de 2010.

# Atendiendo a los niveles de capitalización parece bastante claro que
# a mayor grado de capitalización, menos volumen oculto.

# 4
# Sacar las medidas del punto medio para cada LOB (minuto);
# Sacar los precios medios ponderados en el ask y el bid del volumen oculto.
# Sacar las medidas relativas. Así tendré datos por activo y minuto.
# Las medias serán esas medidas.
# Necesito medidas por semana y por grado de capitalización
# Medirlo en ticks con respecto al precio eficiente.

# Test
tabla_ii_sub3 <- vector(mode = "list", length = length(LOB))

for (d in 1:length(LOB)){
  minute <- unique(LOB[[d]]$datime) # se extrae un vector con los minutos
  subtabla_ii_sub3 <- data.frame() # se inicializan los data.frames depositarios
  for (t in 1:length(minute)){
    LOB_partmin <- LOB[[d]] %>%
        # filtra por minuto y signo de la transacción
      filter(datime == minute[t])
    # Cálculo de los puntos medios:
    q <- (LOB_partmin$quote[LOB_partmin$sign == 1] + LOB_partmin$quote[LOB_partmin$sign == -1])/2
    ask_hvol <- LOB_partmin$hvol[LOB_partmin$sign < 0] # se vectorizan los volúmenes...
    bid_hvol <- LOB_partmin$hvol[LOB_partmin$sign > 0]
    ask_quote <- LOB_partmin$quote[LOB_partmin$sign < 0] # ...y los precios
    bid_quote <- LOB_partmin$quote[LOB_partmin$sign > 0]
    ask_meanq <- ifelse(sum(ask_hvol) == 0, 0, sum(ask_hvol*ask_quote)/sum(ask_hvol))
    bid_meanq <- ifelse(sum(bid_hvol) == 0, 0, sum(bid_hvol*bid_quote)/sum(bid_hvol))
    ask_reldist <- ifelse(ask_meanq == 0, 0, abs(q - ask_meanq)/ask_meanq)
    bid_reldist <- ifelse(bid_meanq == 0, 0, abs(q - bid_meanq)/bid_meanq)
    pmean_reldist <- ifelse(sum(ask_hvol) == 0 & sum(bid_hvol) == 0, NA, 
                              ((ask_reldist*sum(ask_hvol))+(bid_reldist*sum(bid_hvol)))/(sum(ask_hvol)+sum(bid_hvol)))
    # en cell4 se almacenan los resultados de aplicar la función ab2:
    cell <- data.frame(reldist = pmean_reldist)
    # Acumula los resultados por minuto:
    subtabla_ii_sub3 <- rbind(subtabla_ii_sub3, cell)
  }
  # Se almacenan las tablas resultados en lista, convenientemente renombradas
  # las filas.
  cat(paste0("Stock", idx[d]," completado. ")) # Muestra el progreso en pantalla.
  tabla_ii_sub3[[d]] <- subtabla_ii_sub3
  rownames(tabla_ii_sub3[[d]]) <- minute
  rm(d, t, q, ask_hvol, bid_hvol, ask_quote, bid_quote, ask_meanq, bid_meanq, 
     ask_reldist, bid_reldist, pmean_reldist, minute, cell, subtabla_ii_sub3, 
     LOB_partmin)
}

# Separemos por semana y grado de capitalización

tabla_ii_part3 <- data.frame()

for (d in 1:length(tabla_ii_sub3)){
  tabla_ii_sub3[[d]]$week <- paste0(isoweek(ymd_hms(rownames(tabla_ii_sub3[[d]]))), "-", 
                                    year(ymd_hms(rownames(tabla_ii_sub3[[d]]))))
  row <- data.frame(aux = rep(NA, 1))
  for (w in 1:length(weeks)){
    tabla_week <- tabla_ii_sub3[[d]] %>%
      filter(week == weeks[w])
    mean_reldist <- mean(tabla_week$reldist, na.rm = T)
    mean_reldist <- data.frame(mean_reldist)
    row <- cbind(row, mean_reldist)
  }
  row <- row[,2:4]
  colnames(row) <- paste0("Dist_q_", weeks)
  tabla_ii_part3 <- rbind(tabla_ii_part3, row)
  rm(d, row, w, tabla_week, mean_reldist)
}
rownames(tabla_ii_part3) <- paste0("Stock",idx)

# Agregación de resultados: Tabla II
tabla_ii <- cbind(tabla_ii_part1, tabla_ii_part2)
tabla_ii <- cbind(tabla_ii, tabla_ii_part3)

# Vamos a sacar las medias por grado de capitalización
columns_t2 <- colnames(tabla_ii)

tabla_ii_transformed <- data.frame()
for (i in 1:length(idx2)){
  tabla_ii_transfer <- tabla_ii[which(idx == idx2[[i]][1]):which(idx == idx2[[i]][3]),]
  mean_third <- data.frame(lapply(tabla_ii[which(idx == idx2[[i]][1]):which(idx == idx2[[i]][3]),][, columns_t2], mean))
  colnames(mean_third) <- columns_t2
  rownames(mean_third) <- paste0("Stock", idx[which(idx == idx2[[i]][1])], "-", idx[which(idx == idx2[[i]][3])])
  tabla_ii_transfer <- rbind(tabla_ii_transfer, mean_third)
  tabla_ii_transformed <- rbind(tabla_ii_transformed, tabla_ii_transfer)
}

require(scales)
tabla_ii_transformed[, columns_t2] <- lapply(tabla_ii_transformed[, columns_t2], percent_format(accuracy = .01))

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_ii_transformed %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla II", 
                  style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento
