# Se escogen 3 acciones de cada grupo (al azar)
set.seed(603)
idx <- as.character(sample(1:5, size = 3, replace = F))
idx <- c(paste0("", idx), paste0("10", idx), paste0("20", idx))

Stock <- vector(mode = "list", length = length(idx))
for (i in idx){ # Bucle de lectura de los archivos; se almacenan en lista
  Stock[[which(idx==i)]] <- read.table(paste0("Stock", i, ".txt"), header = T)
  rm(i)
}

## a ## --------------------------------------------------------------------
require(tidyverse)

for (d in 1:length(Stock)){
  Stock[[d]] <- Stock[[d]]  %>%
    arrange(day, time) %>% # se asegura que las órdenes están correctamente ordenadas
    mutate(Q = round((ask+bid)/2, 4)) # se obtiene el valor como la media de ask y bid
  
  day <- Stock[[d]]$day # se vectoriza day
  price <- Stock[[d]]$price # se vectoriza price
  
  # Tick Rule
  TR = c(); TR[1] = NA # se inicializa el vector con valor NA
  for (i in 2:length(price)){ # implementación del algoritmo
    TR[i] <- ifelse(day[i] == day[i-1] & price[i] > price[i-1], 1,
                    ifelse(day[i] == day[i-1] & price[i] < price[i-1], -1,
                           ifelse(day[i] == day[i-1] & price[i] == price[i-1], TR[i-1], NA)))
  }
  Stock[[d]]$TR <- TR; rm(TR) # se agrega el vector como columna al dataset
  
  # Quote Rule
  Q <- Stock[[d]]$Q # se vectoriza el valor
  QR = c() # se inicializa un vector vacío
  for (i in 1:length(price)){ # se implementa el algoritmo
    QR[i] <- ifelse(price[i] > Q[i], 1,
                    ifelse(price[i] < Q[i], -1, NA))
  }
  Stock[[d]]$QR <- QR; rm(QR) # se agrega el vector como columna al dataset
  
  # Lee & Ready
  LR = c(); LR[1] = ifelse(price[1] > Q[1], 1, # se inicializa el vector
                           ifelse(price[1] < Q[1], -1, NA))
  for (i in 2:length(price)){
    LR[i] <- ifelse(price[i] > Q[i], 1,
                    ifelse(price[i] < Q[i], -1, 
                           ifelse(price[i] == Q[i] & day[i] == day[i-1], LR[i-1], NA)))
    rm(i)
  }
  Stock[[d]]$LR <- LR; rm(LR, day, price, Q, d)
  # se agrega el vector como columna al dataset
}

## b ## --------------------------------------------------------------------
library(scales)

# se inicializa la lista de almacenamiento:
tabla_i_list <- vector(mode = "list", length = length(Stock))
for (s in 1:length(Stock)){
  subtabla_i <- data.frame() # se inicializa subtabla
  for (d in unique(Stock[[s]]$day)){
    stock_parse <- Stock[[s]] %>% filter(day == d) # filtro de día
    acc_f = function(R){ # se define función interna de precisión (R es el parámetro para el algoritmo)
      mean(stock_parse[, R]==stock_parse[, "buysell"], na.rm = T)
    } # Aplicación de las funciones
    acc_TR = acc_f("TR"); acc_QR = acc_f("QR"); acc_LR = acc_f("LR")
    acc_df = data.frame(acc_TR, acc_QR, acc_LR) # se introducen en data.frame
    subtabla_i <- rbind(subtabla_i, acc_df) # se agrega lo anterior a subtabla_i
    rm(d, acc_TR, acc_QR, acc_LR, acc_df, acc_f, stock_parse)
  }
  tabla_i_list[[s]] <- subtabla_i # se agrega la subtabla_i a tabla_i_list
  var <- colnames(tabla_i_list[[s]]) # se extraen nombres de columnas
  rownames(tabla_i_list[[s]]) <- paste0("day ", unique(Stock[[s]]$day)) # se renombran las filas
  colnames(tabla_i_list[[s]]) <- paste0("Stock", idx[s], ", ", var) # se añade nombre del stock a las columnas
  rm(subtabla_i, s, var)
}

tabla_i <- tabla_i_list[[1]] # Se inicializa tabla_i con la primera subtabla

var <- colnames(tabla_i) # Se disponen los datos como porcentajes con 2 decimales
tabla_i[, var] <- lapply(tabla_i[, var], percent_format(accuracy = .01))
rm(var); tabla_i

# Gráfico complementario a Tabla I
for (i in 1:length(tabla_i_list)){
  tabla_i_list[[i]]$days <- as.numeric(gsub("day ", "", row.names(tabla_i_list[[i]])))
  colnames(tabla_i_list[[i]]) <- c("TR", "QR", "LR", "day"); rm(i)
}

library(cowplot)
fill_color <- c("forestgreen", "dodgerblue4", "firebrick")
sub_graf <- function(ord){
  ggplot() +
    geom_line(data = tabla_i_list[[ord]], aes(x = day, y = TR*100, color = "TR"), alpha = .5, linetype = 1, size = .75) +
    geom_line(data = tabla_i_list[[ord]], aes(x = day, y = QR*100, color = "QR"), alpha = .5, linetype = 1, size = .75) +
    geom_line(data = tabla_i_list[[ord]], aes(x = day, y = LR*100, color = "LR"), alpha = .5, linetype = 1, size = .75) +
    scale_colour_manual("", 
                        breaks = c("TR", "QR", "LR"),
                        values = c(fill_color[1], fill_color[2], fill_color[3])) +
    theme(plot.title = element_text(hjust = .5)) +
    ylab("Precisión (%)") +
    ylim(50, 100) +
    xlab("Día") +
    ggtitle(paste0("Stock",idx[ord]))
}

legend <- get_legend(sub_graf(1) + theme(legend.position = "bottom"))

plot_grid(plot_grid(sub_graf(1) + xlab("") + theme(legend.position = "none"), 
                    sub_graf(2) + xlab("") + ylab("") + theme(legend.position = "none"), 
                    sub_graf(3) + xlab("") + ylab("") + theme(legend.position = "none"),
                    sub_graf(4) + xlab("") + theme(legend.position = "none"), 
                    sub_graf(5) + xlab("") + ylab("") + theme(legend.position = "none"), 
                    sub_graf(6) + xlab("") + ylab("") + theme(legend.position = "none"),
                    sub_graf(7) + theme(legend.position = "none"), 
                    sub_graf(8) + ylab("") + theme(legend.position = "none"), 
                    sub_graf(9) + ylab("") + theme(legend.position = "none"), 
                    nrow = 3), legend, nrow = 2, rel_heights = c(0.95, 0.05))

# Para exportar grandes tablas a formato Word
library(officer)
library(flextable)
library(magrittr)

ft <- flextable(data = tabla_i %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla I", 
                  style = "Table Caption")
# Create a temp file
tmp <- tempfile(fileext = ".docx")
# Create a docx file
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp)

# 2a Tabla: precisión diaria MEDIA y DESVIACIÓN TÍPICA diaria para cada
# algoritmo, POR ACTIVO Y ALGORITMO.

tabla_ii <- data.frame() # Se inicializa tabla_ii como tabla vacía
for (s in 1:length(tabla_i_list)){
  var <- colnames(tabla_i_list[[s]]) # Se toman los nombres de las columnas de la subtabla
  mean_f <- function(pos){ # Se define una función general para la media
    mean(tabla_i_list[[s]][, var[pos]], na.rm = T)
  }
  sd_f <- function(pos){ # Se define una función general para la desviación típica
    sd(tabla_i_list[[s]][, var[pos]], na.rm = T)
  }
  mean_acc_TR <- mean_f(1); sd_acc_TR <- sd_f(1) # Aplicación de funciones a TR
  mean_acc_QR <- mean_f(2); sd_acc_QR <- sd_f(2) # Aplicación de funciones a QR
  mean_acc_LR <- mean_f(3); sd_acc_LR <- sd_f(3) # Aplicación de funciones a LR
  subtabla_ii <- data.frame(mean_acc_TR, sd_acc_TR, mean_acc_QR, 
                            sd_acc_QR, mean_acc_LR, sd_acc_LR)
  tabla_ii <- rbind(tabla_ii, subtabla_ii) # Introducción en data frame y agregación
  rm(var, subtabla_ii, s, mean_f, sd_f, mean_acc_TR, sd_acc_TR, mean_acc_QR, 
     sd_acc_QR, mean_acc_LR, sd_acc_LR)
}
rownames(tabla_ii) <- paste0("Stock", idx) # Se renombran las filas
var <- colnames(tabla_ii) # Se disponen los datos como porcentajes con 2 decimales
tabla_ii[, var] <- lapply(tabla_ii[, var], percent_format(accuracy = .01))
rm(var); tabla_ii

# Para exportar grandes tablas a formato Word
ft <- flextable(data = tabla_ii %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla II", 
                  style = "Table Caption")
# Create a temp file
tmp <- tempfile(fileext = ".docx")
# Create a docx file
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp)

# Representación gráfica de la Tabla II
# Gráfico de columnas con puntos y rango en desviaciones típicas

tabla_ii <- data.frame(lapply(tabla_ii, function(x) as.numeric(sub("%", "", x))))
rownames(tabla_ii) <- paste0("Stock", idx)
tabla_ii$stock <- rownames(tabla_ii)

sub_col <- function(pos){
  alg <- c("Tick Rule", "Quote Rule", "Lee & Ready")
  ggplot(data = tabla_ii) +
    geom_col(aes(x = stock, y = mean_acc_TR), fill = fill_color[pos], alpha = .5) +
    geom_errorbar(aes(x = stock, ymin = mean_acc_TR - sd_acc_TR, 
                      ymax = mean_acc_TR + sd_acc_TR), 
                  col = fill_color[pos], width = .9, size = .7) +
    theme(plot.title = element_text(hjust = .5)) +
    ylab("Precisión (%)") +
    coord_cartesian(ylim=c(60, 100)) +
    xlab("Stock") +
    ggtitle(paste0(alg[pos]))
}

plot_grid(sub_col(1) + xlab(""),
          sub_col(2) + xlab(""),
          sub_col(3), nrow = 3)

## c ## -------------------------------------------------------------------
require(lubridate)

alg <- c("TR", "QR", "LR")
for (d in 1:length(Stock)){
  for (a in alg){ # se reemplazan los NA por 0
    Stock[[d]][,a][is.na(Stock[[d]][,a])] <- 0
  }
}


tabla_iii_list <- vector(mode = "list", length = length(Stock))
for (d in 1:length(Stock)){
  this_1 <- Stock[[d]] %>%
    mutate(time_stamp = ymd(paste0("2011-06-", day)) + hms("09:30:00") + time-34200, 
           # 34200 son los segundos pasados la medianoche; 
           # time_stamp está redondeado a la baja en las -ésimas de segundo.
           ceil_t = ceiling_date(time_stamp, unit = "30 minutes"), # se redondea al alza en intervalos de 30 minutos.
           ceil_halfh = hm(paste0(hour(ceil_t),":",minute(ceil_t)))) %>% # se extren las horas y minutos
    select(-ceil_t)
  # Es posible que haya operaciones a las 9:30:00 y 0 -ésimas de s (pero no antes) 
  # así que, por practicidad, se asignan a las 10:00:00:
  this_1$ceil_halfh[this_1$ceil_halfh==hms("9H 30M 0S")] <- hms("10H 0M 0S")
  # Se construye un vector de texto con las franjas de tiempo:
  time <- unique(as.character(this_1$ceil_halfh))
  alg <- c("TR", "QR", "LR") # Se hace lo mismo con los algoritmos
  acc_at <- data.frame(aux = rep(0, length(time))) # se inicializa el data.frame con una columna auxiliar
  for (a in alg){
    acc_t = data.frame() # se inicializa un data frame
    group_cols <- c("ceil_halfh", "buysell", a) # criterio de agrupación dependiente de a
    for (t in time){ # bucle para la agregación de volúmenes según franja de tiempo
      this_2 <- this_1 %>%
        group_by_at(group_cols) %>%
        summarise(vol = sum(vol)) %>%
        filter(ceil_halfh == t)
      this_2 <- this_2[this_2[,a]!=0,]
      # Precisión de un algoritmo dado para una franja de tiempo dada
      acc = (min(sum(this_2$vol[this_2[, "buysell"]==1]), sum(this_2$vol[this_2[, a]==1])) +
               min(sum(this_2$vol[this_2[, "buysell"]==-1]), sum(this_2$vol[this_2[, a]==-1])))/
        (sum(this_2$vol[this_2[, "buysell"]==1])+sum(this_2$vol[this_2[, "buysell"]==-1]))
      acc = data.frame(acc) # conversión de data.frame
      acc_t = rbind(acc_t, acc) # agregación de resultados de t para un algoritmo dado
    }
    acc_at <- cbind(acc_at, acc_t) # agregación de algoritmos para un activo dado
  }
  acc_at <- acc_at[,2:4] # se desecha la columna auxiliar
  colnames(acc_at) <- paste0("Stock", idx[d], ", ", alg) # se renombran las columnas con el nombre del activo
  rownames(acc_at) <- time # se renombran las filas con las franjas de tiempo
  tabla_iii_list[[d]] <- acc_at # se agrega la subtabla a la lista de tablas
  rm(a, acc, acc_at, acc_t, alg, d, group_cols, t, this_1, this_2, time)
}

tabla_iii <- data.frame() # Se inicializa tabla_ii como tabla vacía
for (s in 1:length(tabla_iii_list)){
  var <- colnames(tabla_iii_list[[s]]) # Se toman los nombres de las columnas de la subtabla
  mean_f <- function(pos){ # Se define una función general para la media
    mean(tabla_iii_list[[s]][, var[pos]], na.rm = T)
  }
  mean_acc_TR <- mean_f(1) # Aplicación de funciones a TR
  mean_acc_QR <- mean_f(2) # Aplicación de funciones a QR
  mean_acc_LR <- mean_f(3) # Aplicación de funciones a LR
  subtabla_iii <- data.frame(mean_acc_TR, mean_acc_QR, mean_acc_LR)
  tabla_iii <- rbind(tabla_iii, subtabla_iii) # Introducción en data frame y agregación
  rm(var, subtabla_iii, s, mean_f, mean_acc_TR, mean_acc_QR, mean_acc_LR)
}
rownames(tabla_iii) <- paste0("Stock", idx) # Se renombran las filas
var <- colnames(tabla_iii) # Se disponen los datos como porcentajes con 2 decimales
tabla_iii[, var] <- lapply(tabla_iii[, var], percent_format(accuracy = .01))
rm(var); tabla_iii

# ----------------------------------------------------------------------------

# Para exportar grandes tablas a formato Word
ft <- flextable(data = tabla_iii %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla III", 
                  style = "Table Caption")
# Create a temp file
tmp <- tempfile(fileext = ".docx")
# Create a docx file
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp)

# Gráfico complementario a Tabla III
for (i in 1:length(tabla_iii_list)){
  tabla_iii_list[[i]]$hour <- as.numeric(hms(row.names(tabla_iii_list[[i]])), "hours")
  colnames(tabla_iii_list[[i]]) <- c("TR", "QR", "LR", "hour"); rm(i)
}

sub_graf2 <- function(ord){
  ggplot(data = tabla_iii_list[[ord]]) +
    geom_line(aes(x = hour, y = TR*100, color = "TR"), alpha = .5, linetype = 1, size = .75) +
    geom_line(aes(x = hour, y = QR*100, color = "QR"), alpha = .5, linetype = 1, size = .75) +
    geom_line(aes(x = hour, y = LR*100, color = "LR"), alpha = .5, linetype = 1, size = .75) +
    scale_colour_manual("", 
                        breaks = c("TR", "QR", "LR"),
                        values = c(fill_color[1], fill_color[2], fill_color[3])) +
    theme(plot.title = element_text(hjust = .5)) +
    ylab("Precisión (%)") +
    ylim(60, 100) +
    xlab("Hora") +
    ggtitle(paste0("Stock",idx[ord]))
}

plot_grid(plot_grid(sub_graf2(1) + xlab("") + theme(legend.position = "none"), 
                    sub_graf2(2) + xlab("") + ylab("") + theme(legend.position = "none"), 
                    sub_graf2(3) + xlab("") + ylab("") + theme(legend.position = "none"),
                    sub_graf2(4) + xlab("") + theme(legend.position = "none"), 
                    sub_graf2(5) + xlab("") + ylab("") + theme(legend.position = "none"), 
                    sub_graf2(6) + xlab("") + ylab("") + theme(legend.position = "none"),
                    sub_graf2(7) + theme(legend.position = "none"), 
                    sub_graf2(8) + ylab("") + theme(legend.position = "none"), 
                    sub_graf2(9) + ylab("") + theme(legend.position = "none"), 
                    nrow = 3), legend, nrow = 2, rel_heights = c(0.95, 0.05))
