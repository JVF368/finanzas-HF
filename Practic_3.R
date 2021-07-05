## 0 ## ------------------------------------------------------------------
# Se escogen 3 acciones de cada grupo (al azar)
set.seed(603)
idx <- as.character(sample(1:5, size = 3, replace = F))
idx <- c(paste0("", idx), paste0("10", idx), paste0("20", idx))
idx2 <- list(idx[1:3], idx[4:6], idx[7:9])

NBBO <- vector(mode = "list", length = length(idx))
Stock <- vector(mode = "list", length = length(idx))
for (i in idx){ # Bucle de lectura de los archivos; se almacenan en lista
  NBBO[[which(idx==i)]] <- read.table(paste0("Stock", i, "NBBO.txt"), header = T)
  Stock[[which(idx==i)]] <- read.table(paste0("../Finanzas HF - Practic_1/Stock", i, ".txt"), header = T)
  rm(i)
}

## 1 ## ------------------------------------------------------------------
require(tidyverse)

# Lo primero que pide es efectuar una regresión lineal de acuerdo con la
# fórmula que tenemos en la diapositiva 16 de la sesión VII.
# Para esta tabla I habrá que usar los datos de la primera práctica,
# porque lo que se pide es el cambio de precios, que es algo que solamente
# puede calcularse con los precios, que es información que tenemos en las tablas
# de Stock. En diferencias deberá ser la variable dependiente.

# TABLA I
pre_tabla_i <- vector(mode = "list", length = length(Stock))

for (d in 1:length(Stock)){
  pre_subtabla_i <- data.frame()
  day <- unique(Stock[[d]]$day)
  for (t in day){
    Stock_day <- Stock[[d]] %>%
      filter(day == t) %>%
      mutate(dprice = price - lag(price))
    lm <- lm(dprice ~ buysell+lag(buysell), data = Stock_day)
    row <- data.frame(beta0 = as.numeric(lm$coefficients[1]), 
                      beta1 = as.numeric(lm$coefficients[2]),
                      beta2 = as.numeric(lm$coefficients[3]),
                      sigmau2 = var(lm$residuals))
    pre_subtabla_i <- rbind(pre_subtabla_i, row)
  }
  pre_tabla_i[[d]] <- pre_subtabla_i
  coef <- colnames(pre_subtabla_i)[1:3]
  rownames(pre_tabla_i[[d]]) <- paste0("day.", day)
  colnames(pre_tabla_i[[d]]) <- c(paste0("St", idx[d], ".", coef), "sigmau2")
  rm(d, pre_subtabla_i, Stock_day, lm, row, t)
}

tabla_i <- pre_tabla_i[[1]][,2:3]
for (d in 2:length(pre_tabla_i)){
  tabla_i <- cbind(tabla_i, pre_tabla_i[[d]][,2:3]); rm(d)
}

format(tabla_i, scientific = T)

# Por cuestiones de compilación la tabla (esta y las siguientes) se ha generado 
# de forma separada, de acuerdo con el código que sigue. Este es un código que 
# NO puede compilarse en Rmd.
library(officer)
library(flextable)
library(magrittr)
# Para exportar tablas a formato Word
ft <- flextable(data = format(tabla_i, scientific = T) %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla I", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento


# Tabla II: 9 activos, 3 coeficientes
tabla_ii <- data.frame()
for (d in 1:length(pre_tabla_i)){
  row <- data.frame(mbeta1 = mean(pre_tabla_i[[d]][,2]),
                    mbeta2 = mean(pre_tabla_i[[d]][,3]))
  tabla_ii <- rbind(tabla_ii, row); rm(d, row)
}
rownames(tabla_ii) <- paste0("St.", idx)

# Para exportar tablas a formato Word
ft <- flextable(data = format(tabla_ii, scientific = T) %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla II", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Test de Wilcoxon
wilcoxont1 <- vector(mode = "list", length = length(idx2))
for (c in 1:length(idx2)){
  subwilcoxont1 <- data.frame()
  for (d in 1:length(idx2[[c]])){
    pre_tabla_i2 <- pre_tabla_i
    colnames(pre_tabla_i2[[which(idx==idx2[[c]][d])]])[2:3] <- coef[2:3]
    rownames(pre_tabla_i2[[which(idx==idx2[[c]][d])]]) <- 
      paste0("day.", day, ".St", idx[which(idx==idx2[[c]][d])])
    portion <- pre_tabla_i2[[which(idx==idx2[[c]][d])]][,2:3]
    subwilcoxont1 <- rbind(subwilcoxont1, portion)
  }
  wilcoxont1[[c]] <- subwilcoxont1
  rm(c, d, pre_tabla_i2, subwilcoxont1, portion)
}

# El test de Wilcoxon deberá hacerse por parejas:
# Agregar los datos por grado de capitalización (3 tablas x 3 coeficientes)
# Al final cabe tener una tabla de 3 x 3

pairs <- list(c(1, 2), c(1, 3), c(2, 3))
cap <- c("Gran", "Med", "Peq")
wilcoxtable <- data.frame(aux = rep(NA, length(coef[2:3])))

for (c in 1:length(pairs)){
  subwilcoxtable <- data.frame()
  for (r in 1:(ncol(wilcoxont1[[c]]))){
    x <- wilcoxont1[[pairs[[c]][1]]][, r]
    y <- wilcoxont1[[pairs[[c]][2]]][, r]
    pvalue <- wilcox.test(x, y, alternative = "two.sided", paired = F)$p.value
    pvalue <- data.frame(pick = pvalue)
    subwilcoxtable <- rbind(subwilcoxtable, pvalue)
  }
  wilcoxtable <- cbind(wilcoxtable, subwilcoxtable)
  rm(c, r, x, y, pvalue, subwilcoxtable)
}
wilcoxtable <- wilcoxtable[, 2:4]
rownames(wilcoxtable) <- coef[2:3]
colnames(wilcoxtable) <- c("Gran_vs_Med", "Gran_vs_Peq", "Med_vs_Peq")
wilcoxtable
# Los p-valores indican que los activos de capitalización media y pequeña son
# iguales entre sí, pero diferentes a los de gran capitalización.

# Para exportar tablas a formato Word
ft <- flextable(data = format(wilcoxtable, scientific = T) %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Contrastes de Wilcoxon", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Tabla III: Descomposición de la horquilla
# Para cada activo, ¿qué % de la horquilla se debe a selección adversa y qué %
# a costes operativos? Mostrar % medios estimados por activo
# Aportar un contraste de significatividad de las empresas
# medianas y pequeñas frente a las grandes.

# E(beta1) = (est.alfa + est.gamma)
# E(beta2) = -est.gamma
# var(et) = sigma^2
# est.S = 2*(est.gamma + est.alfa)
# est.alfa mide el contenido medio informativo de la negociación. Si fuera 0
# no habría asimetrías de información y el impacto sería 100% transitorio.
# Si est.gamma fuera 0 el impacto a c/p sería igual a l/p (no hay efecto 
# transitorio)

# Hacerlo todo a partir de los datos diarios...
# Para cada pre-tabla sacar est.gamma como el opuesto de beta2[columna 2];
# Luego sacar est.alfa como beta1[columna 1] - est.gamma.
# El porcentaje de selección adversa es est.alfa/(est.gamma + est.alfa)
# El porcentaje de costes operativos es est.gamma/(est.gamma + est.alfa)

for (d in 1:length(pre_tabla_i)){
  pre_tabla_i[[d]]$e.gamma <- pre_tabla_i[[d]][,3]*(-1)
  pre_tabla_i[[d]]$e.alpha <- pre_tabla_i[[d]][,2] - pre_tabla_i[[d]]$e.gamma
  pre_tabla_i[[d]] <- pre_tabla_i[[d]] %>%
    # Es posible que algún coeficiente aparezca con un signo que no permita un
    # cálculo adecuado de los % de s.a. y c.o. Por eso se censuran en 0 y 1.
    # Un % de s.a. del 124% y del -24% de c.o. se traduce en un 100% de s.a.
    mutate(sa = pmin(pmax(e.alpha/(e.gamma + e.alpha), 0), 1),
           oc = 1 - sa)
  rm(d)
}

# En resumen:
tabla_iii <- data.frame()
for (d in 1:length(pre_tabla_i)){
  row <- data.frame(msa = mean(pre_tabla_i[[d]]$sa),
                    moc = mean(pre_tabla_i[[d]]$oc))
  tabla_iii <- rbind(tabla_iii, row); rm(d, row)
}
rownames(tabla_iii) <- paste0("St.", idx)
tabla_iii

library(scales)
var <- colnames(tabla_iii) # Se disponen los datos como porcentajes con 2 decimales
tabla_iii[, var] <- lapply(tabla_iii[, var], percent_format(accuracy = .01))
rm(var)

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_iii %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla III", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Luego agregar los datos por capitalización y volver a contrastar por Wilcoxon.
# Repetir los dos últimos bucles.

# Test de Wilcoxon 2
wilcoxont2 <- vector(mode = "list", length = length(idx2))
for (c in 1:length(idx2)){
  subwilcoxont2 <- data.frame()
  for (d in 1:length(idx2[[c]])){
    pre_tabla_i2 <- pre_tabla_i
    colnames(pre_tabla_i2[[which(idx==idx2[[c]][d])]])[1:3] <- coef
    rownames(pre_tabla_i2[[which(idx==idx2[[c]][d])]]) <- 
      paste0("day.", day, ".St", idx[which(idx==idx2[[c]][d])])
    portion <- pre_tabla_i2[[which(idx==idx2[[c]][d])]]
    subwilcoxont2 <- rbind(subwilcoxont2, portion[,7:8])
  }
  wilcoxont2[[c]] <- subwilcoxont2
  rm(c, d, pre_tabla_i2, subwilcoxont2, portion)
}

wilcox.test(wilcoxont2[[1]]$sa, wilcoxont2[[2]]$sa, 
            alternative = "two.sided", paired = F)$p.value
wilcox.test(wilcoxont2[[1]]$sa, wilcoxont2[[3]]$sa, 
            alternative = "two.sided", paired = F)$p.value


# Tabla IV: Descomposición de la volatilidad del precio eficiente
# ¿Qué porcentaje de la volatilidad del precio eficiente se explica por costes 
# de s.a.? Mostrar los porcentajes medios estimados por activo.
# Volver a hacer Wilcoxon.

# Habrá que calcular el precio eficiente, q, como diferencia entre el mejor
# ask y bid. Luego sacar la variación del precio eficiente como diferencia
# entre el precio eficiente actual y su lag. Luego cabe hacer la regresión
# sobre el signo de la transacción actual y quedarnos con el coeficiente que,
# elevado al cuadrado, representa la información privada. También debemos
# quedarnos con la varianza de los residuos, que representará la información
# pública.

pre_tabla_iv <- vector(mode = "list", length = length(Stock))

for (d in 1:length(Stock)){
  pre_subtabla_iv <- data.frame()
  day <- unique(Stock[[d]]$day)
  for (t in day){
    Stock_day <- Stock[[d]] %>%
      filter(day == t) %>%
      mutate(q = (ask+bid)/2,
             dq = q - lag(q))
    lm <- lm(dq ~ buysell, data = Stock_day)
    row <- data.frame(alfa2 = as.numeric(lm$coefficients[2]^2),
                      sigmau2 = var(lm$residuals))
    pre_subtabla_iv <- rbind(pre_subtabla_iv, row)
  }
  pre_tabla_iv[[d]] <- pre_subtabla_iv
  rownames(pre_tabla_iv[[d]]) <- paste0("day.", day)
  rm(d, pre_subtabla_iv, Stock_day, lm, row, t)
}

for (d in 1:length(pre_tabla_iv)){
  pre_tabla_iv[[d]] <- pre_tabla_iv[[d]] %>%
    mutate(vol.sa = round(alfa2*100/(alfa2+sigmau2),2))
}

tabla_iv <- data.frame()
for (d in 1:length(pre_tabla_iv)){
  row <- data.frame(mvol.sa = mean(pre_tabla_iv[[d]]$vol.sa))
  tabla_iv <- rbind(tabla_iv, row); rm(d, row)
}
rownames(tabla_iv) <- paste0("St.", idx)

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_iv %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla IV", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Test de Wilcoxon 3
wilcoxont3 <- vector(mode = "list", length = length(idx2))

for (c in 1:length(idx2)){
  subwilcoxont3 <- data.frame()
  for (d in 1:length(idx2[[c]])){
    pre_tabla_iv2 <- pre_tabla_iv
    rownames(pre_tabla_iv2[[which(idx==idx2[[c]][d])]]) <- 
      paste0("day.", day, ".St", idx[which(idx==idx2[[c]][d])])
    portion <- pre_tabla_iv2[[which(idx==idx2[[c]][d])]]
    subwilcoxont3 <- rbind(subwilcoxont3, data.frame(vol.sa = portion$vol.sa))
  }
  wilcoxont3[[c]] <- subwilcoxont3
  rm(c, d, pre_tabla_iv2, subwilcoxont3, portion)
}

wilcox.test(wilcoxont3[[1]]$vol.sa, wilcoxont3[[2]]$vol.sa, 
                      alternative = "two.sided", paired = F)$p.value
wilcox.test(wilcoxont3[[1]]$vol.sa, wilcoxont3[[3]]$vol.sa, 
            alternative = "two.sided", paired = F)$p.value


# Tabla V: Descomposición de la volatilidad del precio observado.
# ¿Qué % de la volatilidad del activo es debida a ruido en precios (no informativo)?
# ¿Qué % es debido a información pública (calidad)?
# Mostrar los % medios por activo.
# Volver a hacer Wilcoxon.

# Esto puede hacerse con las pre_tabla_i, donde se tienen sigmau2, e.gamma y 
# e.alpha.

tabla_v <- data.frame()
for (d in 1:length(pre_tabla_i)){
  pre_tabla_i[[d]] <- pre_tabla_i[[d]] %>%
    mutate(noinfo = 2*e.gamma*(e.alpha + e.gamma),
           info = (e.alpha^2) + sigmau2,
           percnoinfo = noinfo/(noinfo + info),
           Q = sigmau2/(noinfo + info))
  row <- pre_tabla_i[[d]] %>%
    summarise(mpercnoinfo = mean(percnoinfo),
              mQ = mean(Q))
  tabla_v <- rbind(tabla_v, row); rm(row, d)
}
rownames(tabla_v) <- paste0("St.", idx)
tabla_v

var <- colnames(tabla_v) # Se disponen los datos como porcentajes con 2 decimales
tabla_v[, var] <- lapply(tabla_v[, var], percent_format(accuracy = .01))
rm(var)

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_v %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla V", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Test de Wilcoxon 4
wilcoxont4 <- vector(mode = "list", length = length(idx2))

for (c in 1:length(idx2)){
  subwilcoxont4 <- data.frame()
  for (d in 1:length(idx2[[c]])){
    pre_tabla_i2 <- pre_tabla_i
    colnames(pre_tabla_i2[[which(idx==idx2[[c]][d])]])[1:3] <- coef
    rownames(pre_tabla_i2[[which(idx==idx2[[c]][d])]]) <- 
      paste0("day.", day, ".St", idx[which(idx==idx2[[c]][d])])
    portion <- pre_tabla_i2[[which(idx==idx2[[c]][d])]]
    subwilcoxont4 <- rbind(subwilcoxont4, portion)
  }
  wilcoxont4[[c]] <- subwilcoxont4
  rm(c, d, pre_tabla_i2, subwilcoxont4, portion)
}

wilcox.test(wilcoxont4[[1]]$percnoinfo, wilcoxont4[[2]]$percnoinfo, 
            alternative = "two.sided", paired = F)$p.value
wilcox.test(wilcoxont4[[1]]$percnoinfo, wilcoxont4[[3]]$percnoinfo, 
            alternative = "two.sided", paired = F)$p.value
wilcox.test(wilcoxont4[[1]]$Q, wilcoxont4[[2]]$Q, 
            alternative = "two.sided", paired = F)$p.value
wilcox.test(wilcoxont4[[1]]$Q, wilcoxont4[[3]]$Q, 
            alternative = "two.sided", paired = F)$p.value


## 2 ## -----------------------------------------------------------------------
# Para cada activo y día negociado hay que proporcionar la descomposición de la
# HORQUILLA EFECTIVA MEDIA en sus componentes teóricos (horquilla realizada e
# impacto en precios) utilizando la técnica expuesta en la Sesión VII. Para el
# cálculo del impacto en precios y la horquilla realizada se necesita el punto
# medio de la horquilla en algún momento posterior a cada transacción. Hay que 
# proporcionar resultados utilizando horitontes de tao = {5, 30, 60} segundos
# tras la transacción. El punto medio tras la transacción debe obtenerse de los 
# ficheros NBBO, tomando las cotizaciones que estén AL MENOS, t segundos tras la 
# transacción. Si no existen registros tao segundos tras la transacción puede
# eliminarse la transacción del análisis. Debe entregarse:

# Descomposición de la HORQUILLA EFECTIVA MEDIA:
# Esto es la horquilla efectiva:
# S(e)_it = 2*(P_it - Q_it)*X_it             Puede calcularse con Stock
# Esto es la horquilla realizada:
# S(rz)_it = 2*(P_it - Q_(it+tao))*X_it
# Esto es el impacto en precio:
# IP_it = 2*(Q_(it+tao) - Q_it)*X_it
# Habrá, por tanto, una medida para cada operación (compra y venta)

# Tabla VI:
# La horquilla efectiva, realizada y el impacto estimado medio en precios 
# (entre todas las transacciones) por día y activo (9 activos*7 medidas)

# Procedimiento
# Filtrar por día en Stock[[d]] y NBBO[[d]]
# X_it es buysell en Stock[[d]]
# P_it es el precio en Stock[[d]]
# Q_it es el punto medio en Stock[[d]]; debe generarse
# Q_(it+tao) debe buscarse en NBBO[[d]]

pre_tabla_vi <- vector(mode = "list", length = length(Stock))

for (d in 1:length(Stock)){
  NBBO[[d]] <- NBBO[[d]] %>% mutate(Q = (ask+bid)/2)
  Stock[[d]] <- Stock[[d]] %>% mutate(Q = (ask+bid)/2)
  day <- unique(Stock[[d]]$day)
  pre_subtabla_vi <- data.frame()
  for (t in day){
    NBBO_day <- NBBO[[d]] %>% filter(day == t)
    Stock_day <- Stock[[d]] %>% filter(day == t)
    Qtaopick <- function(tao){
      # Función para la generación del vector de los precios eficientes en
      # un horizonte temporal dado.
      # INPUTS:
      # tao: horizonte temporal en segundos.
      # OUTPUTS:
      # Qtao: vector con los precios eficientes del horizonte temporal de tao,
      # correspondientes a cada una de las operaciones de Stock del día t.
      T <- Stock_day$time
      QNBBO <- NBBO_day$Q
      F <- NBBO_day$time
      Qtao <- c()
      for (p in 1:length(T)){
        if (F[F == min(F[F >= T[p]+tao])][1] - T[p] < (tao + 1))
        {Qtao[p] <- QNBBO[F == min(F[F >= T[p]+tao])][1]}
        else
        {Qtao[p] <- NA}
        rm(p) # Se ha observado que en NBBO hay momentos en los cuales se 
        # presentan varios precios para el mismo momento temporal referido 
        # (el mismo precio o diferentes). Como el enunciado especifica 
        # "al menos", cabe quedarnos con el 1º [1] de los registros en el
        # orden presentado para Q_(it+tao), de forma que se supone que los
        # registros posteriores se efectúan en instantes posteriores aunque
        # la variable time muestre el mismo valor.
      }
      return(Qtao)
    }
    Stock_day$Qtao5 <- Qtaopick(5)
    Stock_day$Qtao30 <- Qtaopick(30)
    Stock_day$Qtao60 <- Qtaopick(60)
    pre_subtabla_vi <- rbind(pre_subtabla_vi, Stock_day)
  }
  pre_tabla_vi[[d]] <- pre_subtabla_vi
  cat(paste0("Stock",idx[d], " completed. "))
  rm(d, t, day, Stock_day, NBBO_day, Qtaopick, pre_subtabla_vi)
}

tabla_vi <- data.frame()

for (d in 1:length(pre_tabla_vi)){
  pre_tabla_vi_stock <- pre_tabla_vi[[d]] %>%
    mutate(Se = 100*2*(price - Q)*buysell,
           Srz5 = 100*2*(price - Qtao5)*buysell,
           ip5 = 100*2*(Qtao5 - Q)*buysell,
           Srz30 = 100*2*(price - Qtao30)*buysell,
           ip30 = 100*2*(Qtao30 - Q)*buysell,
           Srz60 = 100*2*(price - Qtao60)*buysell,
           ip60 = 100*2*(Qtao60 - Q)*buysell) %>%
    # Cálculo de las medias "entre todas las transacciones"
    summarise(mSe = round(mean(Se, na.rm = T),2),
              mSrz5 = round(mean(Srz5, na.rm = T),2),
              mip5 = round(mean(ip5, na.rm = T),2),
              mSrz30 = round(mean(Srz30, na.rm = T),2),
              mip30 = round(mean(ip30, na.rm = T),2),
              mSrz60 = round(mean(Srz60, na.rm = T),2),
              mip60 = round(mean(ip60, na.rm = T),2))
  tabla_vi <- rbind(tabla_vi, pre_tabla_vi_stock)
  rm(d, pre_tabla_vi_stock)
}
rownames(tabla_vi) <- paste0("Stock",idx)


# Tabla VII:
# La horquilla efectiva, la realizada y el impacto en precios medio diario para
# cada activo (promediando las medias por día) + Contraste de Wilcoxon

pre_tabla_vii <- vector(mode = "list", length = length(pre_tabla_vi))

for (d in 1:length(pre_tabla_vi)){
  pre_tabla_vi[[d]] <- pre_tabla_vi[[d]] %>%
    mutate(Se = 100*2*(price - Q)*buysell,
           Srz5 = 100*2*(price - Qtao5)*buysell,
           ip5 = 100*2*(Qtao5 - Q)*buysell,
           Srz30 = 100*2*(price - Qtao30)*buysell,
           ip30 = 100*2*(Qtao30 - Q)*buysell,
           Srz60 = 100*2*(price - Qtao60)*buysell,
           ip60 = 100*2*(Qtao60 - Q)*buysell)
  day <- unique(pre_tabla_vi[[d]]$day)
  pre_subtabla_vii <- data.frame()
  for (t in day){
    pre_tabla_vi_day <- pre_tabla_vi[[d]] %>%
      filter(day == t) %>%
      summarise(mprice = mean(price, na.rm = T),
                mSe = mean(Se, na.rm = T),
                mSrz5 = mean(Srz5, na.rm = T),
                mip5 = mean(ip5, na.rm = T),
                mSrz30 = mean(Srz30, na.rm = T),
                mip30 = mean(ip30, na.rm = T),
                mSrz60 = mean(Srz60, na.rm = T),
                mip60 = mean(ip60, na.rm = T))
    pre_subtabla_vii <- rbind(pre_subtabla_vii, pre_tabla_vi_day)
  }
  pre_tabla_vii[[d]] <- pre_subtabla_vii
  rownames(pre_tabla_vii[[d]]) <- day
  rm(d, t, pre_tabla_vi_day, pre_subtabla_vii)
}

tabla_vii <- data.frame()

for (d in 1:length(pre_tabla_vii)){
  pre_tabla_vii[[d]] <- pre_tabla_vii[[d]] %>%
    mutate(mrSe = mSe/mprice,
           mrSrz5 = mSrz5/mprice,
           mrip5 = mip5/mprice,
           mrSrz30 = mSrz30/mprice,
           mrip30 = mip30/mprice,
           mrSrz60 = mSrz60/mprice,
           mrip60 = mip60/mprice)
  row <- pre_tabla_vii[[d]] %>%
    summarise(mmSe = mean(mSe, na.rm = T),
              mmSrz5 = mean(mSrz5, na.rm = T),
              mmip5 = mean(mip5, na.rm = T),
              mmSrz30 = mean(mSrz30, na.rm = T),
              mmip30 = mean(mip30, na.rm = T),
              mmSrz60 = mean(mSrz60, na.rm = T),
              mmip60 = mean(mip60, na.rm = T),
              mmrSe = mean(mrSe, na.rm = T),
              mmrSrz5 = mean(mrSrz5, na.rm = T),
              mmrip5 = mean(mrip5, na.rm = T),
              mmrSrz30 = mean(mrSrz30, na.rm = T),
              mmrip30 = mean(mrip30, na.rm = T),
              mmrSrz60 = mean(mrSrz60, na.rm = T),
              mmrip60 = mean(mrip60, na.rm = T))
  tabla_vii <- rbind(tabla_vii, row); rm(row, d)
}
rownames(tabla_vii) <- paste0("Stock",idx)

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_vii[,1:7] %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla VI", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Para exportar tablas a formato Word
ft <- flextable(data = tabla_vii[,8:14] %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Tabla VII", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento

# Test de Wilcoxon 5
wilcoxont5 <- vector(mode = "list", length = length(idx2))

for (c in 1:length(idx2)){
  subwilcoxont5 <- data.frame()
  for (d in 1:length(idx2[[c]])){
    pre_tabla_vii2 <- pre_tabla_vii
    rownames(pre_tabla_vii2[[which(idx==idx2[[c]][d])]]) <- 
      paste0("day.", day, ".St", idx[which(idx==idx2[[c]][d])])
    portion <- pre_tabla_vii2[[which(idx==idx2[[c]][d])]]
    subwilcoxont5 <- rbind(subwilcoxont5, portion)
  }
  wilcoxont5[[c]] <- subwilcoxont5
  rm(c, d, pre_tabla_vii2, subwilcoxont5, portion)
}

wilcoxtable5 <- data.frame(aux = rep(NA, ncol(wilcoxont5[[1]])))

for (c in 1:length(pairs)){
  subwilcoxtable5 <- data.frame()
  for (r in 1:(ncol(wilcoxont5[[c]]))){
    x <- wilcoxont5[[pairs[[c]][1]]][, r]
    y <- wilcoxont5[[pairs[[c]][2]]][, r]
    pvalue <- wilcox.test(x, y, alternative = "two.sided", paired = F)$p.value
    pvalue <- data.frame(pick = pvalue)
    subwilcoxtable5 <- rbind(subwilcoxtable5, pvalue)
  }
  wilcoxtable5 <- cbind(wilcoxtable5, subwilcoxtable5)
  rm(c, r, x, y, pvalue, subwilcoxtable5)
}
wilcoxtable5 <- wilcoxtable5[, 2:4]
rownames(wilcoxtable5) <- colnames(pre_tabla_vii[[1]])
colnames(wilcoxtable5) <- c("Gran_vs_Med", "Gran_vs_Peq", "Med_vs_Peq")

# Para exportar tablas a formato Word
ft <- flextable(data = format(wilcoxtable5, scientific = T) %>% add_rownames()) %>% 
  theme_zebra %>% autofit
ft <- set_caption(ft, caption = "Contrastes de Wilcoxon", style = "Table Caption")
# Crea un archivo temp
tmp <- tempfile(fileext = ".docx")
# Crea un documento docx
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)
browseURL(tmp) # abre el documento
