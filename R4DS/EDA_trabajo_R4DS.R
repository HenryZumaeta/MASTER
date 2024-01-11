# //////////////////////////////////////////////////////////////////////////////
# TEMA              :  Análisis Exploratorio de Datos
# AUTOR             :  Zumaeta Lozano, Henry Paolo
# 
# FECHA VALIDACION  :  16/08/2023
# //////////////////////////////////////////////////////////////////////////////

#______________________ PREPARACION DEL ENTORNO DE TRABAJO _____________________ ----

# **************************
# Limpiar el workspace  ----
# **************************
rm(list = ls())

# ***********************
# Cargamos librerías ----
# ***********************
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# "BRugs", # Trabaja con R 32 bits
packages <- c("caret","DataExplorer","VIM","missForest","ggplot2","dummies",
              "lattice","colorspace","data.table","randomForest","foreach",
              "itertools","MASS","pROC","foreign","gmodels","InformationValue",
              "caTools","MLmetrics","dplyr","iterators","tidyverse","kableExtra",
              "scales","Boruta","R2OpenBUGS","factoextra","mvoutlier","outliers",
              "cluster","fpc","mclust","dbscan","readxl","psych","corrplot",
              "mclust","gclus","rrcov","tourr","aplpack","TeachingDemos","rgl",
              "ape","DMwR2","GGally","Hmisc","PerformanceAnalytics","e1071",
              "class","sqldf","gplots","RColorBrewer","robust","descr",
              "openxlsx","mice","gamlss","reshape2","MVN","xts","ggthemes",
              "ggpubr","party","klaR","openair","lubridate","apaTables",
              "corrr","see","tidygraph","ggraph","correlation","ggstatsplot",
              "semPlot", "rmarkdown", "funModeling", "prettydoc", "rmdformats",
              "tint", "tufte", "car", "ggpubr", "emmeans", "SmartEDA", "modeest",
              "flextable","dlookr", "summarytools", "skimr", "moments")

ipak(packages)

# ***************************
# Seteamos el directorio ----
# ***************************
setwd(r"(D:\Programacion\R\ZConsu\MAESTRIA\CI1)")
getwd()


# *********************
# Lectura de datos ----
# *********************
library(foreign)
original <-read.spss("Bases/BASE MODELO DE SATISFACCIÓN - GRUPO 2.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)

# *****************************************
# Revisando la estructura de los datos ----
# *****************************************
str(original)
dim(original)
dplyr::glimpse(original)
summary(original)

#______________________________ LIMPIEZA DE DATOS ______________________________ ----

# *************************
# Modificando columnas ----
# *************************

## ++++++++++++++
## Variables ----
## ++++++++++++++
library(dplyr)

base <- original %>% 
    rename(encu = enc,
           solu = p5_1,
           agil = p5_2,
           inte = p5_3,
           amab = p5_4,
           ofer = p5_5,
           cono = p5_6,
           clar = p5_7,
           espe = p5_8,
           aten = p5_9,
           simp = p5_10,
           flex = p5_11,
           publ = p7_1,
           orde = p7_2,
           limp = p7_3,
           arre = p7_4,
           infr = p7_5,
           comd = p7_6,
           iden = p7_7,
           target = p8)

## ++++++++++++++
## Etiquetas ----
## ++++++++++++++
library(Hmisc)
etiquetas = c("Número de encuesta","Solucion dada en el local",
              "Agilidad mostrada","Interes mostrado","Amabilidad y trato",
              "Oferta de productos y servicios",
              "Nivel de conocimientos del funcionario / cajero",
              "Claridad de la información","Tiempo de espera en ser atendido",
              "Tiempo de atención brindada",
              "Simpleza durante el proceso de atención",
              "Flexibilidad","Publicidad en el local","Orden del local",
              "Limpieza del local","Personal debidamente arreglado",
              "Infraestructura / diseño del local",
              "Comodidad de la instalaciones","Personal debidamente identificado",
              "Satisfacción por la atención")

baseEtiquetada <- base

for (i in 1:length(etiquetas)) {
    label(baseEtiquetada[,i]) <- etiquetas[i]
}

## ++++++++++++++++++++++++++
## Convirtiendo a factor ----
## ++++++++++++++++++++++++++
# Si queremos trabajar toda la data como factor
baseFactor <- base[,-1]

for (i in 1:length(baseFactor)) {
    baseFactor[i] <- as.factor(baseFactor[,i])
}
# Revisando la data
str(baseFactor)
dim(baseFactor)
dplyr::glimpse(baseFactor)
summary(baseFactor)

#_______________________ ANALISIS EXPLORATORIO DE DATOS ________________________ ----

# *********************
# Gráficos básicos ----
# *********************

## +++++++++++++++++++++++++++++++
## Análisis de datos perdidos ----
## +++++++++++++++++++++++++++++++

### ...................................
### Verificación de datos perdidos ----
### ...................................
library(DataExplorer)

# Visualizar la estructura de la data
plot_str(base)

# Resumen de la data
introduce(base)
plot_intro(base)

# Valores faltantes o missing
plot_missing(base) 
profile_missing(base)
VIM::aggr(base, numbers = T, sortVar =T)

## +++++++++++++++++++++++++++++++++++
## Gráficos variables categóricas ----
## +++++++++++++++++++++++++++++++++++

### ..............................................
### Distribución de las variables categóricas ----
### ..............................................
# Barplot
for (i in 3:length(etiquetas)-1) {
    plot(as.factor(base[,i]),
         main = etiquetas[i],
         col = "#87CEFA")
}

library(SmartEDA)
ExpCatViz(baseFactor,
          Page = c(2,5))

ExpCatViz(
    baseFactor %>% 
        select(target, solu), 
    target="target")


# Histograma
for (i in 3:length(etiquetas)-1) {
    plot_histogram(base[,i],
                   title = etiquetas[i],
                   ggtheme = theme_minimal())
}

library(funModeling)
plot_num(base)

# Densidad
library(ggpubr)
ggdensity(baseEtiquetada, x = "target",
          add = "mean", rug = TRUE,
          color = "target", fill = "target")

densityplot(base$target)

# qqplot
for (i in 3:length(etiquetas)-1) {
    plot_qq(base[,i],
            title = etiquetas[i],
            ggtheme = theme_minimal())
}


ggqqplot(base, x = "target")

# Dispersión por categoría
ggline(base,
       x = "solu",
       y = "target",
       add =  c("mean_se", "jitter"),
       xlab = " Solucion dada en el local",
       ylab = "Satisfacción por la atención",
       title = "",
       color = "#00AFBB",
       point.color = "black")

### .........
### Moda ----
### .........
library(modeest)
mlv(baseFactor$target, method = "meanshift")


### ............................
### Análisis de correlación ----
### ............................
# Cuadro completo
plot_correlation(na.omit(base[,-c(1,20)]),
                 title = "Correlación de las variables",
                 ggtheme = theme_minimal())

#Calculo de la correlación
correlacion <- round(cor(base[,-c(1,20)]),2) 
correlacion

# Cuadro triangular superior
corrplot(correlacion, method="number", type="upper")

# Correlaciones visuales
base[,-c(1,20)] %>% 
    correlate() %>% 
    network_plot(min_cor = .2, colors = c("red", "green"), legend = "range")

base[,-c(1,20)] %>% 
    correlation() %>% 
    visualisation_recipe() %>% 
    plot()

# Correlación robusta
library(robust)
base %>%
    covRob()

### ......................
### Diagrama de cajas ----
### ......................
# todas las variables
ggplot(stack(base[,-c(1,20)]), aes(x = ind, y = values)) + # stack(), formato largo
    geom_boxplot()

# Evaluando una variable
ggplot(base, aes(x = solu, y = target)) + 
    geom_boxplot()

# Diagrama de caja y bigotes
ggboxplot(base,
          x = "solu",
          y = "target",
          fill = "solu",
          xlab = " Solucion dada en el local",
          ylab = "Satisfacción por la atención",
          title = "Boxplot") +
    theme_bw()

### .......................
### Diagrama de violin ----
### .......................
ggplot(data = base, aes(solu, target))+
    geom_violin()+
    labs(x = "Solución", y = "Satisfacción")


### .................................
### Gráfica de acondicionamiento ----
### .................................
ggplot(base, (aes(x=solu, y=target))) + 
    stat_binhex(colour="white") + 
    theme_bw() + 
    scale_fill_gradient(low="gray95", high="red") +
    labs(x="Solución", y="Satisfacción")

### ..........................
### Tabla de contingencia ----
### ..........................
library(descr)
CrossTable(x = base$solu,
           y = base$target,
           prop.c = F,
           prop.chisq = F,
           prop.t = F)

### ...........................
### Resúmenes estadísticos ----
### ...........................
# Método 01:
library(funModeling)    
status(base) %>% flextable()


# Método 02:
library(flextable)        
dlookr::describe(base[,-1]) %>%
    flextable()

# Método 03:
base %>% 
    diagnose_numeric() %>% 
    flextable()

# Método 04:
library(SmartEDA)
ExpNumStat(base, by="A", Outlier=TRUE, Qnt = c(.25, .75), round = 2) %>%
    flextable()

ExpNumStat(base, by="G", gp="target", Outlier=TRUE, Qnt = c(.25, .75),
           round = 2) %>%
    flextable()

ExpNumStat(base, by="GA", gp="target", Outlier=TRUE, Qnt = c(.25, .75),
           round = 2) %>%
    flextable()

# Método 05:
library(summarytools)
base %>% 
    group_by(target) %>% 
    descr()

# Método 06:
library(psych)
describeBy(base,
           base$target)

# Método 07:
library(summarytools)
dfSummary(base)

# Método 08:
library(skimr)
skim(base)


#____________________________ POR SI ACASO UNOS TEST ___________________________ ----
# ***********************
# Evaluado normalidad ----
# ***********************
# Individual
library(stats)
shapiro.test(base$target)

# Grupal
library(dlookr)
normality(base) %>%
    mutate_if(is.numeric, ~round(., 3)) %>% 
    flextable()

# ***************************
# Evaluando la asimetría ----
# ***************************
library(moments)
skewness(base$target, na.rm = T) 

skewness(base$solu, na.rm = T) 

# Test de asimetría
agostino.test(base$target)
agostino.test(base$solu)

# **************************
# Evaluando la curtosis ----
# **************************
# El valor de curtosis para una distribución normal es de alrededor de tres.
# valor p que diga si el resultado de la curtosis está significativamente
# lejos de tres

# Test de Anscombe-Glynn
anscombe.test(base$target)
anscombe.test(base$solu)

#____________________________ EXPORTACION DE LA DATA ___________________________ ----

# ***************************
# Exportación de la data ----
# ***************************

## ++++++++++++++++++++++++++
## Crear reporte en HTML ----
## ++++++++++++++++++++++++++
library(prettydoc)
library(rmdformats)
library(tint)
library(tufte)

# Configurando el reporte:
configuracion <- configure_report(
    add_introduce = TRUE,
    add_plot_intro = T,
    add_plot_str = F,
    add_plot_missing = T,
    add_plot_histogram = T,
    add_plot_density = FALSE,
    add_plot_qq = T,
    add_plot_bar = T,
    add_plot_correlation = T,
    add_plot_prcomp = T,
    add_plot_boxplot = T,
    add_plot_scatterplot = T,
    introduce_args = list(),
    plot_intro_args = list(),
    plot_str_args = list(type = "diagonal", fontSize = 35,
                         width = 1000,
                         margin = list(left = 350, right = 250)),
    plot_missing_args = list(),
    plot_histogram_args = list(ggtheme = theme_gray()),
    plot_density_args = list(),
    plot_qq_args = list(sampled_rows = 1000L),
    plot_bar_args = list(),
    plot_correlation_args = list(cor_args = list(use = "pairwise.complete.obs")),
    plot_prcomp_args = list(),
    plot_boxplot_args = list(),
    plot_scatterplot_args = list(sampled_rows = 1000L),
    global_ggtheme = quote(theme_minimal(base_size = 14)),
    global_theme_config = list()
)

# Con formato: prettydoc
create_report(base[,-c(1,20)],
              output_format = html_pretty(toc = TRUE,
                                          toc_depth = 6,
                                          theme ="hpstr"),
              output_file = "EDA_trabajo_R4DS.html",
              report_title = "Análisis Exploratorio de Datos ",
              config = configuracion)

# Con formato: rmdformats
create_report(base[,-c(1,20)],
              output_format = material(),
              output_file = "EDA_trabajo_R4DS.html",
              report_title = "Análisis Exploratorio de Datos",
              config = configuracion)

# Con formato: tint
create_report(base[-c(1,20)],
              output_format = tintHtml(),
              output_file = "EDA_trabajo_R4DS.html",
              report_title = "Análisis Exploratorio de Datos",
              config = configuracion)

# Con formato: tufte
create_report(base[-c(1,20)],
              output_format = tufte_html(),
              output_file = "EDA_trabajo_R4DS.html",
              report_title = "Análisis Exploratorio de Datos",
              config = configuracion)

## ++++++++++++++++++++++++++++++
## Exportamos en formato csv ----
## ++++++++++++++++++++++++++++++
dir.create("Salida")
write.csv(base, "Salida/data_csv.csv", row.names = F)
