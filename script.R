
# Fecha del informe: 10/08/2014

#### Objetivos ####

# 1. Estudiar las características de los clientes que compran el producto.
# 2. Conocer la probabilidad de que un cliente compre el producto 
# dadas determinadas características
# 3. Desplegar un informe para el sector de marketing que permita conocer
# la tasa de respuesta de acuerdo a los llamados y, por lo tanto, el ingreso.

#### librería y datos ####
library(tidyverse)
library(naniar)
library(class)
library(ggtext)
library(showtext)
library(RColorBrewer)
library(corrplot)
showtext_auto()
font_add_google(name="Noto Sans", family = "noto")

# Formato de gráficos
theme_set(
  theme_light(
    base_family = "noto") +
    theme(
      # legend.title = element_text(face = "bold"),
      # axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom", 
      text = element_text(size=10),
      axis.text.x = element_text(size = 10),
      strip.text = element_text(color = "black"),
      strip.background = element_blank()
    )
)

# Cargo los datos
rw <- readxl::read_excel("marketing_campaign.xlsx")
str(rw)

rw$Z_CostContact <- NULL
rw$Z_Revenue <- NULL

# Creamos una columna 'fidelity' que resuma la cantidad de ofertas que aceptó en las 
# campañas anteriores
rw$fidelity <- apply(rw[,c(21:25, 27)], MARGIN = 1, FUN = sum)

# Evalúo cantidad de clientes y valores duplicados
unique(rw) %>% nrow()
unique(rw$ID) %>% length()

# No hay filas ni clientes duplicados
# Evalúo valores nulos
is.na(rw) %>% table()

vis_miss(rw)

rw %>% 
  gg_miss_fct(fct = Education) + 
  labs(x = "Año", y = "Variable", title = "NAs en cada variable según año")

# Hay 24 valores nulos en la variable ingresos. En general corresponden a 
# personas solteras y con educación en segundo ciclo o posgrados. Tambien
# viudos.
# Se completarán los valores con criterios de cercanía entre parecidos.

#### Análisis descriptivo ####

##### ¿Cómo fue el comportamiento en las ultimas campañas? ##### 
rw %>% 
  select(ID, Primera = AcceptedCmp1, 
         Segunda = AcceptedCmp2, 
         Tercera = AcceptedCmp3, 
         Cuarta = AcceptedCmp4, 
         Quinta = AcceptedCmp5, 
         Sexta = Response) %>% 
  pivot_longer(-ID, names_to = "Campaña", values_to = "aceptaciones") %>% 
  mutate(Campaña = factor(Campaña, levels = c("Primera", "Segunda", "Tercera", "Cuarta", "Quinta", "Sexta"))) %>%
  group_by(Campaña) %>% 
  summarise(cantidad.aceptada = sum(aceptaciones)) %>% 
  ggplot() + 
  aes(x = Campaña, y = cantidad.aceptada) +
  geom_col(fill = "darkcyan") +
  geom_text(aes(label = cantidad.aceptada), vjust = -0.5, color = "black", size = 3) +
  xlab("") +
  ylab("Cantidad de clientes") +
  labs(title = "Clientes que adquirieron el producto en las últimas campañas")
  
##### De la muestra de clientes, cuantas veces aceptaron la campaña? #####
datos_clientes <- mutate(rw, fidelity = factor(fidelity))

# Calcula la proporción de clientes para cada nivel de 'fidelity'
proporciones <- datos_clientes %>%
  group_by(fidelity) %>%
  summarise(proporcion = n() / nrow(datos_clientes))

# Crea el gráfico de lollipop
ggplot(proporciones, aes(x = fidelity, y = proporcion)) +
  geom_segment(aes(xend = fidelity, yend = 0), color = "darkcyan", size = 1) +  # Agrega los palitos
  geom_point(size = 1.5, color = "black", fill = "white") +  # Agrega los puntos en el extremo de los palitos
  labs(title = "Proporción de Clientes por Nivel de Fidelity", x = "Nivel de Fidelity", y = "Proporción")

##### ¿Qué características tienen los clientes que compraron nuestros productos la última campaña? #####

# Características socioeconómicas

# Diferencias en edad (puede variar en un año)
rw$edad <- 2014 - rw$Year_Birth

rw %>% 
  ggplot() +
  geom_density(aes(x = edad, col = as.factor(Response), fill = as.factor(Response)), alpha = 0.6) +
  scale_color_manual(values = c("0" = "deeppink2", "1" = "darkcyan"), labels = c("0" = "No compró", "1" = "compró")) + 
  scale_fill_manual(values = c("0" = "deeppink2", "1" = "darkcyan"), labels = c("0" = "No compró", "1" = "compró")) +  
  labs(title = "Edad de compradores y no compradores") +
  guides(color = guide_legend(title=""),
         fill =  guide_legend(title="")) +
  ylab("Densidad")

# Diferencias en educación, estado civil, y cantidad de hijos con analisis de correspondencias multiples

# Creo una variable con cantidad de hijos, es más categórica que otra cosa
rw$qchild <- apply(rw[,c("Kidhome","Teenhome")], MARGIN = 1, FUN = sum)
rw$qchild <- as.factor(rw$qchild)

# Define el orden específico para Education y Marital_Status
order_education <- c("Basic", "2n Cycle", "Graduation", "PhD", "Master")
orden_marital_status <- c("Married", "Together", "Single", "Divorced", "Widow")

rw %>%
  select(ID, Education, Marital_Status, qchild) %>%
  mutate(Education = factor(Education, levels = order_education),
         Marital_Status = factor(Marital_Status, levels = orden_marital_status)) %>%
  pivot_longer(-ID, names_to = "nombre", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_bar(fill = "tomato1") +
  facet_wrap(~nombre, scales = "free") +
  labs(title = "Distribución de Categorías",
       x = "Valor",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) 

# Ventas por educación
rw %>%
  ggplot(aes(x = Education, fill = as.factor(Response))) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "dodgerblue4", "1" = "dodgerblue1"), 
                    labels = c("0" = "No compró", "1" = "compró")) +
  labs(title = "Proporción de Ventas por Nivel de Educación", fill = "Response") +
  guides(fill =  guide_legend(title="")) +
  ylab("Proporción")

# Ventas por Estado Civil (se agrupan yolo, absurd y single)
rw$Marital_Status <- if_else(rw$Marital_Status %in% c("YOLO", "Absurd", "Alone"), "Single", rw$Marital_Status)

rw %>%
  ggplot(aes(x = Marital_Status, fill = as.factor(Response))) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "dodgerblue4", "1" = "dodgerblue1"), 
                    labels = c("0" = "No compró", "1" = "compró")) +
  labs(title = "Proporción de Ventas por Estado Civil", fill = "Response") +
  guides(fill =  guide_legend(title="")) +
  ylab("Proporción") +
  xlab("Estado Civil")

# Ventas por Estado Civil (podría considerarse absurd, alone y YOLO como Soltero)
rw %>%
  ggplot(aes(x = as.factor(qchild), fill = as.factor(Response))) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "dodgerblue4", "1" = "dodgerblue1"), 
                    labels = c("0" = "No compró", "1" = "compró")) +
  labs(title = "Proporción de Ventas por Cantidad de hijos", fill = "Response") +
  guides(fill =  guide_legend(title="")) +
  ylab("Proporción") +
  xlab("Cantidad de hijos")

##### ¿Quién compró en al menos una campaña? #####
rw$atleastone <- if_else(rw$fidelity > 0, "Compró", "No Compró")
rw$qchild <- as.numeric(rw$qchild)-1
rw$hijos <- if_else(rw$qchild >= 1, "Hijos", "sin Hijos")

# Análisis de correspondencias múltiples 
df_mca <- rw %>% 
  select(Education, Marital_Status, hijos, atleastone)
mca <- FactoMineR::MCA(df_mca)

# Gráfico de componentes
factoextra::fviz_mca_var(mca, col.var = "dodgerblue4") + 
  labs(title = "")

##### ¿Hay diferencias en los gastos de los consumidores que compraron y no compraron respecto al ingreso? #####

# Hay presencia de atípicos en varios grupos de variables que pueden distorsionar
# las tendencias.
# Hay una persona que tiene un salario de 666666 (dato mal cargado)

rw %>% 
  select(atleastone, Income, MntWines, MntMeatProducts, MntFishProducts, MntFruits, MntSweetProducts, MntGoldProds) %>% 
  pivot_longer(-c(Income,atleastone), values_to = "val", names_to = "name") %>% 
  ggplot(aes(x = val, y = Income, color = atleastone)) +
  geom_point(size = 2) +
  ylab("PBI Per Capita log") +
  guides(color = guide_legend(title="atleastone")) +
  scale_color_manual(values = c("Compró" = "orange", "No Compró" = "dodgerblue")) +
  xlab("") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme(
    strip.text = element_text(color = "black"),  # Cambia el color del texto del título de la faceta a negro
    strip.background = element_blank()  # Elimina el fondo de la faceta
  ) 

rw <- filter(rw, Income < 600000)

rw %>% 
  mutate_if(is.numeric, log) %>%
  select(atleastone, Income, MntWines, MntMeatProducts, MntFishProducts, MntFruits, MntSweetProducts, MntGoldProds) %>% 
  pivot_longer(-c(Income,atleastone), values_to = "val", names_to = "name") %>% 
  ggplot(aes(x = val, y = Income, color = atleastone)) +
  geom_point(size = 2) +
  ylab("log del ingreso") +
  xlab("log de gasto en productos") +
  guides(color = guide_legend(title="atleastone")) +
  xlab("") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  scale_color_manual(values = c("Compró" = "orange", "No Compró" = "dodgerblue")) +
  theme(strip.text = element_text(color = "black"),
        strip.background = element_blank())

# Si se estudian las elasticidades del ingreso a priori no se observan cambios significativos
# Entre los que compran y no compran los productos

##### ¿Hay productos que se consumen al mismo tiempo? ¿Cuál es su relación con la edad? #####

rw %>% 
  select(edad, MntWines, MntFishProducts, MntFruits, MntGoldProds, MntMeatProducts, 
         MntSweetProducts) %>% 
  GGally::ggpairs() +
  scale_fill_manual(values = mcolors)

rw %>% 
  select(MntWines, MntFishProducts, MntFruits, MntGoldProds, MntMeatProducts, 
         MntSweetProducts, edad) %>% 
  cor(.) %>% 
  corrplot(type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))

# Existen muy pocos casos de adultos mayores a 100 años. Se eliminan los casos por lo
# que el análisis es para menores de 100 años.

## Lo mismo con usuarios que compraron < 150000 kg de carne
rw <- filter(rw, edad <=100)
rw <- filter(rw, MntMeatProducts <= 1500)
rw <- filter(rw, MntSweetProducts <= 250)

##### ¿Qué características tienen los compradores respecto a sus hábitos? ###
rw %>% 
  select(atleastone, Income, MntWines, MntMeatProducts, MntFishProducts, MntFruits, MntSweetProducts, MntGoldProds) %>% 
  pivot_longer(-atleastone, values_to = "val", names_to = "name") %>% 
  ggplot(aes(y = val, fill = atleastone)) +
  geom_boxplot() +
  ylab("Consumo") +
  guides(color = guide_legend(title="atleastone")) +
  scale_fill_manual(values = c("Compró" = "dodgerblue1", "No Compró" = "dodgerblue4")) +
  xlab("") +
  facet_wrap(~ name, scales = "free") +
  guides(fill =  guide_legend(title="")) +
  theme(axis.text.x = element_blank())

## Clustering ####
library(tidymodels)

# Tipos de estandarización
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)

# Paso 1: Preprocesamiento de datos
clustering_data_scaled <- rw %>% 
  select(-c(Year_Birth, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5, Complain, Response)) %>% 
  filter(!is.na(Income)) %>% 
  mutate(hijos = as.numeric(ifelse(hijos == "Hijos", 1, 0)),
         atleastone = as.numeric(ifelse(atleastone == "Compró", 1, 0))) %>% 
  select(-ID) %>% 
  select_if(is.numeric) %>% 
  mutate_all(rob_scale)

# Prueba de hopkins para agrupamiento
hopkins = factoextra::get_clust_tendency(rw_procesado, n=100, seed=321)
cat("Hopkins =", hopkins$hopkins_stat)

library(dplyr)
library(tidymodels)
library(tidyclust)

