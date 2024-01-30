### Librerias y setup ####
library(tidyverse)
library(ggtext)
library(showtext)
library(RColorBrewer)
library(corrplot)
library(statmod) 
library(pROC) 
library(ResourceSelection) 
library(dplyr)
library(MASS)
library(ggplot2)
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

# Cargamos los datos y procesamos las variables. La validación se hizo previamente

rw <- readxl::read_excel("marketing_campaign.xlsx")
str(rw)

rw$Z_CostContact <- NULL
rw$Z_Revenue <- NULL

# Nos interesa conocer si compran o no, independientemente de cuando lo hicieron. Por lo tanto
# nuestra variable a predecir es AtLeastOnce, que indica si al menos compró una vez (si) o si
# nunca lo hizo (no)
rw$fidelity <- apply(rw[,c(21:25, 27)], MARGIN = 1, FUN = sum)
rw$ATLEASTONCE <- if_else(rw$fidelity > 0, 1, 0)

# Estudiamos la distribución de la variable objetivo
ggplot(rw) + 
  aes(x = ifelse(ATLEASTONCE == 1, "Compró", "No Compró")) +
  geom_bar(fill = "darkcyan") +
  xlab("Condición")

# El panel está desbalanceado pero no tanto
# Ahora vamos a seleccionar variables categóricas que nos van a ayudar a conocer las probabilidades
# de que compre o no 

# El primer perfil va a ser Educacion. Estudiamos su distribucion
table(rw$Education) %>% plot()

# Transformación: Se agrupa maestría y 2n Cycle en "maestria"
rw$Education <- ifelse(rw$Education == "2n Cycle", "Master", rw$Education)

# Segundo perfil: Marital_Status. Agrupamos absurd, alone, single y YOLO
# Tambien se agrupa a la gente en pareja

table(rw$Marital_Status)
rw$Marital_Status <- ifelse(rw$Marital_Status %in% c("Absurd", "Alone", "YOLO"), "Single", rw$Marital_Status)
rw$Marital_Status <- ifelse(rw$Marital_Status %in% c("Together", "Married"), "Together", rw$Marital_Status)
# Tercer perfil: childs. (Si tiene o no hijos)
rw$qchild <- apply(rw[,c("Kidhome","Teenhome")], MARGIN = 1, FUN = sum)
rw$childs <- ifelse(rw$qchild>0, "Con.Hijos", "Sin.Hijos")

# Compras totales
rw$total.purchases <- apply(rw[,16:19], MARGIN = 1, FUN = sum)


### Modelo maximal ####
df.total <- rw %>% 
  dplyr::select(ATLEASTONCE, Education, Marital_Status, qchild, fidelity, MntWines, 
                MntFishProducts, MntFruits, MntGoldProds, MntMeatProducts, 
                MntSweetProducts, total.purchases) %>% 
  dplyr::filter(MntGoldProds < 290 & MntSweetProducts < 200 & MntMeatProducts < 1300)

modelo.maximal <- glm(ATLEASTONCE ~ ., family = binomial(link = "logit"), data = df.total)
deviance.max <- summary(modelo.maximal)$deviance

# bondad de ajuste
hoslem.test(df.total$ATLEASTONCE, fitted(modelo.maximal))

## Residuos del modelo
deviance.std <- rstandard(modelo.maximal, type="deviance")
cuantil.res <- qresid(modelo.maximal)
prob_estim <- fitted(modelo.maximal)
plot(prob_estim, modelo.maximal)

#### Grafico probabilistico normal para los residuos cuantil
qqnorm(cuantil.res, 
       main = "prob.normal",
       xlab = "Cuantiles teor.", 
       ylab = "Cuantiles de res.",
       plot.it = TRUE, 
       datax = FALSE)
qqline(cuantil.res)

# Capacidad predictiva
Curva.ROC <- roc(df.total$ATLEASTONCE, fitted(modelo.maximal))
plot(Curva.ROC, lwd=2,
     main="Curva ROC",
     xlab="1-Especificidad",
     ylab="Sensibilidad")
legend("bottomright",
       legend=paste("AUC=",round(auc(Curva.ROC),2)),
       box.lty=0)
AUC_pROC <- auc(Curva.ROC)
print(AUC_pROC)

# Parametros
library(sjPlot)
plot_model(modelo.2)

# Vamos a estudiar cuanto aporta cada variable al modelo a traves de 
# un shrinkage

### ML Shrinkage ####
install.packages("bestglm")
library(bestglm)
library(caret)
library(pROC)
library(glmnet)

set.seed(123)

p.entr <- 0.8

indices_entrenamiento <- sample(1:nrow(df.total), 
                                floor(p.entr * nrow(df.total)))

df.total$Education <- as.factor(df.total$Education)
df.total$Marital_Status <- as.factor(df.total$Marital_Status)

# Crear conjuntos de entrenamiento y prueba
samp <- sample(1:nrow(df.total), round(0.8*nrow(df.total),0))
train <- df.total[samp, ]
test <- df.total[-samp, ]

y <- train[, "ATLEASTONCE"]
X <- train[, -ATLEASTONCE]
Xy <- as.data.frame(cbind(X,y))

best.logit_AIC <- bestglm(Xy, IC = "AIC", family = binomial)
