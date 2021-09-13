
# ------------------------------------------------------------------
# CARGAMOS LOS DATOS
# ------------------------------------------------------------------

# Preparamos el espacio de trabajo y reseteamos objetos cargados
getwd()
setwd("E:/FID/rstudio/")
getwd()
ls()
rm(list = ls()) 
ls()

# Cargamos y limpiamos nulos
marks <- read.csv("./data/marks.csv", sep=",", head = TRUE)
marks[is.na(marks)] <- 0

# Comprobamos dataset
head(marks)
dim(marks)
str(marks)

# ------------------------------------------------------------------
# ANÁLISIS EXPLORATORIO
# ------------------------------------------------------------------

# Comprobar si hay alguna fila incompleta
any(!complete.cases(marks))

# Datos ausentes por variable
map_dbl(datos, .f = function(x){sum(is.na(x))})

# Eliminamos la columnas Ids
marks$idStudent <- NULL


# 

marks %>%
  group_by(variable) %>% 
  summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>%
  ggplot(aes(x = reorder(variable, desc(porcentaje_NA)), y = porcentaje_NA)) +
  geom_col() +
  labs(title = "Porcentaje valores ausentes por variable",
       x = "Variable", y = "Porcentaje NAs") +
  theme_bw()


# ------------------------------------------------------------------
# VISUALIZACIÓN DEL DATASET
# ------------------------------------------------------------------

# Correlación
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)

filter_marks %>%
  filter(Flow == "SI") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

# Densidad
filter_marks %>% 
  select(Professor,T1, T2, T3,T4, Total_T, Total_D, Deliveries, Extras, Total_TED, Marks, Flow) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

# TODO: No termina de visualizar adecuadamente. Engorrosa.
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
featurePlot(x = filter_marks[, 3:6], 
            y = filter_marks$Flow, 
            plot = "pairs",
            auto.key = list(columns = 2))


# Notas medias de trabajos en grupos . Diagrama Polar
data <- filter_marks
data_group <- data %>% group_by(idGroup) %>%
  summarize(group_mark = mean(Deliveries + Extras), )

ggplot(data_group, aes(x=as.factor(idGroup), y=group_mark)) + 
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-3,8) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  ) +
  coord_polar(start = 0)


# Notas finales por alumnos y grupos. Diagrama Polar
data <- marks %>% select(idStudent, idGroup, Marks) %>% rename(individual=idStudent, group=idGroup, value=Marks)
data = data %>% arrange(group, value)   
                           
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
   
ggplot(data, aes(x=as.factor(id), y=value, fill=as.factor(group))) +
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-3,10) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )


# Densidad calificaciones
data <- filter_marks
ggplot(data, aes(x = Marks, fill = 'Alumnos')) + 
  geom_histogram(bins = 50, aes(y = ..density..)) + 
  geom_density(alpha = 0.3) + 
  ggtitle("Densidad en calificaciones") + theme_bw()


# Calificaciones finales por alumno proactivo
ggplot(data, aes(Marks)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=Flow)) + xlab("Calificaciones") + 
  ylab("Alumnos proactivos") + 
  ggtitle("Calificaciones por alumnos proactivos")

ggplot(data, aes(Flow, Marks, color = Flow )) + 
  geom_boxplot() + 
  ggtitle("Calificaciones por alumnos proactivos") + 
  xlab("Alumnos proactivos") + 
  ylab("Calificaciones")


# Calificaciones por Profesor
ggplot(data) +
  geom_bin2d(aes(x=Marks, y=Professor)) +
  xlab("CALIFICACIONES") + ylab("PROFESORES") + 
  ggtitle("Calificaciones por Profesor")


# Calificaciones por Grupo
ggplot(data, aes(x = reorder(idGroup, Marks, FUN = median), y = Marks, col = idGroup)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_bw()

ggplot(data, aes(x=Professor, y=Marks)) +
  geom_segment( aes(x=Professor, xend=Professor, y=0, yend=Marks), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Profesor") +
  ylab("Calificación") + 
  ggtitle("Calificaciones por profesor")


# Calificaciones por Profesor y Tipo de Alumnos
ggplot(data = data)+
  geom_bar(mapping = aes(x = Flow,Marks,fill = factor(Professor)),stat = 'Identity',position = 'dodge') + 
  ggtitle("Calificaciones por Profesor y Tipo") + 
  xlab("Alumnos proactivos") + 
  ylab("Calificaciones")


# Calificaciones por Grupos
# Agrupamos por grupo y sumamos la nota de los trabajos en grupo
data_group <- data %>% group_by(idGroup, Professor) %>%
  summarize(group_mark = mean(Deliveries + Extras))

# Notas medias de todas los items evaluables. Spider chart
install.packages("fmsb")
library("fmsb")

data[1,] = c(0.0)
data[2,] = c(1.5)
data <- marks %>% 
  select(T1, T2, T3, T4, D2, D3, D4, D5) %>% 
  rename(Code=T1, Github=T2, Sonar=T3, iTop=T4, Delivery2=D2, Delivery3=D3, Delivery4=D4, Delivery5=D5) %>%
  summarise_all(list(mean))

data <- rbind(rep(1.5,8) , rep(0.0,8) , data)

radarchart( data, axistype=1, 
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1.6,0.4), cglwd=0.8,
            vlcex=0.8 
) 



# ---------------------------------------------------------------------------------------------
# PRUEBA 1: VAMOS A PREDECIR LOS VALORES DE LA CLASE PROACTIVE
# Clasificación binaria
# ---------------------------------------------------------------------------------------------


# Preprocesamos el dataset. Limpiamos valores nulos y filtramos columnas no necesarias
regVar <- c("idGroup","Professor","T1", "T2", "T3","T4", "Total_T", "Total_D", "Deliveries", "Extras", "Total_TED", "Marks", "Flow")
marks[is.na(marks)] <- 0
filter_marks <- marks[, regVar]
str(filter_marks)

  
# Separamos datos de training y testesting con el paquete CARET. 
# No seleccionamos datos para validar el modelo, utilizaremos validación cruzada.
# Como el dataset es pequeño podemos usar LOOCV sin problemas.
library(caret)
set.seed(825)


# Separamos los grupos de datos en base a importancia de grupos, con groupsKFold
inTraining <- createDataPartition(filter_marks$Flow, p = .80, list = FALSE)
training <- filter_marks[inTraining,]
testing <- filter_marks[-inTraining,]


# Los alumnos son evaluados por profesor y nos puede interesar agruparlos.
# 4 Folds grouped by Proffesor cross validation, repeated 3 times
group_folds <- groupKFold(training$Professor, k = 4)


# Preparamos la clase de control. 
# Activamos que nos devuelva las probabilidades para analisis ROC
# Realizamos una validación cruzada LOOCV 
# Sustituimos para poder usar Resample por CV, 10-fold cross-validation
# Añadimos las muestras
myControl_clas <- trainControl(
  index = group_folds,
  method = "CV",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = TRUE,
  verbose = FALSE
)


# Probamos con un primer algoritmo
model_class_glm <- train(Flow~ ., training,
                        method="glm",
                        trControl=myControl_clas)

print(model_class_glm)

# Probamos con un segundo algoritmo
# Esta técnica es muy útil para conjunto de datos con un elevado número de variables predictoras y pocos valores
model_class_glmnet <- train(Flow~ ., training,
                            method = "glmnet",
                            trControl = myControl_clas
)

print(model_class_glmnet)
plot(model_class_glmnet)

# Probamos con un tercer algoritmo
model_class_xgbTree <- train(Flow~ ., training,
                            method = "xgbTree",
                            trControl=myControl_clas)

print(model_class_xgbTree)
plot(model_class_xgbTree)

# Probamos con un cuarto algoritmo
model_class_nbayes <- train(Flow~ ., training,
                            method = "naive_bayes",
                            trControl=myControl_clas)

# Observamos los parámetros utilizamos por defecto con train
print(model_class_nbayes)
plot(model_class_nbayes)


# Personalizamos los parámetros para que utilice el más optimo
nb_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                         laplace = c(0, 0.5, 1), 
                         adjust = c(0.75, 1, 1.25, 1.5))

model_class_nbayes_tun <- train(Flow~ ., training,
                            method = "naive_bayes",
                            usepoisson = TRUE,
                            tuneGrid = nb_grid,
                            trControl=myControl_clas)

print(model_class_nbayes_tun)
plot(model_class_nbayes_tun)

# Como último paso hacemos una comparativa de modelos con la función resample de Caret
model_list <- list(
  glm = model_class_glm, 
  glmnet = model_class_glmnet,
  xgbTree = model_class_xgbTree,
  nbayes = model_class_nbayes_tun
)

resamps <- resamples(model_list)
# Error: LOOCV is not compatible with `resamples()` since only one resampling estimate is available.
# Pasamos trainControl a CV para poder utilizar la función

summary(resamps, metric="ROC")

# Visualizamos los resultados de la comparativa
bwplot(resamps, metric = "ROC")
dotplot(resamps, metric="ROC")
xyplot(resamps, what = "BlandAltman")
splom(resamps)

# TODO: En las dos últimas apararecen pocas muestras, es extraño. Investigar.

# Como último paso, y en base al mejor modelo realizamos la predicción para ver el rendimiento real
prediction <- predict(model_class_nbayes_tun, testing, type = "prob")
summary(prediction)

probs <- prediction[,2]
probs

install.packages("ROCR")
library(ROCR)

# Make a prediction object: pred
pred <- prediction(probs, testing$Flow)

# Make a performance object: perf
perf <- performance(pred, "tpr", "fpr")

#Plot this curve.Buen resultado, objetivo siempre minimizar FPR, maximizar TPR
plot(perf)

# Valor de AUC o área bajo la curva
perf_auc <- performance(pred, "auc")
print(perf_auc@y.values[[1]])

# Exploramos la densidad
trellis.par.set(caretTheme())
densityplot(model_class_nbayes_tun, pch = "|")


# TODO: Visualizamos que tal bueno ha sido con la matriz de confusión

cm <- confusionMatrix(model_class_nbayes_tun)


draw_confusion_matrix <- function(cm) {
  
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix(cm)



# -------------------------------------------------------------------------
# PRUEBA 2: Predicción de la nota final en base a la nota parcial de iTop
# Regresión lineal simple
# -------------------------------------------------------------------------

filter_marks <- marks[,c(7,28), drop=FALSE]

colnames(filter_marks) <- c('Nota iTop', 'Nota final')
colnames(filter_marks)
plot(filter_marks)

# Se observa una tendencia lineal
lm_filter_marks <- lm(filter_marks$Marks ~ ., data=filter_marks)
lm_filter_marks$coefficients

final_marks_new <- filter_marks[1,]
predict(lm_filter_marks, final_marks_new)

abline(lm_filter_marks$coefficients, col = "red")


# Calcular el rendimiento RMSE (Raíz cuadrada de la suma final)
final_marks_est <- predict(lm_filter_marks)
res <- filter_marks$Marks - final_marks_est
rmse <- sqrt(mean(res^2))
print(rmse)

# Calcular rendimiento R^2. Apreciamos que el rendimiento es malo, R^2 muy cercano a cero.
summary(lm_filter_marks)$r.squared



# ------------------------------------------------------------------------
# PRUEBA 3: VAMOS A PREDECIR LOS VALORES DE T4 en BASE A T1, T2, T3
# Regresión multilineal
# ------------------------------------------------------------------------

# Filtramos el dataset con las columnas que nos interesan para optimizar los cálculos
filter_marks <- marks[,(4:7), drop=FALSE]

# Visualizamos si hay tendencia lineal
plot(filter_marks)


# Visualizamos y vemos si hay una tendencia lineal. Si es así aplicamos lm, lineal model.
# Si no hay tendencia lineal, o no se puede visualizar bien porque tuviera muchas variables,
# lo que se suele hacer es aplicar un lm y otro, por ejemplo un paramétrico, 
# y luego se ve cuales de los dos tiene mejor métrica, cual generaliza mejor.

plot(T4 ~ T1, filter_marks)
plot(T4 ~ T2, filter_marks)
plot(T4 ~ T3, filter_marks)

# Observamos que no siguen una tendencia lineal visible en ninguna de ellas. 
# En este punto habría que realizar una transformación para dar linealidad o aplicar algoritmos no paramétricos

lm_filter_marks <- lm(T4 ~ ., data=filter_marks)
summary(lm_filter_marks)$r.squared

# Observamos que el valor está muy cerca de 0, por lo que nuestro modelo es muy malo.

# Para que el modelo sea bueno NO se debe observar ningún patrón 
# en los residuos frente al real (estimados frente a real)
plot(lm_filter_marks$fitted.values, lm_filter_marks$residuals,
     xlab = "Fitted values", ylab = "Residuals")

# Para que el modelo sea bueno SI se debe observar patrón en los residuos 
qqnorm(lm_filter_marks$residuals, ylab = "Residual Quantiles")

# TODO: Es raro los resultados de los residuos, si el modelo era tan malo debería tener resultados diferentes. Investigarlo.

# Vamos a usar el paquete CARET para obtener un modelo no paramétrico, como KNN o M5P
library(caret)

# En este caso al no ser predicción binaria no solicitamos ROC
myControl_clas <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = multiClassSummary,
  classProbs = TRUE, # IMPORTANT!
  savePredictions = TRUE,
  verbose = FALSE
)


# Eliminamos filas con valores NA porque da error
filter_marks_del <- filter_marks[-c(186:188), ]
filter_marks_del <- filter_marks_del[-c(63), ]

# nos pide el paquete e1071
install.packages("e1071")
library(e1071)

model_clas_glm <- train(T4 ~ ., filter_marks_del, method="bayesglm", trControl=myControl_clas)

# se produce un error porque el valor a predecir debe ser binario: glm models can only use 2-class outcomes
print(model_clas_glm)


# Transformamos en Aprobado/Suspenso en función de la nota de T4
filter_marks_del$T4 <- factor(filter_marks_del$T4 > 0.25, labels = c("Aprobado", "suspenso"))

# Calculamos si están balanceados. No lo están mucho.
table(filter_marks_del$T4)


# Vamos a visualizar algunos datos

install.packages("mlbench")
library(mlbench)
library(caret)
regVar <- c("T1", "T2", "T3")
str(filter_marks_del[, regVar])

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

featurePlot(x = filter_marks[, regVar], 
            y = filter_marks$T4, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))

# No me termina de visualizar bien todos los datos. El problema es la carga de los datos, si hay nulos falla y deja de pintar,
# Si cargamos como read.csv en vez de read.csv2, marca los nulos como NA y si los muestra bien.


#----------------------------------------------
# PRUEBA 4, HACEMOS LO MISMO DE DIFERENTE FORMA
#-----------------------------------------------

# Analizamos previamente la relación de variables para comprobar la linealidad entre ellas
install.packages("psych")
install.packages("GGally")
library(psych)
library(GGally)

# Seleccionamos los datos
filter_marks <- marks  %>%  select(T1, T2, T3,T4)

# Distribución de cada variable mediante histogramas y correlación
multi.hist(x = filter_marks, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

ggpairs(filter_marks, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


# Generamos el modelo
model_ln <- lm(T4 ~ ., data = filter_marks )
summary(model_ln)

# R^2 = 0.056, mal modelo


# Vamos a analizar ahora si podemos predecir los resultados finales en base a T1,T2,T3,T4
filter_marks <- marks  %>%  select(T1, T2, T3,T4, Marks)
multi.hist(x = filter_marks, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

ggpairs(filter_marks, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

model_ln <- lm(Marks ~ ., data = filter_marks )
summary(model_ln)

# R^2 = 0.5785, MEJOR QUE ANTES.



# ------------------------------------------------------------------------
# PRUEBA 5: APLICAMOS REGRESIÓN NO PARAMÉTRICA con Knn
# ------------------------------------------------------------------------
library(caret)

particiones  <- 10
repeticiones <- 5
hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)


set.seed(342)

# Caso 1, predecir T4
filter_marks <- marks  %>%  select(T1, T2, T3,T4)
knn_model <- train(T4 ~ ., data = filter_marks,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "RMSE",
                    trControl = control_train)
knn_model
plot(knn_model, type = 'l', lwd = 2)


# Caso 2, predecir Marks
filter_marks <- marks  %>%  select(T1, T2, T3, T4, Marks)
knn_model <- train(Marks ~ ., data = filter_marks,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "RMSE",
                    trControl = control_train)

knn_model
plot(knn_model, type = 'l', lwd = 2)


#pred <- predict(knn_model, training)
#RMSE(pred, filter_marks$Marks)


# ------------------------------------------------------------------------
# PRUEBA 3: ANALISIS NO SUPERVISADO
# Clustering
# ------------------------------------------------------------------------

# Cargamos la librerías que usaremos en esta sección
library(tidyverse)
library(caret)

# Cargamos los datos completos por defecto
marks <- read.csv("./data/marks-v1.0.csv", sep=",", head = TRUE)

# Preparamos el dataset. Limpiamos valores nulos y filtramos columnas no necesarias para este problema concreto
regVar <- c("T1", "T2", "T3","T4", "Total_T", "Total_D", "Deliveries", "Extras")
marks[is.na(marks)] <- 0
cluster_marks <- marks[, regVar]

# Fijamos a tres decimales las columnas numéricas
#Cambiar tipo de columnas: cluster_marks <- format(round(cluster_marks, 3), nsmall = 3)
# TODO: El siguiente comando no cambia cuando las decimales son inferiores a 3, como el caso de T1
cluster_marks <- cluster_marks %>% mutate(across(where(is.numeric), round, 3))

# Comprobamos dataset
head(cluster_marks)
dim(cluster_marks)
str(cluster_marks)

# -----------------------------------
# Clustering con KMEANs
# -----------------------------------
  
# Como la magnitud de los valores varía entre variables, las escalamos para normalizarlas.
datos <- scale(cluster_marks)

install.packages("factoextra")
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(datos, method = "euclidean"), nstart = 50) 

# Observamos que con K=4 o 5 se obtienen buenos resultados.

# El paquete factoextra permite obtener visualizaciones de las agrupaciones resultantes.
# Si el número de variables (dimensionalidad) es mayor de 2, automáticamente realiza un PCA y 
# representa las dos primeras componentes principales.

set.seed(123)
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")  


# -----------------------------------------------
# Clustering con K-MEDOIDS (PAM)
# -----------------------------------------------
  
# Este algoritmo es más robusto que Kmeans, sobre todo si hay ruido. Necesita saber de primera mano el nºK
library(cluster)

# En este caso, dado que se sospecha de la presencia de outliers, se emplea la distancia de Manhattan como medida de similitud
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
               diss = dist(datos, method = "manhattan"))

# En este caso obtenemos unos buenos resultados con K=5
set.seed(123)
pam_clusters <- pam(x = datos, k = 5, metric = "manhattan")
pam_clusters

# El objeto devuelto por pam() contiene entre otra información: las observaciones que finalmente se han seleccionado 
# como medoids ($medoids) y el cluster al que se ha asignado cada observación ($clustering).

fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")


# --------------------------------------------
# VALIDAMOS la eficacia de ambos algoritmos
# --------------------------------------------
  
# Utilizamos las funciones eclust() y fviz_silhouette() del paquete factoextra() 
# para de forma sencilla los coeficientes de Silhoutte 
# La función eclust(), con FUNcluster permite, como CARET, aplicar varios algoritmos de clustering


# KMEANS
km_clusters <- eclust(x = datos, FUNcluster = "kmeans", k = 5, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

# Media silhouette por cluster
km_clusters$silinfo$clus.avg.widths

# Coeficiente silhouette para cada observación
head(km_clusters$silinfo$widths)


# PAM

pam_clusters <- eclust(x = datos, FUNcluster = "pam", k = 5, seed = 123,
                      hc_metric = "manhattan", nstart = 50, graph = FALSE)

fviz_silhouette(sil.obj = pam_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic())

# Media silhouette por cluster
pam_clusters$silinfo$clus.avg.widths

# Coeficiente silhouette para cada observación
head(pam_clusters$silinfo$widths)


# COMPARANDO ALGORITMOS CON CLVALID

install.packages("clValid")
library("clValid")
resamps <- clValid(
  obj        = datos,
  nClust     = 2:6,
  clMethods  = c("hierarchical", "kmeans", "pam"),
  validation = c("stability", "internal")
)
summary(resamps)



# Creamos un clúster como en clase
-----------------------------------
km_puntos <- kmeans(cluster_marks, center=3, nstar=20)
km_puntos$cluster
print(km_puntos)
plot(cluster_marks, col=km_puntos$cluster, main="Tres clusters")    


# Vamos a analizar primeramente la dimensionalidad de las variables y si podemos reducirlas con PCA
# TODO: Profundizar en esta parte si da tiempo
head(cluster_marks)
str(cluster_marks)
cluster_marks
par(mar=c(1,1,1,1))
plot(cluster_marks)

iris_pca <- prcomp(cluster_marks[1:10], scale=FALSE, center=TRUE)
iris_pca

summary(iris_pca)
plot(iris_pca)
biplot(iris_pca)

iris_pca_escalado <- prcomp(cluster_marks[1:10], scale=TRUE, center=TRUE)
summary(iris_pca_escalado)
biplot(iris_pca_escalado)


# 
