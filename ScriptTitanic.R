library(knitr)
library(VIM)
library(car)
library(C50)
library(e1071)
library(rpart)
library(rpart.plot)





# Se cargan los datos del conjunto de entrenamiento
entrenamiento <- read.csv("/Users/miguelpr93/Desktop/titanic/train.csv", header = TRUE)

# Se cargan los datos del conjunto de test
test <- read.csv("/Users/miguelpr93/Desktop/titanic/test.csv", header = TRUE)

# Se cargan los datos del conjunto de test
validacion <- read.csv("/Users/miguelpr93/Desktop/titanic/gender_submission.csv", header = TRUE)

# Con los siguientes registros se comprueba la correcta carga de los datos mostrando un ejemplo de los primeros seis registros
head(entrenamiento)
head(test)
head(validacion)

# Seleccion de campos
entrenamientoAnalisis <- entrenamiento[c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)]
head(entrenamientoAnalisis)

# Seleccion de campos
testAnalisis <- test[c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)]
head(testAnalisis)


# Observar el tipo de dato que asigna a cada variable R por defecto.
tipoEntrenamiento <- sapply(entrenamientoAnalisis,class) 
kable(data.frame(variables=names(tipoEntrenamiento),clase=as.vector(tipoEntrenamiento)))

tipoTest <- sapply(testAnalisis,class) 
kable(data.frame(variables=names(tipoTest),clase=as.vector(tipoTest)))

tipoValidacion <- sapply(validacion,class) 
kable(data.frame(variables=names(tipoValidacion),clase=as.vector(tipoValidacion)))

# Se cambia el tipo de dato para que se interprete como variable categorica
entrenamientoAnalisis$Survived <- as.factor(entrenamientoAnalisis$Survived)
entrenamientoAnalisis$Pclass <- as.factor(entrenamientoAnalisis$Pclass)
testAnalisis$Pclass <- as.factor(testAnalisis$Pclass)
validacion$Survived <- as.factor(validacion$Survived)


# Se comprueba el correcto cambio en los tipos de datos
tipoEntrenamiento <- sapply(entrenamientoAnalisis,class) 
kable(data.frame(variables=names(tipoEntrenamiento),clase=as.vector(tipoEntrenamiento)))

tipoTest <- sapply(testAnalisis,class) 
kable(data.frame(variables=names(tipoTest),clase=as.vector(tipoTest)))

tipoValidacion <- sapply(validacion,class) 
kable(data.frame(variables=names(tipoValidacion),clase=as.vector(tipoValidacion)))



# Se comprueba los valores que tiene para conjunto de datos como 0 o vacios
sapply(entrenamientoAnalisis, function(x) sum(is.na(x)))
sapply(entrenamientoAnalisis, function(x) sum(trimws(x) == ""))
sapply(entrenamientoAnalisis, function(x) sum(x == 0))

sapply(testAnalisis, function(x) sum(is.na(x)))
sapply(testAnalisis, function(x) sum(trimws(x) == ""))
sapply(testAnalisis, function(x) sum(x == 0))

sapply(validacion, function(x) sum(is.na(x)))
sapply(validacion, function(x) sum(trimws(x) == ""))
sapply(validacion, function(x) sum(x == 0))



# Correccion de los ceros un elementos vacios
entrenamientoAnalisis$Age <- kNN(entrenamientoAnalisis)$Age
testAnalisis$Age <- kNN(testAnalisis)$Age
testAnalisis$Fare <- kNN(testAnalisis)$Fare

# Creacion de una valor nuevo para el valor para Embarked
levels(entrenamientoAnalisis$Embarked) <- c(levels(entrenamientoAnalisis$Embarked), "NA")
levels(testAnalisis$Embarked) <- c(levels(testAnalisis$Embarked), "NA")

# Asignacion del valor NA en vez del valor vacio
entrenamientoAnalisis$Embarked[entrenamientoAnalisis$Embarked == ""] <- 'NA'

# Diagrama de cajas para cada variable de tipo numeric o integer
# Para el conjunto de datos de entrenamiento
boxplot(entrenamientoAnalisis$Age)
boxplot(entrenamientoAnalisis$SibSp)
boxplot(entrenamientoAnalisis$Parch)
boxplot(entrenamientoAnalisis$Fare)
# Para el conjunto de datos de test
boxplot(testAnalisis$Age)
boxplot(testAnalisis$SibSp)
boxplot(testAnalisis$Parch)
boxplot(testAnalisis$Fare)

# Distribucion variable
# Para el conjunto de datos de entrenamiento
hist(entrenamientoAnalisis$Age)
hist(entrenamientoAnalisis$SibSp)
hist(entrenamientoAnalisis$Parch)
hist(entrenamientoAnalisis$Fare)
# Para el conjunto de datos de test
hist(testAnalisis$Age)
hist(testAnalisis$SibSp)
hist(testAnalisis$Parch)
hist(testAnalisis$Fare)



nrow(entrenamientoAnalisis) 
entrenamientoAnalisis<-entrenamientoAnalisis[remove_outliers(entrenamientoAnalisis$Fare,1)==FALSE,]
nrow(entrenamientoAnalisis) 

# Comprobacion de la normalidad de los datos
# Para el conjunto de datos de entrenamiento
shapiro.test(entrenamientoAnalisis$Age)
shapiro.test(entrenamientoAnalisis$SibSp)
shapiro.test(entrenamientoAnalisis$Parch)
shapiro.test(entrenamientoAnalisis$Fare)
# Para el conjunto de datos de test
shapiro.test(testAnalisis$Age)
shapiro.test(testAnalisis$SibSp)
shapiro.test(testAnalisis$Parch)
shapiro.test(testAnalisis$Fare)

# Comprobacion grafica de la normalidad
qqnorm(entrenamientoAnalisis$Age, pch = 1, frame = FALSE)
qqline(entrenamientoAnalisis$Age, col = "steelblue", lwd = 2)

qqnorm(entrenamientoAnalisis$SibSp, pch = 1, frame = FALSE)
qqline(entrenamientoAnalisis$SibSp, col = "steelblue", lwd = 2)

qqnorm(entrenamientoAnalisis$Parch, pch = 1, frame = FALSE)
qqline(entrenamientoAnalisis$Parch, col = "steelblue", lwd = 2)

qqnorm(entrenamientoAnalisis$Fare, pch = 1, frame = FALSE)
qqline(entrenamientoAnalisis$Fare, col = "steelblue", lwd = 2)

qqnorm(testAnalisis$Age, pch = 1, frame = FALSE)
qqline(testAnalisis$Age, col = "steelblue", lwd = 2)

qqnorm(testAnalisis$SibSp, pch = 1, frame = FALSE)
qqline(testAnalisis$SibSp, col = "steelblue", lwd = 2)

qqnorm(testAnalisis$Parch, pch = 1, frame = FALSE)
qqline(testAnalisis$Parch, col = "steelblue", lwd = 2)

qqnorm(testAnalisis$Fare, pch = 1, frame = FALSE)
qqline(testAnalisis$Fare, col = "steelblue", lwd = 2)


# Test de Levene para la homogeneidad de varianza
leveneTest(y = entrenamientoAnalisis$Age, group = entrenamientoAnalisis$Survived, center = "median")
leveneTest(y = entrenamientoAnalisis$SibSp, group = entrenamientoAnalisis$Survived, center = "median")
leveneTest(y = entrenamientoAnalisis$Parch, group = entrenamientoAnalisis$Survived, center = "median")
leveneTest(y = entrenamientoAnalisis$Fare, group = entrenamientoAnalisis$Survived, center = "median")


# Funcion para la normalizacion de los datos
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalizacion de los datos
# Para el conjunto de datos de entrenamiento
entrenamientoAnalisis$Age <- normalize(entrenamientoAnalisis$Age)
entrenamientoAnalisis$SibSp <- normalize(entrenamientoAnalisis$SibSp)
entrenamientoAnalisis$Parch <- normalize(entrenamientoAnalisis$Parch)
entrenamientoAnalisis$Fare <- normalize(entrenamientoAnalisis$Fare)
# Para el conjunto de datos de test
testAnalisis$Age <- normalize(testAnalisis$Age)
testAnalisis$SibSp <- normalize(testAnalisis$SibSp)
testAnalisis$Parch <- normalize(testAnalisis$Parch)
testAnalisis$Fare <- normalize(testAnalisis$Fare)



# Arbol de decision

# Se establece una semilla para la generacion de numeros aleatorios
set.seed(1234)

# Construccion del modelo
clasifcadorAD <-rpart(Survived ~ ., data = entrenamientoAnalisis)

# Prediccion de la clasificacion en el conjunto de test
prediccionAD <- predict(clasifcadorAD, newdata = testAnalisis, type = 'class')

# Analisis de la clasificacion
exitoPrediccionAD <- sum(prediccionAD == validacion$Survived)/length(prediccionAD)
exitoPrediccionAD
tablePrediccionAD <- table(prediccionAD, validacion$Survived)
tablePrediccionAD
fourfoldplot(tablePrediccionAD, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Matriz de confusion")

# Grafico arbol decision
rpart.plot(clasifcadorAD)


# Regresion logistica

# Construccion del modelo
clasificadorRL <- glm(Survived ~ ., family = binomial, data = entrenamientoAnalisis)

# Prediccion de la clasificacion en el conjunto de test
prediccionRL <- predict(clasificadorRL, type = 'response', newdata = testAnalisis)
prediccionRL <- ifelse(prediccionRL > 0.5, 1, 0)
prediccionRL <- as.factor(prediccionRL)

# Analisis de la clasificacion
exitoPrediccionRL <- sum(prediccionRL == validacion$Survived)/length(prediccionRL)
exitoPrediccionRL
tablePrediccionRL <- table(prediccionRL, validacion$Survived)
tablePrediccionRL
fourfoldplot(tablePrediccionRL, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Matriz de confusion")


# Clasificador Bayesiano Ingenuo 

# Se establece una semilla para la generacion de numeros aleatorios
set.seed(1234)

# Construccion del modelo
clasificadorBayes <- naiveBayes(Survived ~ ., data = entrenamientoAnalisis)

# Prediccion de la clasificacion en el conjunto de test
prediccionBayes <- predict(clasificadorBayes, newdata = testAnalisis)

# Analisis de la clasificacion
exitoPrediccionBayes <- sum(prediccionBayes == validacion$Survived)/length(prediccionBayes)
exitoPrediccionBayes
tablePrediccionBayes <- table(prediccionBayes, validacion$Survived)
tablePrediccionBayes
fourfoldplot(tablePrediccionBayes, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Matriz de confusion")

# Exportacion de los datos
# Se incluye la prediccion generada por el arbol de decision
testAnalisis$Survived <- prediccionAD
# Se almacenan los datos en csv
write.csv(entrenamientoAnalisis, "/Users/miguelpr93/Desktop/titanic/trainExport.csv", row.names = FALSE)
write.csv(testAnalisis, "/Users/miguelpr93/Desktop/titanic/testExport.csv", row.names = FALSE)
write.csv(validacion, "/Users/miguelpr93/Desktop/titanic/validacionExport.csv", row.names = FALSE)
