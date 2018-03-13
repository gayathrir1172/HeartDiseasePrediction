library(readr)
library(class)
library(gmodels)
library(e1071)
'/ Loading dataset and making a copy of it'
framingham <- read_csv("C:/Users/pc/Desktop/HeartDiseasePrediction/framingham.csv")
data <- read_csv("C:/Users/pc/Desktop/HeartDiseasePrediction/framingham.csv")

'/Replacing na values with mean'  

m <- round(mean(data$male, na.rm = TRUE))
data$male[is.na(data$male)] <- m
print(m)  

m <- round(mean(data$age, na.rm = TRUE))
data$age[is.na(data$age)] <- m
print(m)

m <- round(mean(data$education, na.rm = TRUE))
data$education[is.na(data$education)] <- m
print(m)

m <- round(mean(data$currentSmoker, na.rm = TRUE))
data$currentSmoker[is.na(data$currentSmoker)] <- m
print(m)

m <- round(mean(data$cigsPerDay, na.rm = TRUE))
data$cigsPerDay[is.na(data$cigsPerDay)] <- m
print(m)

m <- round(mean(data$BPMeds, na.rm = TRUE))
data$BPMeds[is.na(data$BPMeds)] <- m
print(m)

m <- round(mean(data$prevalentStroke, na.rm = TRUE))
data$prevalentStroke[is.na(data$prevalentStroke)] <- m
print(m)

m <- round(mean(data$prevalentHyp, na.rm = TRUE))
data$prevalentHyp[is.na(data$prevalentHyp)] <- m
print(m)

m <- round(mean(data$diabetes, na.rm = TRUE))
data$diabetes[is.na(data$diabetes)] <- m
print(m)

m <- round(mean(data$totChol, na.rm = TRUE))
data$totChol[is.na(data$totChol)] <- m
print(m)

m <- round(mean(data$sysBP, na.rm = TRUE))
data$sysBP[is.na(data$sysBP)] <- m
print(m)

m <- round(mean(data$diaBP, na.rm = TRUE))
data$diaBP[is.na(data$diaBP)] <- m
print(m)

m <- round(mean(data$BMI, na.rm = TRUE))
data$BMI[is.na(data$BMI)] <- m
print(m)

m <- round(mean(data$heartRate, na.rm = TRUE))
data$heartRate[is.na(data$heartRate)] <- m
print(m)

m <- round(mean(data$glucose, na.rm = TRUE))
data$glucose[is.na(data$glucose)] <- m
print(m)

m <- round(mean(data$TenYearCHD, na.rm = TRUE))
data$TenYearCHD[is.na(data$TenYearCHD)] <- m
print(m)

data$male <- (data$male - min(data$male))/(max(data$male)-min(data$male))
data$age <- (data$age - min(data$age))/(max(data$age)-min(data$age))
data$education <- (data$education - min(data$education))/(max(data$education)-min(data$education))
data$currentSmoker <- (data$currentSmoker - min(data$currentSmoker))/(max(data$currentSmoker)-min(data$currentSmoker))
data$cigsPerDay <- (data$cigsPerDay - min(data$cigsPerDay))/(max(data$cigsPerDay)-min(data$cigsPerDay))
data$BPMeds <- (data$BPMeds - min(data$BPMeds))/(max(data$BPMeds)-min(data$BPMeds))
data$prevalentStroke <- (data$prevalentStroke - min(data$prevalentStroke))/(max(data$prevalentStroke)-min(data$prevalentStroke))
data$prevalentHyp <- (data$prevalentHyp - min(data$prevalentHyp))/(max(data$prevalentHyp)-min(data$prevalentHyp))
data$diabetes <- (data$diabetes - min(data$diabetes))/(max(data$diabetes)-min(data$diabetes))
data$totChol <- (data$totChol - min(data$totChol))/(max(data$totChol)-min(data$totChol))
data$sysBP <- (data$sysBP - min(data$sysBP))/(max(data$sysBP)-min(data$sysBP))
data$diaBP <- (data$diaBP - min(data$diaBP))/(max(data$diaBP)-min(data$diaBP))
data$BMI <- (data$BMI - min(data$BMI))/(max(data$BMI)-min(data$BMI))
data$heartRate <- (data$heartRate - min(data$heartRate))/(max(data$heartRate)-min(data$heartRate))
data$glucose <- (data$glucose - min(data$glucose))/(max(data$glucose)-min(data$glucose))
data$TenYearCHD <- (data$TenYearCHD - min(data$TenYearCHD))/(max(data$TenYearCHD)-min(data$TenYearCHD))

'/Splitting into train and test dataset'
train <- data[1:2756,]
test <- data[2757:4240,]
'/264 wrong predictions before removing highly correlated features'
'/ 258 wrong predictions after removing highly correlated features accuracy 82.61%'
correlationMatrix <- cor(train)
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.5)
print(highlyCorrelated)

train <- train[-4]
train <- train[-7]
train <- train[-9]
train <- train[-12]

levels(train$TenYearCHD)

model <- naiveBayes(as.factor(TenYearCHD)~.,data=train)
class(model)
pred <- predict(model,test[-16])
print(table(pred,test$TenYearCHD))
print(confusionMatrix(table(pred,test$TenYearCHD)))
