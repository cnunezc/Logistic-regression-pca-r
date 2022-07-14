##### Import Red
library(readr)
library(caret)
library(ggplot2)
library(lattice)
#install.packages("InformationValue")
#install.packages("ISLR")
library(InformationValue)
library(ISLR)

## Red Wine
## Import Data

winequality_red <- read_delim("winequality-red.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_red)


dqualityred <- ifelse(winequality_red$quality >= 7 , 1, 0)


tsulfurred <- winequality_red$`total sulfur dioxide`/100


df_red <- data.frame(rfixedacidity = winequality_red$`fixed acidity`,
                     rvolatilityacidity = winequality_red$`volatile acidity`,
                     rcitricacid = winequality_red$`citric acid`,
                     rresidualsugar = winequality_red$`residual sugar`,
                     rchlorides = winequality_red$chlorides,
                     rfreeso2 = winequality_red$`free sulfur dioxide`,
                     tsulfurred,
                     r_ph = winequality_red$pH,
                     rsulphates = winequality_red$sulphates,
                     ralcohol = winequality_red$alcohol,
                     dqualityred)
## Logit Whole Set

reg_red <- glm( dqualityred ~ rfixedacidity+ 
                  rvolatilityacidity +
                  rcitricacid +
                  rresidualsugar +
                  rchlorides +
                  rfreeso2 +
                  tsulfurred +
                  r_ph +
                  rsulphates +
                  ralcohol, data = df_red, family = binomial)
summary(reg_red)

#install.packages("margins")
library("margins")
mfx_red<- margins_summary(reg_red)

## install.packages("xlsx")
library(xlsx)
write.xlsx(mfx_red, "mfx_red.xlsx")

## Red Prediction

red_predict <- predict(reg_red, df_red, type="response")
red_class <- ifelse(red_predict>0.5, 1, 0)
#install.packages("caret")
library(caret)

confusionMatrix(red_class,dqualityred)

## Sensitivity: The “true positive rate” – the percentage of individuals the model correctly predicted would be good.
sensitivity(red_class,dqualityred)

## Specificity: The “true negative rate” – the percentage of individuals the model correctly predicted would not bad.
specificity(red_class,dqualityred)

## Total misclassification rate: The percentage of total incorrect classifications made by the model.
misClassError(red_class,dqualityred)

## Model Accuracy
mean(red_class == dqualityred)

## PCA Red

df_redPC <- data.frame(rfixedacidity = winequality_red$`fixed acidity`,
                       rvolatilityacidity = winequality_red$`volatile acidity`,
                       rcitricacid = winequality_red$`citric acid`,
                       rresidualsugar = winequality_red$`residual sugar`,
                       rchlorides = winequality_red$chlorides,
                       rfreeso2 = winequality_red$`free sulfur dioxide`,
                       tsulfurred,
                       r_ph = winequality_red$pH,
                       rsulphates = winequality_red$sulphates,
                       ralcohol = winequality_red$alcohol)
redpc <- princomp(df_redPC, cor = TRUE, scores = TRUE)

summary(redpc)
screeplot(redpc, type = "lines")

pc.red <- redpc$sdev^2
pc.redpvar <- pc.red / sum(pc.red)
plot(cumsum(pc.redpvar), type = 'b')
abline (h=0.9)
redpc$loadings[, 1:7]
biplot(redpc, col=c("gray","steelblue"),cex=c(0.5,1.3))
head(redpc$scores)
biplot(redpc, col=c("gray","steelblue"),cex=c(0.8,0.01))
scoresred<-data.frame(redpc$scores)
redqty <- factor(dqualityred)
ggplot(data=scoresred, aes(x=Comp.1, y=Comp.2, label=rownames(scoresred), color=redqty)) +
  geom_text(size=4)

reg_redpc <- glm( dqualityred ~ rfixedacidity + 
                    rcitricacid +
                    rresidualsugar +
                    rchlorides +
                    r_ph +
                    rsulphates +
                    ralcohol, data = df_red, family = binomial)
summary(reg_redpc)

library("margins")
library(xlsx)
mfx_redpc<-margins_summary(reg_redpc)
write.xlsx(mfx_redpc, "mfx_redpc.xlsx")

## Red Prediction After PCA

redpc_predict <- predict(reg_redpc, df_red, type="response")
redpc_class <- ifelse(redpc_predict>0.5, 1, 0)
## install.packages("caret")
library(ggplot2)
library(lattice)

library(InformationValue)
library(ISLR)
library(caret)

confusionMatrix(redpc_class,dqualityred)

## Sensitivity: The “true positive rate” – the percentage of individuals the model correctly predicted would be good.
sensitivity(redpc_class,dqualityred)

## Specificity: The “true negative rate” – the percentage of individuals the model correctly predicted would not bad.
specificity(redpc_class,dqualityred)

## Total misclassification rate: The percentage of total incorrect classifications made by the model.
misClassError(redpc_class,dqualityred)

## Model Accuracy
mean(redpc_class == dqualityred)

##install.packages("stargazer")
library(stargazer)

red_reg <- stargazer(reg_red, reg_redpc , type = "text")
write.xlsx(red_reg, "red_reg.xlsx")


################  White   ################
winequality_white <- read_delim("winequality-white.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_white)

dqualitywhite <- ifelse(winequality_white$quality >= 7 , 1, 0)


tsulfurwhite <- winequality_white$`total sulfur dioxide`/100

## White Regression All Variables
df_white <- data.frame(wfixedacidity = winequality_white$`fixed acidity`,
                       wvolatilityacidity = winequality_white$`volatile acidity`,
                       wcitricacid = winequality_white$`citric acid`,
                       wresidualsugar = winequality_white$`residual sugar`,
                       wchlorides = winequality_white$chlorides,
                       wfreeso2 = winequality_white$`free sulfur dioxide`,
                       tsulfurwhite,
                       w_ph = winequality_white$pH,
                       wsulphates = winequality_white$sulphates,
                       walcohol = winequality_white$alcohol,
                       dqualitywhite)

## White Logit

reg_white <- glm( dqualitywhite ~ wfixedacidity+ 
                    wvolatilityacidity +
                    wcitricacid +
                    wresidualsugar +
                    wchlorides +
                    wfreeso2 +
                    tsulfurwhite +
                    w_ph +
                    wsulphates +
                    walcohol, data = df_white, family = binomial)
summary(reg_white)

## install.packages("margins")
library("margins")
mfx_w<-margins_summary(reg_white)
library(xlsx)
write.xlsx(mfx_w, "mfx_w.xlsx")

## White Prediction

white_predict <- predict(reg_white, df_white, type="response")
white_class <- ifelse(white_predict>0.5, 1, 0)
## install.packages("caret")

confusionMatrix(white_class,dqualitywhite)

## Sensitivity: The “true positive rate” – the percentage of individuals the model correctly predicted would be good.
sensitivity(white_class,dqualitywhite)

## Specificity: The “true negative rate” – the percentage of individuals the model correctly predicted would not bad.
specificity(white_class,dqualitywhite)

## Total misclassification rate: The percentage of total incorrect classifications made by the model.
misClassError(white_class,dqualitywhite)

## Model Accuracy
mean(white_class == dqualitywhite)

## White PCA

df_wPC <- data.frame(wfixedacidity = winequality_white$`fixed acidity`,
                     wvolatilityacidity = winequality_white$`volatile acidity`,
                     wcitricacid = winequality_white$`citric acid`,
                     wresidualsugar = winequality_white$`residual sugar`,
                     wchlorides = winequality_white$chlorides,
                     wfreeso2 = winequality_white$`free sulfur dioxide`,
                     tsulfurwhite,
                     w_ph = winequality_white$pH,
                     wsulphates = winequality_white$sulphates,
                     walcohol = winequality_white$alcohol)

reg_wpc <- glm( dqualitywhite ~ wfixedacidity + 
                  wcitricacid +
                  wresidualsugar +
                  wchlorides +
                  wfreeso2 +
                  tsulfurwhite +
                  wsulphates, data = df_white, family = binomial)
summary(reg_wpc)
mfx_wpc<-margins_summary(reg_wpc)
library(xlsx)
write.xlsx(mfx_wpc, "mfx_wpc.xlsx")

## White PCA Prediction

wpc_predict <- predict(reg_wpc, df_white, type="response")
wpc_class <- ifelse(wpc_predict>0.5, 1, 0)
## install.packages("caret")

confusionMatrix(wpc_class,dqualitywhite)

## Sensitivity: The “true positive rate” – the percentage of individuals the model correctly predicted would be good.
sensitivity(wpc_class,dqualitywhite)

## Specificity: The “true negative rate” – the percentage of individuals the model correctly predicted would not bad.
specificity(wpc_class,dqualitywhite)

## Total misclassification rate: The percentage of total incorrect classifications made by the model.
misClassError(wpc_class,dqualitywhite)

## Model Accuracy
mean(wpc_class == dqualitywhite)

wpc <- princomp(df_wPC, cor = TRUE, scores = TRUE)
summary(wpc)
screeplot(wpc, type = "lines")

pc.w <- wpc$sdev^2
pc.wpvar <- pc.w / sum(pc.w)
plot(cumsum(pc.wpvar), type = 'b')
abline (h=0.9)
wpc$loadings[, 1:7]
biplot(wpc, col=c("gray","steelblue"),cex=c(0.5,1.3))
head(wpc$scores)
biplot(wpc, col=c("gray","steelblue"),cex=c(0.8,0.01))
scoresw<-data.frame(wpc$scores)
wqty <- factor(dqualitywhite)
ggplot(data=scoresw, aes(x=Comp.1, y=Comp.2, label=rownames(scoresw), color=wqty)) +
  geom_text(size=4)

write.xlsx(red_w, "red_w.xlsx")