install.packages('caTools')
install.packages('ROCR')

library(caTools)
library(ROCR)

set.seed(88)
split <- sample.split(encuestas$IdNivelEducativo, SplitRatio = 0.75)

#get training and test data
dresstrain <- subset(encuestas, split == TRUE)
dresstest <- subset(encuestas, split == FALSE)


#logistic regression model
model <- glm (IdNivelEducativo ~ IdTipoDeEmpresa + IdProvincia, data = encuestas, family = binomial)
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(dresstrain$IdNivelEducativo, predict > 0.5)

#ROCR Curve
ROCRpred <- prediction(predict, dresstrain$IdNivelEducativo)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))