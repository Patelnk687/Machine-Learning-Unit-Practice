library(readr)
quality <- read_csv("C:/Users/NP/Desktop/SPRINGBOARD/quality.csv")
View(quality)
str(quality)
table(quality$PoorCare)
install.packages("caTools")
library("caTools")
set.seed(88)
split<- sample.split(quality$PoorCare, SplitRatio = 0.75)
split
qualityTrain <- subset(quality, split==TRUE)
qualityTest <- subset(quality, split==FALSE)
nrow(qualityTest)
nrow(qualityTrain)
QualityLog<-glm(PoorCare ~ OfficeVisits+Narcotics, data= qualityTrain, family= binomial)
summary(QualityLog)


predictTrain<-predict(QualityLog,type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

table(qualityTrain$PoorCare,predictTrain>0.5)

10/25
70/74
table(qualityTrain$PoorCare,predictTrain>0.7)
table(qualityTrain$PoorCare,predictTrain>0.2)
install.packages("ROCR")
library("ROCR")
ROCRpred<-prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf<- performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
