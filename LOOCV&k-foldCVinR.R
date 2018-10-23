install.packages("ISLR")
install.packages('boot')
library("ISLR")
library("boot")
Auto
attach(Auto)

set.seed(1)


#fit a linear model
model= glm(mpg~horsepower, data=Auto)
MSE_LOOCV<- cv.glm(Auto,model)
MSE_LOOCV
MSE_LOOCVdelta<-cv.glm(Auto,model)$delta[1]
MSE_LOOCVdelta


#compare up to 10 polynomial
MSE_LOOCV=NULL

for(i in 1:10){
  model = glm(mpg~poly(horsepower,i), data = Auto)
  MSE_LOOCV[i]<- cv.glm(Auto,model)$delta[1]
  
}
MSE_LOOCV




###K-FOLD CROSS VALIDATION

MSE_10_fold_cv=NULL

for(i in 1:10){
  model = glm(mpg~poly(horsepower,i), data = Auto)
  MSE_10_fold_cv[i]<- cv.glm(Auto,model, K=10)$delta[1]
  
}
MSE_10_fold_cv
