This project is adapted from Harvard University's statistical software workshop.

http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html

These statistical software workshop materials by Harvard University are licensed 
under a Creative Commons Attribution-ShareAlike 4.0 International License.



Linear regression practice
format the linear model
model1<- lm(xvariable~ yvariable1+ yvariable2+ yvariable3+... yvariablen)

find the summary
 summary(model1)
# look at the r^2 value and gthe adjusted r^2, these need to be cloer to 1 then the model is good.

find the coefficient
coef(summary(model1))
#gives the estimates, standard error and t values of the variables in the model