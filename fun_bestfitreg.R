<<<<<<< HEAD


##Compare Model
fun_regcompare<-function(formula1,formula2){
  model1<-lm(formula = formula1)
  model2<-lm(formula = formula2)
  result<-list()
  
  result[['aov']]<-capture.output(anova(model1,model2))
  return(result)
}

#RMSE
#r2<-c(summary(model1)[[8]],summary(model2)[[8]])

#rmse1<-sqrt(sum(residuals(model1)^2)/model1$df.residual)
#rmse2<-sqrt(sum(residuals(model2)^2)/model2$df.residual)
#rmse<-c(rmse1,rmse2)
#df2<-data.frame(result=c(r2,rmse),Type=c('R-squred','R-squred','RMSE','RMSE'),Model=c('model1','model2','model1','model2'))

#ggplot(data=df2, aes( y=result,fill=Model)) +
#  geom_bar(stat="identity", position=position_dodge())+facet_grid(.~Type)



# Stepwise Regression
fun_regstepaic<-function(formula,direction){
  library(MASS)
  result<-list()
  step <- stepAIC(object=lm(formula), direction = direction)
  result[['step']]<-capture.output(stepAIC(object=lm(formula), direction = direction))[1:which(capture.output(stepAIC(object=lm(formula), direction = direction))=="Call:")-1]
  result[['aov']]<-capture.output(step$anova)
  result[['summary']]<-capture.output(step)
  return(result)
}


=======


##Compare Model
fun_regcompare<-function(formula1,formula2){
  model1<-lm(formula = formula1)
  model2<-lm(formula = formula2)
  result<-list()
  result[['aov']]<-capture.output(anova(model1,model2))
  return(result)
}

# Stepwise Regression
fun_regstepaic<-function(formula,direction){
  library(MASS)
  result<-list()
  step <- stepAIC(object=lm(formula), direction = direction)
  result[['step']]<-capture.output(stepAIC(object=lm(formula), direction = direction))[1:which(capture.output(stepAIC(object=lm(formula), direction = direction))=="Call:")-1]
  result[['aov']]<-capture.output(step$anova)
  result[['summary']]<-capture.output(step)
  return(result)
}


>>>>>>> d0df0a5e5ac586d86a4e86a31c6091184bb9354d
