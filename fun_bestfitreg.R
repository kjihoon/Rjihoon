

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


