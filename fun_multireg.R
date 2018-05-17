
#test set

#path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/" 
path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/mv/img/"



fun_plot<-function(p,width = 800, height = 800,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}


## return plot =>> reg_psy
## return result =>> summary,aov
fun_multireg<-function(formula,plot=T,group=NULL,clientid="admin"){
  clientid<<-clientid
  model<-lm(formula = formula)
  df<-model$model
  yvar<- names(df)[1] #y variable name
  xvar<- names(df)[2:length(df)] #x variable name
  library(ggplot2)
  library(psych)
  library(car)
  result<-list()
  
  if (plot !=F){
    type<-c()
    for (i in 1:length(df)) type<-append(type,class(df[,i]))
    #psych
    fun_plot({p<-pairs.panels(df[,type!="factor"], cex =1.3,
                             method = "pearson",hist.col = "red",
                             density = TRUE,
                             ellipses = TRUE # show correlation ellipses
    )},plotname = "reg_psy.png")
    
    output<-capture.output(summary(model))
    result[['summary']]<-output[which(output=="Residuals:"):length(output)]
    result[['aov']]<-capture.output(anova(model))
    result[['vif']]<-capture.output(vif(model))
  } 
  
  return(result)
}

## return plot =>> reg_influence(), reg_influence2(index)
## return result =>> residtest(tukey-test) dw(dubin - watson) 
fun_multireg_resid<-function(formula,plot=T,clientid="admin",max.lag=F){
  clientid<<-clientid
  model<-lm(formula = formula)
  df<-model$model
  var<-names(df) # all variable names
  yvar<- var[1] #y variable name
  xvar<- var[2:length(var)] #x variable name
 
  result<-list()
  ##general resid plot
  result[['residtest']]<-fun_plot(capture.output(residualPlots(model)),plotname = "reg_resid.png")
  
  ##dubin watson value
  if (max.lag!=F){
    result[['dw']]<-capture.output(dwt(model,max.lag))
  }
  
  ##cook's distance & hat value .....etc
  tryCatch({
    png(filename =paste0(path,clientid,"reg_influence.png"),width = 800, height = 800)
    par(mfrow=c(2,2))
    #diag plot
    for (i in c(2,1,4,5)){
      if (i==4){
        #Cook’s D plot (identify D values>4/(n-k-1)):
        cutoff <- 4/(nrow(df)-length(model$coefficients)-2)
        p<-plot(model,which=i, cex.lab=1.5,cook.levels=cutoff,main="Identify D values>4/(n-k-1)") #influence value
        abline(h=cutoff, lty=2, col="red")
      }else if(i==5){
        p<-plot(hatvalues(model),xlab="Outlier: Value > Hat*2(blue)\nOutlier: Value > Hat*3(red)",ylab="Hat-Value(Leverage)",main="Hat-value",sub="leverage") #detect outlier
        abline(h=mean(hatvalues(model))*2,col="blue",lty=4)
        abline(h=mean(hatvalues(model))*3,col="red",lty=3)
        logic<-hatvalues(model)>mean(hatvalues(model))*2
        logic2<-hatvalues(model)>mean(hatvalues(model))*3
        for (i in 1:length(logic)){
          if (logic2[i]&&logic[i]){
            text(i,hatvalues(model)[i],i,col="red",cex=1.2,pos = 3)
          }else if (logic[i]){
            text(i,hatvalues(model)[i],i,col="blue",cex=1.2,pos=3)
          }
        }
      }else{
        p<-plot(model,which=i, cex.lab=1.5)
      }
    }
    par(mfrow=c(1,1))
  },finally = dev.off()) 
  
  ##detect influence and outlier using plot2
  fun_plot({
    influenceIndexPlot(model,main = "Influence Index", cex.lab=1.8)
  },plotname = "reg_influence2.png",width = 800,height = 1200)
  
  result[['influence']]<-capture.output(round(influencePlot(model),3))
  return(result)
}


##variable relative importance (R-squred)
fun_relweights <- function(fit,clientid="admin",...){
  clientid<<-clientid
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  fun_plot({dotchart(import$Weights, labels=row.names(import),
                     xlab="% of R-Square", pch=19,col="blue",
                     main="Relative Importance of Predictor Variables",
                     sub=paste("Total R-Square=", round(rsquare, digits=3)),
                     ...)},plotname = "reg_weight.png",width = 500,height = 500)
  import<-capture.output(import)
  return(import)
}


##Cross Validation (not yet)
shrinkage <- function(fit, k=10){ 
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)} 
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)] 
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k) 
  r2 <- cor(y, fit$fitted.values)^2 
  r2cv <- cor(y, results$cv.fit)^2 
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n") 
  cat("Change =", r2-r2cv, "\n")
}


























