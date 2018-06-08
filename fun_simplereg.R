
#path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/"
path<<-"C:/newPJ/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/Anl/img/"

fun_plot<-function(p,width = 400, height = 400,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}


## return plot =>> reg_psy, reg_line
## return result =>> summary
fun_simplereg<-function(formula,plot=T,group=NULL,clientid="admin"){
  clientid<<-clientid
  model<-lm(formula = formula)
  df<-model$model
  yvar<- names(df)[1] #y variable name
  xvar<- names(df)[2] #x variable name
  library(ggplot2)
  library(psych)
  library(car)
  if (plot==T){
  fun_plot({p<-pairs.panels(df, cex =1.3,
                           method = "pearson",hist.col = "red",
                           density = TRUE,
                           ellipses = TRUE # show correlation ellipses
  )},plotname = "reg_psy.png")
  }
  result<-list()
  if (is.null(group)){
    output<-capture.output(summary(model))
    result[['summary']]<-output[which(output=="Residuals:"):length(output)]
    result[['beta']]<-substring(round(coefficients(model),5),1,5)
    result[['rsq']]<-substring(round(summary(model)[[8]],5)*100,1,5)
      
    if (plot==T){
      p<-ggplot(df,aes(eval(parse(text = xvar)),eval(parse(text = yvar))))+
        geom_point(aes(color=abs(model$residuals)))+geom_smooth(method="lm",alpha=.1,col="red")+xlab(xvar)+ylab(yvar)+
        theme_bw()+scale_color_gradient2(low = "skyblue", mid = "blue", high = "black")+
        guides(color=F)
      fun_plot(p,plotname = "reg_line.png")
    }
    
  }else {
    
    for (i in unique(group)){
      each<-paste0(i,"_summary")
      fit<-lm(eval(parse(text=yvar))[group==i]~eval(parse(text=xvar))[group==i])
      names(fit$coefficients)[2]<-xvar
      output<-capture.output(summary(fit))
      result[[each]] <- output[which(output=="Residuals:"):length(output)]
    }
    result[['group']] <-unique(group)
    if (plot==T){
      p<-ggplot(df,aes(eval(parse(text = xvar)),eval(parse(text = yvar)),col=group))+
        geom_point()+geom_smooth(method="lm",alpha=.1)+xlab(xvar)+ylab(yvar)+theme_bw()
      fun_plot(p,plotname = "reg_line.png")
    }
  }
  return(result)
}


## return plot =>> reg_influence, reg_influence2,reg_resid
## return result =>> residtest(tukey-test) influence, if max.lag=T,then dw
fun_simplereg_resid<-function(formula,plot=T,clientid="admin",maxlag=F){
  clientid<<-clientid
  model<-lm(formula = formula)
  df<-model$model
  yvar<- names(df)[1] #y variable name
  xvar<- names(df)[2] #x variable name

  result<-list()
  ##general resid plot
  result[['residtest']]<-fun_plot(capture.output(residualPlots(model)),plotname = "reg_resid.png")
  
  ##dubin watson value
  if (maxlag!=F){
    result[['dw']]<-capture.output(dwt(model,maxlag))
  }
  
  ##cook's distance & hat value .....etc
  tryCatch({
    png(filename =paste0(path,clientid,"reg_influence.png"),width = 700, height = 700)
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
  },plotname = "reg_influence2.png",width = 400,height = 800)
  
  result[['influence']]<-capture.output(round(influencePlot(model),3))
  return(result)
}
