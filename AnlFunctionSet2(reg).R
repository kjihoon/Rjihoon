
##test data
formula= iris$Sepal.Length~iris$Sepal.Width+iris$Petal.Width+iris$Species
data=NULL
x=F
y=F
group=NULL
influence = T


plotFun<-function(p,width = 800, height = 800,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
  }



regression<-function(formula, data=NULL, x = FALSE, y = FALSE,group=NULL,clientid="admin",influence=F){
  library(ggplot2)
  model<-lm(formula=formula, data=data, x = x, y = y)
  df<-model$model
  yvar<- names(df[1]) ## y variable name
  xvar<-strsplit(as.character(formula)[3],split = " + ",fixed = T)[[1]] ## x variable names
  xlen<-length(xvar)## independent variable length
  clientid<<-clientid
  path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
  type<-c()
  for (i in 1:length(df)) type<-append(type,class(df[,i]))
  
  #psych
  library(psych)
  library(car)
  plotFun({p<-pairs.panels(df[,type!="factor"], cex =1.3,
                           method = "pearson",hist.col = "red",
                           density = TRUE,
                           ellipses = TRUE # show correlation ellipses
  )},plotname = "reg_psy.png")
  
  if (xlen==1){ ##Simple Regression
      
    #Simple regression scatter & linear(lm)
    if (!is.null(group)){
      #linear plot each group
      p<-ggplot(df,aes(eval(parse(text = xvar)),eval(parse(text = yvar)),col=group))+
        geom_point()+geom_smooth(method="lm",alpha=.1)+xlab(xvar)+ylab(yvar)+theme_bw()
      plotFun(p,plotname = "reg_line.png")
      
      #Separate model each group
      model<-list()
      for (i in unique(group)){
        model[[i]] <- lm(eval(parse(text=yvar))[group==i]~eval(parse(text=xvar))[group==i])
      }
      
    }else{
      
      ##linear plot
      p<-ggplot(df,aes(eval(parse(text = xvar)),eval(parse(text = yvar))))+
        geom_point(aes(color=abs(model$residuals)))+geom_smooth(method="lm",alpha=.1,col="red")+xlab(xvar)+ylab(yvar)+
        theme_bw()+scale_color_gradient2(low = "skyblue", mid = "blue", high = "black")+
        guides(color=F)
      plotFun(p,plotname = "reg_line.png")
      
      ##resid plot
      p<-residualPlots(model,tests=F)
      plotFun(residualPlots(model),plotname = "reg_resid.png")
      model[['residtest']]<-p
      
      ##resid plot2 (comp1onet vs variables)
      #plotFun(crPlots(model))
    }
    
    #Multi regression scatter matix plot & linear(lm)
  }else{
    
    ##resid plot
    p<-residualPlots(model,tests=F)
    plotFun(residualPlots(model),plotname = "reg_resid.png")
    model[['residtest']]<-p
    
    ##resid plot2 (componet vs variables)
    #plotFun(crPlots(model))
    
    ##Additional Option on Multi regression model
    model[['anovaTable']]<-anova(model)
    model[['vif']]<-car::vif(model)
  }
  
  
  if (influence==T&&is.null(group)){
    ##detect influence and outlier using plot
    tryCatch({
      png(filename =paste0(path,clientid,"reg_influence.png"),width = 800, height = 800)
      par(mfrow=c(2,2))
      for (i in c(2,1,4,5)) p<-plot(model,which=i, cex.lab=1.5)
      par(mfrow=c(1,1))
    },finally = dev.off()) 
    
    ##detect influence and outlier using plot2
    plotFun({
      influenceIndexPlot(model,main = "Influence Index", cex.lab=1.8)
    },plotname = "reg_influence2.png",width = 500,height = 1000)
    model[['influence']]<-round(influencePlot(model),3)
  }
  
  
  return(model)
}


