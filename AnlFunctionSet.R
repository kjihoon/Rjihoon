mean<-iris$Sepal.Length
funttest<-function(x, y = NULL,
               alternative = c("two.sided", "less", "greater"),
               mu = 0, paired = FALSE, varequal = FALSE,
               conflevel = 0.95,clientID="admin"){
  library(ggplot2)
  library(plyr)
  model<-t.test(x=x, y = y,
                alternative = alternative,
                mu = mu, paired = paired, var.equal = varequal,
                conf.level = conflevel)
  v1=deparse(substitute(x))
  v2=deparse(substitute(y))
  output<-list()
  output[["result"]]<-capture.output(model)
  
  path="C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
  if (is.null(y)){
    #boxplot
    png(filename =paste0(path,clientID,"plot.png"),width = 500, height = 500)
    p<-ggplot(data = data.frame(x), aes(x = "", y = x,col=v1)) + 
      geom_boxplot()+xlab(v1)+ggtitle("One Sample T-test Box Plot")
    print(p)
    dev.off()
    
    #hist and dens
    bin.size = 1 + 3.322*log(length(x))# binsize
    png(filename =paste0(path,clientID,"plot_hist.png"),width = 500, height = 500)
    p<-qplot(x, geom = "histogram", breaks = seq(min(x),max(x), (max(x)-min(x))/bin.size), 
          colour = I("white"), fill = I("grey")) +
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * (max(x)-min(x))/bin.size
        }, 
        args = c(mean = mean(x), sd = sd(x), n = length(x), bw = (max(x)-min(x))/bin.size),colour='red')+ggtitle("One Sample T-test Hist & N-D Density Plot")+xlab(v1)
    print(p)
    dev.off()
    
  }else{
    #boxplot
    df<-data.frame('x'=c(x,y),'label'=c(rep(v1,length(x)),rep(v2,length(y))))
    output[["df"]]<-df
    png(filename =paste0(path,clientID,"plot.png"),width = 500, height = 500)
    if (paired==F){
      title= "Independence Two Samle T-test"
    }else{
      title= "Paired Two Samle T-test"
    }
    p<-ggplot(df,aes(label,x,col=label))+geom_boxplot()+ggtitle(title)
    print(p)
    dev.off()
    
    #hist and dens
    png(filename =paste0(path,clientID,"plot_hist.png"),width = 500, height = 500)
    grid <- with(df, seq(min(x), max(x), length = 1000))
    normaldens <- ddply(df, "label", function(df) {
      data.frame( 
        x = grid,
        density = dnorm(grid, mean(df$x), sd(df$x))*length(df$x)*(max(x)-min(x))/(1 + 3.322*log(length(x)))
      )
    })
    p<-ggplot(df, aes(x,fill=label))  + 
      geom_histogram(binwidth =(max(x)-min(x))/(1 + 3.322*log(length(x))),col="white")+ 
      geom_line(aes(y = density), data = normaldens,colour="red") +
      facet_grid(label~.)+ggtitle("Two Sample T-test Hist & N-D Density Plot")
    
    
    print(p)
    dev.off()
  }

  return(output)
}





