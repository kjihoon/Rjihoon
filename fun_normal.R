<<<<<<< HEAD
path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/"


fun_plot<-function(p,width = 500, height = 500,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}


##Normal test 2plot 1result
fun_normal<-function(x,clientid="admin"){
  clientid<<-clientid
  xvar<<-deparse(substitute(x))
  ##qqplot  
  tryCatch({png(filename =paste0(path,clientid,"normal_qq.png"),width = 500, height = 500);
    qqnorm(x);grid();
    qqline(x,col="red")},finally = while(dev.cur()[1]>1){dev.off()})
  ##hist&dens
  bin.size <<- 1 + 3.322*log(length(x))# binsize
  
    fun_plot({hist(x, breaks=bin.size, col="grey",border="white",freq=FALSE,xlab=xvar,
         main=paste("Distribution of",xvar),xlim = c(min(x),max(x)));
    rug(jitter(x), col="brown");
    curve(dnorm(x, mean=mean(x), sd=sd(x)),add=TRUE, col="blue", lwd=2);
    lines(density(x)$x, density(x)$y,col="red", lwd=2, lty=2);
    legend("topright",legend = c( "Normal Curve", "Kernel Density Curve"),
           lty=1:2, col=c("blue","red"), cex=.7)},plotname = "normal_dist.png")
  result<-list()
  shapiro<-capture.output(shapiro.test(x))
  shapiro[length(shapiro)+1]<-"alternative hypothesis: It does not follow the normal distribution."
  result[['shapiro']]<-shapiro
 
  return(result)
}
=======
path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/"


fun_plot<-function(p,width = 500, height = 500,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}


##Normal test 2plot 1result
fun_normal<-function(x,clientid="admin"){
  clientid<<-clientid
  xvar<<-deparse(substitute(x))
  ##qqplot  
  tryCatch({png(filename =paste0(path,clientid,"normal_qq.png"),width = 500, height = 500);
    qqnorm(x);grid();
    qqline(x,col="red")},finally = while(dev.cur()[1]>1){dev.off()})
  ##hist&dens
  bin.size <<- 1 + 3.322*log(length(x))# binsize
  
    fun_plot({hist(x, breaks=bin.size, col="grey",border="white",freq=FALSE,xlab=xvar,
         main=paste("Distribution of",xvar),xlim = c(min(x),max(x)));
    rug(jitter(x), col="brown");
    curve(dnorm(x, mean=mean(x), sd=sd(x)),add=TRUE, col="blue", lwd=2);
    lines(density(x)$x, density(x)$y,col="red", lwd=2, lty=2);
    legend("topright",legend = c( "Normal Curve", "Kernel Density Curve"),
           lty=1:2, col=c("blue","red"), cex=.7)},plotname = "normal_dist.png")
  result<-list()
  result[['shapiro']]<-capture.output(shapiro.test(x))
  return(result)
}
>>>>>>> d0df0a5e5ac586d86a4e86a31c6091184bb9354d
