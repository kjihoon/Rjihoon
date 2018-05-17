<<<<<<< HEAD
#install.packages("corrplot")
library(corrplot)
path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/"
fun_plot<-function(p,width = 800, height = 800,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}

##return ==> result
fun_cortest<-function(x, y,alternative ="two.sided",method = "pearson", conflevel = 0.95){
  model<-cor.test(x=x,y=y,alternative = alternative,method=method,conf.level = conflevel)
  result<-list()
  result[['result']]<-capture.output(model)  
  #c("pearson", "kendall", "spearman")
  # c("two.sided", "less", "greater")
  return(result)
}
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}


#return==> cormat,corpmat
fun_cormplot<-function(df,clientid="admin",method = "pearson"){
  clientid<<-clientid
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(df,method =method)
  m<-cor(df,method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  result<-list()
  result[['cormat']]<-capture.output(fun_plot({corrplot(m, method="color", col=col(200),  
                     type="upper", order="hclust", 
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col="black", tl.srt=45, #Text label color and rotation
                     # Combine with significance
                     p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                     # hide correlation coefficient on the principal diagonal
                     diag=FALSE 
  )},plotname = "corplot.png"))
  result[['corpmat']]<-capture.output(p.mat)
  return(result)
}  


=======
#install.packages("corrplot")
library(corrplot)
path<<-"C:/anl/.metadata/.plugins/org.eclipse.wst.server.core/tmp1/wtpwebapps/anl/img/"
#path<<-"C:/hah/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/anl/img/"
fun_plot<-function(p,width = 800, height = 800,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,clientid,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}

##return ==> result
fun_cortest<-function(x, y,alternative ="two.sided",method = "pearson", conflevel = 0.95){
  model<-cor.test(x=x,y=y,alternative = alternative,method=method,conf.level = conflevel)
  result<-list()
  result[['result']]<-capture.output(model)  
  #c("pearson", "kendall", "spearman")
  # c("two.sided", "less", "greater")
  return(result)
}
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}


#return==> cormat,corpmat
fun_cormplot<-function(df,clientid="admin",method = "pearson"){
  clientid<<-clientid
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(df,method =method)
  m<-cor(df,method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  result<-list()
  result[['cormat']]<-capture.output(fun_plot({corrplot(m, method="color", col=col(200),  
                     type="upper", order="hclust", 
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col="black", tl.srt=45, #Text label color and rotation
                     # Combine with significance
                     p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                     # hide correlation coefficient on the principal diagonal
                     diag=FALSE 
  )},plotname = "corplot.png"))
  result[['corpmat']]<-capture.output(p.mat)
  return(result)
}  


>>>>>>> d0df0a5e5ac586d86a4e86a31c6091184bb9354d
  