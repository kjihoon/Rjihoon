
#o
perceptron<-function(learningrate=0.01,x,y,maxIteration=300,bias=1){
  x<-cbind(bias,x);xn<-names(x)
  x<-as.matrix(x)
  
  w<-matrix(runif(ncol(x),-1,1),ncol = ncol(x))
  y<-ifelse(y[1]==y,-1,1)
  for (i in 1:maxIteration){
    #s<-sample(1:length(y))
    #x<-x[s,]
    #y<-y[s]
    p<-ifelse(x%*%t(w)<0,1,-1)
    if (length(which(p!=y))==0){
      cat(i,"번째 수렴\n")
      break;
    }else{
      j<-which(p!=y)[1]
      w<-w+learningrate*x[j,]*(p[j]-y[j])
      print(j)
    }
  }
  colnames(w)<-xn
  return(w)
}
exdata<-iris[1:100,]
x<-exdata[,1:4]
y<-exdata[,5]
w<-perceptron(x=x,y=y,maxIteration = 5000)


result<-ifelse(as.matrix(cbind(1,x))%*%t(w)>0,-1,1)
result<-table(result,ifelse(y==y[1],-1,1))











