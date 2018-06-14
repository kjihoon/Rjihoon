



perceptron<-function(learningrate=0.01,x,y,maxIteration=300,bias=1){
  x<-cbind(bias,x);xn<-names(x)
  x<-as.matrix(x)
  
  w<-matrix(runif(ncol(x),-1,1),ncol = ncol(x))
  y<-ifelse(y[1]==y,-1,1)
  for (i in 1:maxIteration){
    s<-sample(1:length(y)) #가중치 수정없이 머무를 가능성에 대한 처리
    x<-x[s,]
    y<-y[s]
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

x<-spam[,c(6,7,5)]
y<-spam[,ncol(spam)]
w<-perceptron(x=x,y=y,maxIteration = 5000)


result<-ifelse(as.matrix(cbind(1,x))%*%t(w)>0,-1,1)
result<-table(result,ifelse(y==y[1],-1,1))




s<-sample(x=1:100,30)
exdata<-iris[1:100,]
testset<-exdata[s,]
trainset<-exdata[-s,]

w<-perceptron(x=trainset[,1:4],y=trainset[,5])

testset[,5]<-ifelse(testset[,5]=="setosa",1,0)

y_pred<-as.matrix(cbind(bias=1,testset[,1:4]))%*%t(w)
y_pred<-ifelse(y_pred>0,1,0)

table(y_pred,testset[,5])

