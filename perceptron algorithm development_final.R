##perceptron

###1
exdata<-iris[1:100,c(2,3,5)]
exdata[,3]<-ifelse(exdata[,3]==exdata[1,3],1,0)

x<-exdata[,1:2]
x<-cbind(bias=1,x)
y<-as.matrix(exdata[,3])
w<-matrix(runif(3,-1,1))


##2
xw<-as.matrix(x)%*%w
head(xw)
output<-ifelse(xw>0,1,0)


plot(x[,2:3],col=y+1,pch=ifelse(y==output,1,4))


#3

plot(x[,2:3],col=y+1,xlim=c(1,7),ylm=c(-3,8))
grid()
learningrate<-0.01 #learning rate 초기화
maxIteration<-3000 #최대 학습횟수
w<-runif(3,-1,1)
for (i in 1:maxIteration){
  output<-ifelse(as.matrix(x)%*%w>0,1,0)
  if (length(which(output!=y))==0){ #모두 정분류 되었을때 중지
    cat(i,"번째 중지\n")
    abline(a=-w[1]/w[3],b=-w[2]/w[3],col="red",lwd=2)
    break
  }else{
    j<-which(output!=y)[1] #오분류된 첫번째obs index
    w<-w+learningrate*(y[j]-output[j])*t(x[j,])
    abline(a=-w[1]/w[3],b=-w[2]/w[3])
    Sys.sleep(0.3)
  }
}


perceptron(x=iris[,1:4],y=iris[,5])



