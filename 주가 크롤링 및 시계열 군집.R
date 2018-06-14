library(rvest)
library(xml2)
library(stringr)
ll<-c("006570","005930","035720","090430","000660","051900","035760","057050","130960","041510")
stock<-c()
stocktable<-data.frame()
for (ii in 1:length(ll)){
  stock<-c()
  for (i in 1:6){
    url<-paste0("http://finance.naver.com/item/sise_day.nhn?code=",ll[ii],"&page=")
    url<-paste0(url,i)
    html1<-read_html(url)
    node<-html_nodes(html1,css="table")%>%html_nodes(".num")%>%html_nodes(".tah")
    text<-html_text(node)
    
    t<-text[c(1,7,13,19,25,31,37,43,49,55)]
    stock<-append(stock,t)
    print(i)
    
  }
  stock<-str_replace_all(stock,",","");stock<-as.numeric(stock)
  stocktable<-rbind(stocktable,stock)
}
row.names(stocktable)<-ll;names(stocktable)<-c(1:60)
stocktable<-t(stocktable)

for (i in 1:10){
  stocktable[,i]<-as.vector(scale(stocktable[,i]))
}
plot(stocktable[,1],type="l",col=1)
for (i in 2:10){
  lines(stocktable[,i],type="l")#,col=i)
}

tstock<-t(stocktable)
k<-kmeans(tstock,3);k<-k$cluster


plot(stocktable[,1],type="l",col=k[1])

for (i in 2:10){
  lines(stocktable[,i],type="l",col=k[i])
}


names(k)[k==3]