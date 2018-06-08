
path<<-"C:/newPJ/.metadata/.plugins/org.eclipse.wst.server.core/tmp0/wtpwebapps/eris/img/"
fun_plot<-function(p,width = 500, height = 500,plotname="plot.png"){
  tryCatch({
    png(filename =paste0(path,plotname),width = width, height = height)
    print(p)
  },finally = while(dev.cur()[1]>1){dev.off()})
}

pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("rlist")
pkgTest("rjson")
pkgTest("ggplot2")
pkgTest("reshape")
pkgTest("stringr")

readData<-function(data,idx=2){
  
  data$V2<-trimws(data$V2,"both")
  
  ## temp humid
  th<-data[data$V2=="temp",]
  temp_humid<-str_split_fixed(as.character(th$V4),"/",n = 2) %>% apply(2,as.numeric)
  th<-cbind(th[,1:3],temp_humid)
  names(th)<-c("date","location","busidx","temp","humid")
  th<-melt(th[,-2],id.vars = c("busidx","date")) 
  th<-split(th[,-1],th$busidx) ## split table by busidx
  list.save(th, paste0("c:/LOG/th_",format(Sys.time(),"%y%m%d%H"),idx,".json")) ##save json
  for (i in 1:length(th)){
    p<-ggplot(th[[i]],aes(x=factor(rep(1:(nrow(th[[i]])/2),2)),y=value,group=variable,col=variable))+geom_line()+facet_grid(.~variable)+ggtitle(paste0("(",format(Sys.time(),"%y/%m/%d/%Hh"),")Bus index: ",names(th[i]))) + 
      theme(plot.title = element_text(lineheight=.8, face="bold"),axis.text.x=element_blank())+xlab("Time")+geom_smooth()
    fun_plot(p,plotname = paste0("th_",format(Sys.time(),"%y%m%d%H"),idx,".png"))
  }
  
  ## can data
  
  
  
  
  #return("true")
}

