





#while(dev.cur()[1]>1) dev.off()


#a<-summary(lm(iris$Sepal.Length~iris$Sepal.Width))

#Rserve::Rserve (debug = FALSE, port = 6311) #데몬 실행
r<-function(){
  r<-grep("^Rserve",readLines(textConnection(system('tasklist',intern=TRUE))),value=TRUE)
  r<-substr(r,30,35)[length(r)]
  r<-paste0("TASKKILL /PID ",r," /F")
  system(r)
}




##library(Rserve)


<<<<<<< HEAD

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
=======
>>>>>>> d0df0a5e5ac586d86a4e86a31c6091184bb9354d
       