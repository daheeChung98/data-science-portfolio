t.n<-50
o.n<-10
dat<-matrix(0,nrow=t.n*o.n*3,ncol=3) #id, observation, time
dat[,1]=rep(1:(o.n*3),each=t.n)
obs<-NULL
time<-NULL
line_types <- c(2,3,4)
color_types <- c("black","red","blue")

for(i in 1:o.n){
  idx<-seq(1,10, length.out=t.n)
  val<-dnorm(idx,4+runif(1,-1,1),1)*rnorm(1,35,2)
  #val<-dnorm(idx,3,1)*rnorm(length(idx),35,4)
  plot(idx,val,lty=2,lwd=2,col="black",xlim=c(1,10),ylim=c(0,25),type="l", xlab = "Time", ylab = "X",ann=F);par(new=T)
  obs<-c(obs,val)
}

for(i in 1:o.n){
  idx<-seq(1,10, length.out=t.n)
  val<-dnorm(idx,7+runif(1,-1,1),1)*rnorm(1,45,2)
  plot(idx,val,lty=3,lwd=2,col="red",xlim=c(1,10),ylim=c(0,25),type="l", xlab = "Time", ylab = "X",ann=F);par(new=T)
  obs<-c(obs,val)
}

for(i in 1:o.n){
  idx<-seq(1,10, length.out=t.n)
  val<-(0.4*dnorm(idx,3+runif(1,-1,1),1)+0.6*dnorm(idx,7+runif(1,-1,1),1))*rnorm(1,40,2)
  plot(idx,val,lty=4,lwd=2,col="blue",xlim=c(1,10),ylim=c(0,25),type="l", xlab = "Time", ylab = "X",ann=F);par(new=T)
  title(xlab="Time",
        ylab="X",
        cex.lab=1.2, font.lab=2)
  legend("topleft", legend = c("Group 1","Group 2","Group 3"),col=color_types, lty = line_types,lwd=c(4,4,4), border="white",cex = 2.5,
         box.lty = 0,);par(new=T)
  obs<-c(obs,val)
}

