#ICB exercise 10
#drug given at time 250
N0<- 100
M0<- 1
rM<- .2
rN<- .2 
K<- 1000000
timesteps<- 100
Ns<-numeric(length=timesteps)
Ms<-numeric(length=timesteps)
Ns[1]<-N0
Ms[1]<-M0
x<-0

for(i in 1:1000){
  if(x<1){
    Ns[i+1]<-Ns[i]+rN*Ns[i]*(1-((Ns[i]+Ms[i])/K))
    Ms[i+1]<-Ms[i]+rM*Ms[i]*(1-((Ns[i]+Ms[i])/K))
  }
  else if(x>0){
    Ns[i+1]<-Ns[i]+(rN/-2)*Ns[i]
    Ms[i+1]<-Ms[i]+(rM/2)*Ms[i]*(1-((Ns[i]+Ms[i])/K))
  }
  if(i==250){
    x<-x+1
  }
}
Ms
Ns 


library(ggplot2)
sim<- data.frame(time=1:length(Ns), N=Ns, M=Ms)
ggplot(data=sim, aes(x=time, y=N), aes(x=time, y=M))+geom_line(aes(x=time, y=N), color="red")+geom_line(aes(x=time, y=M), color="blue")+theme_classic()
