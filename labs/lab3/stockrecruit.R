#SSB <- c(47.946, 47.924,	48.011,	47.480,	46.404,	45.502,	44.499,	43.872,	43.959,	44.093,	42.546,	39.778,	36.915,34.201,30.893,27.866,	23.955,	21.837,	22.722,	24.454,	25.707,	26.562,	26.934,	27.520,	27.849,	27.852,	27.344,	27.033)
#REC <- c(47.061,63.629,16.512,83.210,24.183,68.479,13.500,23.025,12.513,46.046,17.147,10.150,4.908,6.499,139.679,17.938,39.313,33.862,40.992,35.049,20.582,33.545,13.655,48.482,12.114,19.370,10.760,22.693)
#SBPR <- 1.306
#
#
#h <- 0.83032	
#Rzero <- 24.439
#SSB_sim <- 0:50
#pred_rec <-(0.8*Rzero*h*SSB_sim)/(0.2*SBPR*Rzero*(1-h)+(h-0.2)*SSB_sim)
#
#par(mfrow=c(1,1))
#
#plot(SSB,REC,pch=21, bg=1, xlim=c(0,50))
#lines(SSB_sim,pred_rec,type="l", lwd=2)

library(reshape2)
library(ggplot2)
#read in .dat file
dat <- read.table("stockrecruit.dat",skip=4)
colnames(dat) <- c("SSB","REC","SP","SBPR")

#read in .std file
std <- read.table("stockrecruit.std",header=TRUE)

#create table of parameter values for each stock
params <- data.frame(stock=unique(dat$SP),
					 mu_beta=std[std$name=="mu_beta","value"], 
	                 beta_devs=std[std$name=="beta_devs","value"],
	                 Rzero=std[std$name=="Rzero","value"],
	                 h=std[std$name=="h","value"],
	                 SBPR=unique(dat$SBPR)) 

#simulate recruitment for each stock for a range of SSBs
SR.sim <-data.frame(SSB=0:200)
for (i in params$stock){
SR.sim[,i+1] <- (0.8*params$Rzero[i]*params$h[i]*SR.sim$SSB)/(0.2*params$SBPR[i]*params$Rzero[i]*(1-params$h[i])+(params$h[i]-0.2)*SR.sim$SSB)}

#plot stock-recruit relationships with base graphics
d.mar <- c(5, 4, 4, 2) + 0.1
win.graph()
par(mfrow=c(3,4),mar=d.mar-2)
for (i in params$stock){
	plot(dat[dat$SP==i,"SSB"],dat[dat$SP==i,"REC"],pch=21,bg=1,xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(0,200))
	lines(SR.sim$SSB,SR.sim[,i+1],lwd=2)
}

#reshape for ggplot
colnames(SR.sim)<- c("SSB",factor(1:11))
SR.sim <- melt(SR.sim, id.vars="SSB",variable.name="SP",value.name="REC")

#plot stock-recruit relationships with ggplot
ggplot(dat,aes(x=SSB,y=REC))+
geom_point()+
geom_line(data=SR.sim, size=1.2)+
theme_classic()+
facet_wrap(~SP,scales="free")