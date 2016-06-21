library(reshape2)
library(ggplot2)
#read in .dat file
dat <- read.table("sr_new.dat",skip=4)
colnames(dat) <- c("SSB","REC","SP","SBPR")

#read in .std file
std <- read.table("stockrecruit.std",header=TRUE)

#create table of parameter values for each stock
params <- data.frame(stock=unique(dat$SP),
					 mu_beta=std[std$name=="mu_beta","value"], 
	                 log_sigma_h=std[std$name=="log_sigma_h","value"],
	                 log_sigma=std[std$name=="log_sigma","value"],
	                 log_Rzero=std[std$name=="log_Rzero","value"],
	                 h_devs=std[std$name=="h_devs","value"],
	                 hbar=std[std$name=="hbar","value"],
	                 SBPR=unique(dat$SBPR))
params$Rzero <- exp(params$log_Rzero)
params$h     <- params$hbar + params$h_devs

#simulate recruitment for each stock for a range of SSBs
SR.sim <-data.frame(SSB=0:200)
for (i in params$stock){
SR.sim[,i+1] <- (0.8*params$Rzero[i]*params$h[i]*SR.sim$SSB)/(0.2*params$SBPR[i]*params$Rzero[i]*(1-params$h[i])+(params$h[i]-0.2)*SR.sim$SSB)}
SR.sim[SR.sim<0] <- 0


#plot stock-recruit relationships with base graphics
d.mar <- c(5, 4, 4, 2) + 0.1
win.graph()
par(mfrow=c(4,6),mar=d.mar-c(3,2,3,2))
for (i in params$stock){
	plot(dat[dat$SP==i,"SSB"],dat[dat$SP==i,"REC"],pch=21,bg=1,xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(0,200))
	lines(SR.sim$SSB,SR.sim[,i+1],lwd=2)
}

#reshape for ggplot
colnames(SR.sim)<- c("SSB",factor(1:max(dat$SP)))
SR.sim <- melt(SR.sim, id.vars="SSB",variable.name="SP",value.name="REC")

#plot stock-recruit relationships with ggplot
ggplot(dat,aes(x=SSB,y=REC))+
geom_point()+
geom_line(data=SR.sim)+
theme_classic()+
facet_wrap(~SP,scales="free")