#load required packages
library(reshape2)
library(plyr)

#read in data from .csv files
yield       <- read.csv("yield.csv")
cpue        <- read.csv("cpue.csv")

#reshape data frame
yield       <- melt(yield,id.vars="Year",variable.name = "Fleet",value.name="Yield",na.rm=TRUE)
yield$Fleet <- as.numeric(yield$Fleet)

cpue       <- melt(cpue,id.vars="Year",variable.name = "Fleet",value.name="CPUE",na.rm=TRUE)
cpue$Fleet <- as.numeric(cpue$Fleet)

#exclude years/fleets that don't have both yield and cpue
data <- merge(yield,cpue, all=FALSE)

#reorder data frame by fleet
data <- data[,c("Fleet","Year","Yield","CPUE")]
data <- data[order(data$Fleet),]

#approximate effort (repeated in .tpl file)
data$Effort <- data$Yield/data$CPUE

#sum yield by year
tot_yield <- ddply(data[,c("Year","Yield")],"Year",numcolwise(sum))

#separate cpue by fleet
fleet_1 <- subset(data[,c("Year","Yield","CPUE")], data$Fleet==1)
fleet_2 <- subset(data[,c("Year","Yield","CPUE")], data$Fleet==2)
fleet_4 <- subset(data[,c("Year","Yield","CPUE")], data$Fleet==4)
fleet_7 <- subset(data[,c("Year","Yield","CPUE")], data$Fleet==7)

#construct .dat file
dat <- c(
"#nyear",
as.character(length(unique(data$Year))),
"#nyear_1",
as.character(length(fleet_1$Year)),
"#nyear_2",
as.character(length(fleet_2$Year)),
"#nyear_4",
as.character(length(fleet_4$Year)),
"#nyear_7",
as.character(length(fleet_7$Year)),
"#nfleet",
as.character(length(unique(data$Fleet))),
"",
"#Years",
paste(as.vector(tot_yield$Year),collapse=" "),
"#Yield",
paste(as.vector(tot_yield$Yield),collapse=" "),
"",
"#Years: fleet1",
paste(as.vector(fleet_1$Year),collapse=" "),
"#Yield: fleet1",
paste(as.vector(fleet_1$Yield),collapse=" "),
"#CPUE: fleet1",
paste(as.vector(fleet_1$CPUE),collapse=" "),
"",
"#Years: fleet2",
paste(as.vector(fleet_2$Year),collapse=" "),
"#Yield: fleet2",
paste(as.vector(fleet_2$Yield),collapse=" "),
"#CPUE: fleet2",
paste(as.vector(fleet_2$CPUE),collapse=" "),
"",
"#Years: fleet4",
paste(as.vector(fleet_4$Year),collapse=" "),
"#Yield: fleet4",
paste(as.vector(fleet_4$Yield),collapse=" "),
"#CPUE: fleet4",
paste(as.vector(fleet_4$CPUE),collapse=" "),
"",
"#Years: fleet7",
paste(as.vector(fleet_7$Year),collapse=" "),
"#Yield: fleet7",
paste(as.vector(fleet_7$Yield),collapse=" "),
"#CPUE: fleet7",
paste(as.vector(fleet_7$CPUE),collapse=" "),
"",
"#End of file marker",
42,
"#!")

write.table(dat,paste(getwd(),"/lab4_2.DAT",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)

#wrangle results
results <- read.delim("lab4_2.rep",colClasses="numeric",comment.char="#",sep=" ",header=FALSE,fill=TRUE)
rownames(results) <- c("year","biomass","yield","obs_cpue_1","pred_cpue_1","obs_cpue_2","pred_cpue_2","obs_cpue_4","pred_cpue_4","obs_cpue_7","pred_cpue_7","cpue_res_1","cpue_res_2","cpue_res_4","cpue_res_7","F","MSY","Bmsy","Fmsy","rel_biom","rel_F")
results <- data.frame(t(results))
rownames(results) <- NULL

#plot
par(mfrow=c(2,2))
plot(fleet_1$Year,fleet_1$CPUE,ylab="CPUE",xlab="Year",main="Fleet 1")
lines(fleet_1$Year,results$pred_cpue_1[!is.na(results$pred_cpue_1)],lwd=2,col=2)
plot(fleet_2$Year,fleet_2$CPUE,ylab="CPUE",xlab="Year",main="Fleet 2")
lines(fleet_2$Year,results$pred_cpue_2[!is.na(results$pred_cpue_2)],lwd=2,col=2)
plot(fleet_4$Year,fleet_4$CPUE,ylab="CPUE",xlab="Year",main="Fleet 4")
lines(fleet_4$Year,results$pred_cpue_4[!is.na(results$pred_cpue_4)],lwd=2,col=2)
plot(fleet_7$Year,fleet_7$CPUE,ylab="CPUE",xlab="Year",main="Fleet 7")
lines(fleet_7$Year,results$pred_cpue_7[!is.na(results$pred_cpue_7)],lwd=2,col=2)

par(mfrow=c(1,2))
plot(results$year[!is.na(results$year)],results$biomass[!is.na(results$biomass)],type="l",ylab="Biomass",xlab="Year")
plot(results$year[!is.na(results$year)],results$yield[!is.na(results$yield)],type="l",ylab="Yield",xlab="Year")
