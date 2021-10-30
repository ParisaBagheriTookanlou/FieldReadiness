
install.packages('reshape2')
install.packages('ggplot2')
install.packages('plyr')
install.packages('grid')
install.packages('tikzDevice')
install.packages('data.table')

library ('reshape2')
library ('ggplot2')
library ('plyr')
library ('grid')
library ('tikzDevice')
library ('data.table')
library(MDP2)
library("discretizeGaussian")

# Plot the simulated data of weather info and sensor data with the realts of Gussian SSM for three scenarios
####################################################################################################################################

#Adjust the scaling factor for MP and SP
datS1 <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS1.csv", stringsAsFactors = F)
datS2 <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS2.csv", stringsAsFactors = F)
datS3 <- read.csv2("C:/Users/au520279/Documents/FieldReadiness/paper/results_data_paper/datS3.csv", stringsAsFactors = F)

datS1$SP <- datS1$SP*10
datS2$SP <- datS2$SP*10
datS3$SP <- datS3$SP*10
datS1$MP<-datS1$MP*10
datS2$MP<-datS2$MP*10
datS3$MP<-datS3$MP*10
datS1$GR<-datS1$GR/10
datS2$GR<-datS2$GR/10
datS3$GR<-datS3$GR/10

datS1_M <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 5))
datS2_M <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 5))
datS3_M <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 5))

datS1_W1 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))
datS2_W1 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))
datS3_W1 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))

datS1_W2 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))
datS2_W2 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))
datS3_W2 <-  as.data.frame(matrix(nrow = (param$tMax), ncol = 4))

colnames(datS1_M) <- c("t", "Soil_Moisture", "Posterior_Mean", "Posterior_Variance", "scenario")
colnames(datS2_M) <- c("t", "Soil_Moisture", "Posterior_Mean", "Posterior_Variance", "scenario")
colnames(datS3_M) <- c("t", "Soil_Moisture", "Posterior_Mean", "Posterior_Variance", "scenario")

colnames(datS1_W1) <- c("t", "Temprature", "Precipitation", "scenario")
colnames(datS2_W1) <- c("t", "Temprature", "Precipitation", "scenario")
colnames(datS3_W1) <- c("t", "Temprature", "Precipitation", "scenario")

colnames(datS1_W2) <- c("t",  "Global_Radiation", "Wind_Speed", "scenario")
colnames(datS2_W2) <- c("t",  "Global_Radiation", "Wind_Speed", "scenario")
colnames(datS3_W2) <- c("t",  "Global_Radiation", "Wind_Speed", "scenario")

for (i in 1:param$tMax){
 datS1_M$t[i] <- datS1$t[i]+12+(t_count-1)
 datS2_M$t[i] <- datS2$t[i]+12+(t_count-1)
 datS3_M$t[i] <- datS3$t[i]+12+(t_count-1)
}

for (i in 1:param$tMax){
  datS1_M$Soil_Moisture[i] <- datS1$MW[i]
  datS2_M$Soil_Moisture[i] <- datS2$MW[i]
  datS3_M$Soil_Moisture[i] <- datS3$MW[i]
}

for (i in 1:param$tMax){
  datS1_M$Posterior_Mean[i] <- datS1$MP[i]
  datS2_M$Posterior_Mean[i] <- datS2$MP[i]
  datS3_M$Posterior_Mean[i] <- datS3$MP[i]
}

for (i in 1:param$tMax){
  datS1_M$Posterior_Variance[i] <- datS1$SP[i]
  datS2_M$Posterior_Variance[i] <- datS2$SP[i]
  datS3_M$Posterior_Variance[i] <- datS3$SP[i]
}

for (i in 1:param$tMax){
  datS1_M$scenario[i] <- datS1$scenario[i]
  datS2_M$scenario[i] <- datS2$scenario[i]
  datS3_M$scenario[i] <- datS3$scenario[i]
}

for (i in 1:param$tMax){
  datS1_W1$t[i] <- datS1$t[i]+12+(t_count-1)
  datS2_W1$t[i] <- datS2$t[i]+12+(t_count-1)
  datS3_W1$t[i] <- datS3$t[i]+12+(t_count-1)
}

for (i in 1:param$tMax){
  datS1_W2$t[i] <- datS1$t[i]+12+(t_count-1)
  datS2_W2$t[i] <- datS2$t[i]+12+(t_count-1)
  datS3_W2$t[i] <- datS3$t[i]+12+(t_count-1)
}

for (i in 1:param$tMax){
  datS1_W1$Temperature[i] <- datS1$Tem[i]
  datS2_W1$Temperature[i] <- datS2$Tem[i]
  datS3_W1$Temperature[i] <- datS3$Tem[i]
}

for (i in 1:param$tMax){
  datS1_W1$Precipitation[i] <- datS1$Pre[i]
  datS2_W1$Precipitation[i] <- datS2$Pre[i]
  datS3_W1$Precipitation[i] <- datS3$Pre[i]
}

for (i in 1:param$tMax){
  datS1_W2$Global_Radiation[i] <- datS1$GR[i]
  datS2_W2$Global_Radiation[i] <- datS2$GR[i]
  datS3_W2$Global_Radiation[i] <- datS3$GR[i]
}

for (i in 1:param$tMax){
  datS1_W2$Wind_Speed[i] <- datS1$Wind[i]
  datS2_W2$Wind_Speed[i] <- datS2$Wind[i]
  datS3_W2$Wind_Speed[i] <- datS3$Wind[i]
}

for (i in 1:param$tMax){
  datS1_W1$scenario[i] <- datS1$scenario[i]
  datS2_W1$scenario[i] <- datS2$scenario[i]
  datS3_W1$scenario[i] <- datS3$scenario[i]
}

for (i in 1:param$tMax){
  datS1_W2$scenario[i] <- datS1$scenario[i]
  datS2_W2$scenario[i] <- datS2$scenario[i]
  datS3_W2$scenario[i] <- datS3$scenario[i]
}

dtPlot1 <- rbindlist(list(datS1_M,datS2_M,datS3_M))
dat1<-melt(data.frame(dtPlot1),

           id.vars=c("scenario","t"),
           measure.vars=c("Soil_Moisture","Posterior_Mean","Posterior_Variance"),

           variable.name="name",

           value.name="y"
)

dat1$scenario<-factor(dat1$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))
plot1<-ggplot(data=dat1, aes(x=factor(t), y=y, group=name, shape=name, linetype=name ) ) +

 geom_line(size=0.3)+
 scale_y_continuous(breaks=seq(0,50,1), labels = c(0:50) ) +

  facet_grid(. ~ scenario) +

  xlab("Day number") + ylab(" ")

 g <- guide_legend("",nrow=1,byrow=TRUE, override.aes = list(fill=NA))

 plot1 + guides(shape = g, linetype=g)  +


  geom_line(size=0.3) +

  theme_bw() +

  theme(legend.position="bottom", panel.background = element_blank(),

        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),

        legend.key = element_rect(fill = NA, colour = NA),

        legend.key.width = unit(1, "cm"), legend.text.align=0.1, axis.title.x= element_text(vjust = -0.7),

        axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),

        strip.background=element_rect(fill = NA), legend.title = element_text(size = 6),
        legend.text = element_text(size = 6))

dev.off()
#######################################################################################################

dtPlot2 <- rbindlist(list(datS1_W1,datS2_W1,datS3_W1))

dat2<-melt(data.frame(dtPlot2),

           id.vars=c("scenario","t"),
           measure.vars=c("Temperature","Precipitation"),

           variable.name="name",

           value.name="y"
)

dat2$scenario<-factor(dat2$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))

plot2<-ggplot(data=dat2, aes(x=factor(t), y=y, group=name, shape=name, linetype=name ) ) +

  geom_line(size=0.3) + scale_y_continuous(breaks=seq(0,50,1), labels = c(0:50) ) +

  facet_grid(. ~ scenario) +

  xlab("Day number") + ylab(" ")

g <- guide_legend("",nrow=1,byrow=TRUE, override.aes = list(fill=NA))


plot2 + guides(shape = g, linetype=g)  +

  geom_line(size=0.3) +

  theme_bw() +

  theme(legend.position="bottom", panel.background = element_blank(),

        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),

        legend.key = element_rect(fill = NA, colour = NA),

        legend.key.width = unit(0.5, "cm"), legend.text.align=0.5, axis.title.x= element_text(vjust = -0.7),

        axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),

        strip.background=element_rect(fill = NA))

dev.off()
####################################################################################################################################
dtPlot2 <- rbindlist(list(datS1_W2,datS2_W2,datS3_W2))

dat2<-melt(data.frame(dtPlot2),

           id.vars=c("scenario","t"),
           measure.vars=c("Global_Radiation", "Wind_Speed"),

           variable.name="name",

           value.name="y"
)


dat2$scenario<-factor(dat2$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))

plot2<-ggplot(data=dat2, aes(x=factor(t), y=y, group=name, shape=name, linetype=name ) ) +

  geom_line(size=0.3) + scale_y_continuous(breaks=seq(0,50,1), labels = c(0:50) ) +

  facet_grid(. ~ scenario) +

  xlab("Day number") + ylab(" ")

g <- guide_legend("",nrow=1,byrow=TRUE, override.aes = list(fill=NA))

plot2 + guides(shape = g, linetype=g)  +

  geom_line(size=0.3) +

  theme_bw() +

  theme(legend.position="bottom", panel.background = element_blank(),

        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),

        legend.key = element_rect(fill = NA, colour = NA),

        legend.key.width = unit(0.5, "cm"), legend.text.align=0.5, axis.title.x= element_text(vjust = -0.7),

        axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),

        strip.background=element_rect(fill = NA))

dev.off()
####################################################################################################################################
  datS1$t <- c(1:14)
  datS2$t <- c(1:14)
  datS3$t <- c(1:14)

# Plot optimal tillage decisions for three scenarios
####################################################################################################################################
# Plot optimal decisions
#tikz("OptimalPaper_plot.tex", width = 9, height = 6, standAlone=T)

plot(c(0,14), c(1,6), yaxt="n", xlab="Day number", ylab='', xaxt="n", bty='n', pch=NA)
abline(v=1:30,lty= 2, col="gray85")
axis(1, las=1, at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), labels=c("13","14","15","16","17","18","19","20","21","22","23","24","25","26"),
     cex.axis=0.7)
axis(2, las=3, col = "white", at=c(2, 5), labels=c("Ploughing", "Preparation and Seeding"), line=1, cex.axis=0.9)
axis(2, las=1, col = "white", at=c(1.5, 2, 2.5), labels=c("Scenario 1", "Scenario 2", "Scenario 3"), line=-3, cex.axis=0.7)
axis(2, las=1, col = "white", at=c(4.5, 5, 5.5), labels=c("Scenario 1", "Scenario 2", "Scenario 3"), line=-3, cex.axis=0.7)


#title(main="Optimal ", cex.main=1)

dat<-datS1
dat<-as.data.table(dat)
vecFCor<-c(1.5, 4.5)
for(j in 1:param$opNum){
  for(i in dat[dat$operation==j]$t){

    if( (dat[dat$t==i]$optAction=="do.") || (dat[dat$t==i]$optAction=="doF.") ) labCol=15 else labCol=17
    datL<-rep(vecFCor[j],1)
    points(x = c(i), y = datL, type="p", pch=labCol, col="black", cex = 1.85)
  }
}


dat<-datS2
dat<-as.data.table(dat)
vecFCor<-c(2, 5)
for(j in 1:param$opNum){
  for(i in dat[dat$operation==j,]$t){

    if( (dat[dat$t==i]$optAction=="do.") || (dat[dat$t==i]$optAction=="doF.") ) labCol=15 else labCol=17
    datL<-rep(vecFCor[j],1)
    points(x = c(i), y = datL, type="p", pch=labCol, col="green", cex = 1.85)
  }
}


dat<-datS3
dat<-as.data.table(dat)
vecFCor<-c(2.5, 5.5)
for(j in 1:param$opNum){
  for(i in dat[dat$operation==j,]$t){

    if( (dat[dat$t==i]$optAction=="do.") || (dat[dat$t==i]$optAction=="doF.") ) labCol=15 else labCol=17
    datL<-rep(vecFCor[j],1)
    points(x = c(i), y = datL, type="p", pch=labCol, col="blue", cex = 1.85)
  }
}

dev.off()
####################################################################################################################################


