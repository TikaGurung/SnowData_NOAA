
rm(list = ls())
setwd('~/Documents/UNL/Class/METR879/Assignment/input1')
outdir <- '~/Documents/UNL/Class/METR879/Assignment/output/'
#read csv and omit NAs valued 
NorthAirPort <- read.csv('NorthPlatteRegionalAirpot.csv',stringsAsFactors = F, header = T,colClasses = c('DATE' = 'character'))
sum(is.na(NorthAirPort$SNOW))/length(NorthAirPort$SNOW)*100 #perentage of missing values i.e. NAs
NorthAirPort<-na.omit(NorthAirPort) ##omit NAs 
NorthExpFarm <- read.csv('NorthPlatteExperimentalFarm.csv',header = T,stringsAsFactors = F)
sum(is.na(NorthExpFarm$SNOW))/length(NorthExpFarm$SNOW)*100
NorthExpFarm<-na.omit(NorthExpFarm)
Wallace2w <-read.csv('Wallace2w.csv',header = T,stringsAsFactors = F)
sum(is.na(Wallace2w$SNOW))/length(Wallace2w$SNOW)*100
Wallace2w <-na.omit(Wallace2w)
Hershey5SSE <- read.csv('Hershey5SSE.csv',header = T, stringsAsFactors = F)
sum(is.na(Hershey5SSE$SNOW))/length(Hershey5SSE$SNOW)*100
Hershey5SSE <-na.omit(Hershey5SSE)


##time index for each data set
time_NorthAirPort <-  as.POSIXlt(NorthAirPort$DATE,format="%m/%d/%Y",tz="UTC")
time_NorthExpFarm <-  as.POSIXlt(NorthExpFarm$DATE,format="%m/%d/%Y",tz="UTC")
time_Wallace2w    <-  as.POSIXlt(Wallace2w$DATE,format="%m/%d/%Y",tz="UTC")
time_Hershey5SSE  <-  as.POSIXlt(Hershey5SSE$DATE,format="%m/%d/%Y",tz="UTC")


###snow data daily to yearly
yearly_snow_NorthAirPort <- aggregate(NorthAirPort$SNOW, by=list(time_NorthAirPort$year+1), FUN="mean", colname=c("Year"))
yearly_snow_NorthExpFarmt <- aggregate(NorthExpFarm$SNOW, by=list(time_NorthExpFarm$year+1), FUN="mean", colname=c("Year"))
yearly_snow_Wallace2w <- aggregate(Wallace2w$SNOW, by=list(time_Wallace2w$year+1), FUN="mean", colname=c("Year"))
yearly_snow_Hershey5SSE <- aggregate(Hershey5SSE$SNOW, by=list(time_Hershey5SSE$year+1), FUN="mean", colname=c("Year"))

df_yearly_all <- data.frame(seq(1981,2010),yearly_snow_NorthAirPort$x, yearly_snow_NorthExpFarmt$x, yearly_snow_Wallace2w$x,
                            yearly_snow_Hershey5SSE$x)
colnames(df_yearly_all) <- c('Year', 'North Platee Airport', "North Platte Exp. Farm", 'Wallace 2w', 'Hershey 5SSE')
df_yearly_all$Average <- rowMeans(df_yearly_all[2:5])

##calculating yearly anomalies
df_yearly_all$Anomaly<- mean(df_yearly_all$Average) - df_yearly_all$Average #

plot(df_yearly_all$Year, df_yearly_all$Anomaly, type = "l", xlab = "Year", ylab = "Yearly Anomaly", main = "Yearly Snowfall Anomalies from 1981-2010")

## plot of time series snow data with average of 4 stations and anomalies of them

{
#png(paste(outdir,'Yearly_TimeSeries_SnowData.png', sep = ''), height = 8,width = 7,units = 'in',res = 300)
par(mfrow=c(3,2))
for (i in 2:6) {

plot(df_yearly_all$Year,df_yearly_all[,i],type = 'l',lwd=2,xlab = 'Year', ylab = 'Snow [m]',
     ylim = c(0,0.3),
     main = names(df_yearly_all[i]))
###fit linear model to the data
linear_NorthAirPort <-lm(df_yearly_all[,i]~seq(1981,2010))

##Summary of the model
eq_a <- paste0("y = ", round(linear_NorthAirPort$coefficients[2],3),"x","+",round(linear_NorthAirPort$coefficients[1],3))


##Ploting the linear model
abline(linear_NorthAirPort, col="steelblue", lwd=2)
text(2000, 0.15,eq_a)

}

#t-test analysis
  test_all <-NA
for (i in 2:6) {
test <- t.test(df_yearly_all$Year,df_yearly_all[,i])
test_all[i] <-round(test$p.value,5)

}
  test_all <- test_all[-1] #removing the assign NA value

names(test_all) <- names(df_yearly_all[2:6])
test_all

#plot outside of loop because the y axis limit is differet
plot(df_yearly_all$Year, df_yearly_all$Anomaly, type = "l",lwd = 2, xlab = "Year", ylab = "Anomaly [m]", 
     ylim = c(-0.1,0.1),
     main = "Yearly snow anomalies")
abline(h=0,lty=2)

#dev.off()
}

### monthly snow data
monthly_snow_NorthAirPort <- aggregate(NorthAirPort$SNOW, by=list(time_NorthAirPort$mon+1), FUN="mean", colname=c("Month"))
monthly_snow_NorthExpFarm <- aggregate(NorthExpFarm$SNOW, by=list(time_NorthExpFarm$mon+1), FUN="mean", colname=c("Month"))
monthly_snow_Wallace2w <- aggregate(Wallace2w$SNOW, by=list(time_Wallace2w$mon+1), FUN="mean", colname=c("Month"))
monthly_snow_Hershey5SSE <- aggregate(Hershey5SSE$SNOW, by=list(time_Hershey5SSE$mon+1), FUN="mean", colname=c("Month"))

#barplot(monthly_snow_NorthAirPort$x)

df_monthly_all <- data.frame(seq(1,12), monthly_snow_NorthAirPort$x,monthly_snow_NorthExpFarm$x, monthly_snow_Wallace2w$x,
                             monthly_snow_Hershey5SSE$x)
colnames(df_monthly_all) <- c('Month', 'North Platee Airport', "North Platte Exp. Farm", 'Wallace 2w', 'Hershey 5SSE')
df_monthly_all$Average <- rowMeans(df_monthly_all[2:5])
df_monthly_all$SD <- rowSds(as.matrix(df_monthly_all[2:5]))

##

{
#png(paste(outdir,'Climatology of 4 station.png',sep = ''),height = 7, width = 9,units = 'in', res = 300)
par(mfrow=c(2,2))  
for (j in 2:5) {
barplot(df_monthly_all[,j], names.arg = df_monthly_all$Month,
        ylab = "Snow [m]", ylim = c(0,0.25),
        xlab = "Month",
        main = names(df_monthly_all[j]),
        border = 'deepskyblue4',
        col ="deepskyblue4")

}

#dev.off()
}


##average monthly snow climatology
{
#png(paste(outdir,'Average_Climatology of 4 stations Data.png',sep = ''), height = 6, width = 6,units = 'in', res = 300)
x <- barplot(df_monthly_all$Average, names.arg = df_monthly_all$Month,
        ylab = "Snow [m]", ylim = c(0,0.25),
        xlab = "Month",
        main = names(df_monthly_all[6]),
        border = 'deepskyblue4',
        col ="deepskyblue4")

arrows(x, df_monthly_all$Average - df_monthly_all$SD, x,
       df_monthly_all$Average + df_monthly_all$SD, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
#dev.off()
}
##

##calculating return period
# Sort the yearly average snowfall values in descending order
snowfall <-df_yearly_all # change the variable since we are going to alter the value

snowfall <- snowfall[order(snowfall$Average, decreasing = TRUE),]

# Calculate the annual probability
n <- nrow(snowfall)
annual_probability <- 1 / (n + 1 - 1:n)

# Calculate the return period
return_period <- 1 / annual_probability

{
#png(paste(outdir,'Probability and return period.png',sep = ''),height = 6, width = 8,units = 'in', res = 300)
par(mfrow=c(1,2))
plot(snowfall$Average, annual_probability*100,pch=19, ylab = "Probability density [%]", xlab = 'Snow [m]')
plot(snowfall$Average, return_period, pch=19,  xlab = "Snowfall [m]", ylab = "Return period [year]")
#dev.off()
}



summary(snowfall$Average)

#histogram of averrage snow data

png(paste(outdir,'Histogram of snow.png',sep = ''),height = 6, width = 8,units = 'in', res = 300)

hist(snowfall$Average, main="Snow", xlab="Snow [m]", ylab="Frequency [year]", col="lightblue", border="white")
box()
dev.off()
