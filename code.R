setwd("/Users/a533733/Documents/otherProjects/weatherInterp")

#load table
dCO <- read.table("data/stationsCO.txt")
dCO <- dCO[,c(2,3,4,5,8,10)]
names(dCO) <- c("station","elev","latitude","longitude","time","temp")
#transformations
dCO$elev <- dCO$elev*3.28084
dCO$temp <- 9/5*(dCO$temp-273.15) + 32

#plot color map
maxTemp <- max(dCO$temp)
minTemp <- min(dCO$temp)
tempRange <- maxTemp-minTemp
colors <- rgb((dCO$temp-minTemp)/tempRange,0,1-(dCO$temp-minTemp)/tempRange,1)
plot(dCO$longitude, dCO$latitude, col=colors)

#thin plate spline
library(fields)
loc <- as.matrix(dCO[,4:3])
tpsTemp <- Tps(loc, dCO$temp, df = nrow(dCO)-1)
tpsElev <- Tps(loc, dCO$elev, df = nrow(dCO)-1)

#Now..
#Leave out each observation, one at a time
#Interpolate its temperature and elevation
#put these, as well as *actual* values, into a new data frame
#then fit deviation of temp on deviation of elevation
tempInterp <- NULL
elevInterp <- NULL
for(i in 1:nrow(dCO))
{
	#fit
	dCOTemp <- dCO[-i,]
	tpsTemp <- Tps(as.matrix(dCOTemp[,4:3]), dCOTemp$temp)#, df = nrow(dCOTemp)-1)
	tpsElev <- Tps(as.matrix(dCOTemp[,4:3]), dCOTemp$elev)#, df = nrow(dCOTemp)-1)
	
	#interpolate
	tempInterp <- c(tempInterp, predict(tpsTemp, dCO[i,4:3]))
	elevInterp <- c(elevInterp, predict(tpsElev, dCO[i,4:3]))
}

dResid <- data.frame(dCO$temp-tempInterp, dCO$elev-elevInterp)
names(dResid) <- c("tempResid","elevResid")
save(dResid, file="dResid.RData")

m <- lm(tempResid ~ elevResid, data=dResid)
summary(m)

#Prediction process:
#1. Put in latitude, longitude, and elevation
#2. Use latitude and longitude to make elevInterp and tempInterp
#3. Use (elev - elevInterp) to estimate temp

m <- lm(tempResid ~ elevResid, data=dResid)
loc <- as.matrix(dCO[,4:3])
tpsTemp <- Tps(loc, dCO$temp)#, df = nrow(dCO)-1)
tpsElev <- Tps(loc, dCO$elev)#, df = nrow(dCO)-1)
predictTemp <- function(latitude, longitude, elevation)
{
	#get interps
	tempInterp <- predict(tpsTemp, t(c(longitude,latitude)))
	elevInterp <- predict(tpsElev, t(c(longitude,latitude)))
	
	#use model
	output <- tempInterp + coef(m)[1] + coef(m)[2]*(elevation-elevInterp)
	
	list(
		prediction = as.numeric(output),
		tempInterp = as.numeric(tempInterp),
		elevInterp = as.numeric(elevInterp)
		)
}
