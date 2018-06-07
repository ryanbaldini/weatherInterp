shinyServer(function(input, output) {

	#Load last updated version
	#load("tpsTemp.RData")
	#load("dCO.RData")
	
	#Download data and update
	rawData <- scan("https://madis-data.ncep.noaa.gov/madisPublic1/cgi-bin/madisXmlPublicDir?rdr=&time=0&minbck=-59&minfwd=0&recwin=3&dfltrsel=2&state=CO&latll=0.0&lonll=0.0&latur=90.0&lonur=0.0&stanam=&stasel=0&pvdrsel=0&varsel=1&qcsel=99&xml=0&csvmiss=1&nvars=T", what="character")
	#Extract
	temp <- (as.numeric(rawData[which(rawData == "V-T")+9]) - 273.15)*9/5 + 32
	elev <- 3.28084*as.numeric(rawData[which(rawData == "V-T")+2])
	lat <- as.numeric(rawData[which(rawData == "V-T")+3])
	long <- as.numeric(rawData[which(rawData == "V-T")+4])
	#Collect
	dCO <- data.frame(lat,long,elev,temp)
	#Drop temperature outliers
	drop <- which(abs((temp-mean(temp))/sd(temp)) >= 5)
	if(length(drop)>0) dCO <- dCO[-drop,]
	#Clean up
	rm(temp,elev,lat,long,drop)

	#Fit thin-plate splines
	library(fields)
	#loc <- as.matrix(dCO[,1:2])
	#tpsTemp <- Tps(loc, dCO$temp, df = nrow(dCO)-1)
	#tpsElev <- Tps(loc, dCO$elev, df = nrow(dCO)-1)
	loc <- as.matrix(dCO[,1:3])
	tpsTemp <- Tps(loc, dCO$temp)
	#Save it
	#save(tpsTemp, file="tpsTemp.RData")
	
	#Create output
	output$predictedTemp <- renderText({
		if(!is.null(tpsTemp) & !is.null(input$lat) & !is.null(input$long) & !is.null(input$elev))
		{
			loc <- t(c(input$lat, input$long, input$elev))
			paste("Predicted Temperature:", round(predict(tpsTemp,loc),1), "F")
		}
	})	
})