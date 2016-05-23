library(dplyr)
library(ggplot2)
library(ggmap)
library(lubridate)

#the data used in the the allcrime file, plus data for other years (and an assortment 
#of Austinmunicipal data sources) are available at:
#   https://data.austintexas.gov/browse?category=Public+Safety

setwd("C:/Users/jeff/Documents/R/Map_project")
allcrime <- read.csv("APD_Incident_Extract_2008and2015.csv", stringsAsFactors=FALSE)
#---------------------------------------------------------------------
#categorize the 483 incident types
ctgy <- as.data.frame(unique(allcrime$Crime.Type))
ctgy[,1] <- as.character(ctgy[,1])

ctgy.test <- grep("ASSAULT+|MURDER|AGG+|ASSLT+|KIDNAPPING+", ctgy[,1])
ctgy[ctgy.test,2] <- "violence"
ctgy.test <- grep("MARIJUANA+|CONTROLLED SUB", ctgy[,1])
ctgy[ctgy.test,2] <- "Drugs"
ctgy.test <- grep("BURG+|THEFT+|CARD ABUSE+|ROBBERY+", ctgy[,1])
ctgy[ctgy.test,2] <- "stealing" 
ctgy.test <- grep("PROSTITUTION+", ctgy[,1])
ctgy[ctgy.test,2] <- "prostitution" 
ctgy.test <- grep("RAPE+|SEXUAL ASSAULT+", ctgy[,1])
ctgy[ctgy.test,2] <- "Rape" 
ctgy.test <- grep("CRASH+", ctgy[,1])
ctgy[ctgy.test,2] <- "Crash" 
ctgy.test <- grep("FAMILY DISTURBANCE+", ctgy[,1])
ctgy[ctgy.test,2] <- "Domestic" 
ctgy.test <- grep("PUBLIC INTOXICATION+", ctgy[,1])
ctgy[ctgy.test,2] <- "PI" 

names(ctgy) <- c("Crime.Type", "category")

atxcrime <- left_join(allcrime, ctgy, by="Crime.Type")
#    Add Year column
  atxcrime[,11] <- as.Date(atxcrime$Date,'%m/%d/%Y')
  atxcrime[,11] <- format(atxcrime[,11],'%Y')
  colnames(atxcrime)[11] <- "Year"
  # get rid of some stray 2014 dates (only 2)   
  plot.data<- filter(atxcrime, atxcrime$Year=="2008" | atxcrime$Year=="2015")

violence <- filter(atxcrime, atxcrime$category == "violence")
crash <- filter(atxcrime, atxcrime$category =="Crash")
drugs <- filter(atxcrime, atxcrime$category =="Drugs")
domestic <- filter(atxcrime, atxcrime$category=="Domestic")
PI <- filter(atxcrime, atxcrime$category=="PI")

#    Check what % is categorized
Coded <- filter(atxcrime, is.na(atxcrime$category))
  print(paste("% Coded:",(1-(nrow(Coded)/nrow(atxcrime)))*100)) 
  rm(Coded); rm(allcrime)
#----------------------------------------------------------
#     Get lon/lat
#first get all existing spread around
#d = as.data.frame(matrix(ncol=10, nrow=329901))
hasll <- filter(atxcrime, !is.na(atxcrime$LONGITUDE)) # has LAT/LON
#temp <- filter(atxcrime, !is.na(atxcrime$LONGITUDE)) %>% unique(temp$ADDRESS)
lonlatdb <- unique(hasll$ADDRESS, stringsAsFactors=FALSE)
#lonlatdb <- as.data.frame(unique(lonlatdb$ADDRESS, stringsAsFactors=FALSE))#    list of unique addresses
lonlatdb <- as.data.frame(I(lonlatdb))
  #lonlatdb <- as.character(temp2[,1])
  names(lonlatdb) <- "ADDRESS"
#df of atxcrime rows with unique Addresses that have lon/lat
lonlatdb <- left_join(hasll, lonlatdb, by = "ADDRESS", match = "first")
  lonlatdb <- lonlatdb[,c(6,7,8)]
  names(lonlatdb) <- c("ADDRESS", "lon", "lat")
atxcrime <- left_join(atxcrime, lonlatdb, by = "ADDRESS", match = "first")
HasGeo <- filter(atxcrime, !is.na(atxcrime$LONGITUDE))

#     Check what % of entries have Geo coordinates
  print(paste("% with Geo:", round(((nrow(HasGeo)/nrow(atxcrime))*100)))) 
          rm(HasGeo)

  #    Draw Map  
b <- get_map(location="Austin", zoom=12, maptype="roadmap")
#ggmap(b, extent="device")
ggmap(b)
#     Plot points
#pp <- filter(atxcrime, !is.na(atxcrime$LONGITUDE))

#     select category, Plot points
crime.ctgy <- "Drugs"
plot.data <- filter(atxcrime, !is.na(atxcrime$LONGITUDE) & atxcrime$category==crime.ctgy)

#plot.data[,11] <- as.Date(plot.data$Date,'%m/%d/%Y')
  #names(plot.data[11]) <- "Year"
  #plot.data[,11] <-format(plot.data[,11],'%Y')
  # get rid of some stray 2014 dates (only 2)   
  #plot.data<- filter(plot.data, plot.data$V11=="2008" | plot.data$V11=="2015")
  
#display map + plot.data
c <- ggmap(b) + geom_point(data=plot.data, aes(x=LONGITUDE, y=LATITUDE, color = plot.data$Year), alpha = .5) + ggtitle(paste("ATX Crime Reports: ",crime.ctgy))
#c <- c + + scale_fill_discrete(name="Year")

#separate map for each year
c <- c + labs(x = "latitude", y="longitude")
c <- c + labs(color="Year")

c + facet_grid(category ~ Year)
