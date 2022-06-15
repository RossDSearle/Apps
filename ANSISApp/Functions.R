

calculateNLoss <- function(soilType, soilMoisture, rainfall){
  
  if(soilType=='Forest Hill'){
    moistureCategories <- rev(c(2, 1.4, 1.2, 1, 0.7, 0.6, 0.5, 0.4, 0))
  }else if(soilType=='Quirindi'){
    moistureCategories <- rev(c(2, 1.4, 1.2, 1, 0.7, 0.6, 0.5, 0.4, 0))
  }else if(soilType=='Narrabri'){
    moistureCategories <- rev(c(2, 1.1, 1.1, 1, 0.9, 0.8, 0.8, 0.7, 0))
  }else if(soilType=='Hudson'){
    moistureCategories <- rev(c(2, 1.1, 1.0, 1, 0.9, 0.9, 0.8, 0.8, 0))
  }
  
  riskCats <- c(0, 15, 23, 100)
  riskNames <- c('Low', 'Moderate', 'High')
  riskColors <- c('green', 'orange', 'red')

  soilType <- str_remove(soilType, ' ')
  
  rainCategories <- c(0,5,10,15,20,1000)
  rainIndex = findInterval(rainfall, rainCategories)
 
  smIndices <- seq(9,1,-1)  
  moistureIndex = smIndices[findInterval(soilMoisture, moistureCategories)]
 
  df <- VolPotentialTable[soilType][[1]]

  outObj = NULL
  v = df[moistureIndex, rainIndex]
  rcat <- riskNames[findInterval(v, riskCats)]
  col <- riskColors[findInterval(v, riskCats)]
  
  outObj$Value <- v
  outObj$Cat <- rcat
  outObj$Colour <- col
  
  return(outObj)
}

getSoilType <- function(lon, lat){
  sR <- terra::rast(paste0(DataRoot, '/SoilTypes/GenericSoilTypes.tif'))
  v <- terra::extract(sR, y=data.frame(x=lon, y=lat))[1,2]
  soilType <- soilTypes[as.character(v)]
  return(soilType)
}


getSMIPSData <- function(lon, lat){
  
  dt <- Sys.Date()
  dy <- str_pad(day(dt), width=2, side = 'left', pad='0')
  mn <-  str_pad(month(dt), width=2, side = 'left', pad='0')
  yr <- year(dt)
  
  outObj <- NULL
  
  smipsR <- paste0(smipsDataRoot, '/', yr)
  fls <- list.files(paste0(smipsR), pattern = smipsFilter)
  l1 <- str_remove(fls,smipsFilter)
  l2 <- str_remove(l1, '.tif')
  dsdts <- as.Date(paste0(str_sub(l2, 1, 4), '-', str_sub(l2, 5, 6), '-', str_sub(l2, 7, 8)))
  ddf <- data.frame(filename=fls, dt=dsdts)
  sdf <- ddf[order(ddf$dt),]
  lastDate <- sdf[nrow(sdf),]
  rname <- paste0(smipsR, '/', lastDate$filename)
  sR <- terra::rast(rname)
  v <- terra::extract(sR, y=data.frame(x=lon, y=lat))[1,2]
  sm <- min(1, v)
  outObj$SoilMoisture <- sm
  outObj$Date <- format(lastDate$dt, "%d %B %Y")
  return(outObj) 
}



getStateForecast <- function(forecastStoreDF, lon, lat){
  
  loc <- st_point(c(lon,lat))
  reg <- BoMRegions[loc,]
  
  idx <- which(forecastStoreDF$State==reg$STATE)
  
  if(is.na(forecastStoreDF[idx, 2]))
  {
    furl <- forecastURLs[reg$STATE]
    print(furl)
    f <- RCurl::getURI(furl)
    forecastStoreDF[idx, 2] <- f
    print(paste0("forecast for ", reg$STATE, ' Downloaded'))
    
  }else{
    StateForecast <- forecastStoreDF[idx, 2]
  }
  
  return(forecastStoreDF)
}

# downloadStateForecast <- function(lon, lat){
#   
#   loc <- st_point(c(lon,lat))
#   reg <- BoMRegions[loc,]
#   furl <- forecastURLs[reg$STATE]
#   print(furl)
#   f <- RCurl::getURI(furl)
#   #cat(f, file='c:/temp/nsw.xml')
#   return(f)
# }


#StateForecast=getForecast(StateForecast = StateForecast, lon = RV$CurrentLongitude, lat = RV$CurrentLatitude)

getForecast <- function(forecastStoreDF, lon, lat){
  
  loc <- st_point(c(lon,lat))
  reg <- BoMRegions[loc,]
  

  rec <- forecastStoreDF[forecastStoreDF$State==reg$STATE, ]
  StateForecast <- rec$forecast
  
  loc <- st_sf(x = st_sfc(st_point(x = c(lon,lat))))
  st_crs(loc) <- st_crs(rainfallForecastLocations)
  template  <- xmlTreeParse(StateForecast,useInternalNodes=T)
  r <- xmlRoot(template)
  flocs  <- unlist(xpathSApply (r ,"//product/forecast/area", xmlGetAttr, 'aac'))
  idxs <- which (rainfallForecastLocations$AAC %in% flocs)
  filtLocs <- rainfallForecastLocations[idxs,]
  idx = st_nearest_feature(loc,filtLocs)
  id <- filtLocs[idx,]$AAC 
  StationName <- filtLocs[idx,]$PT_NAME
  StateName <- filtLocs[idx,]$STATE_NAME

  nodes = getNodeSet(r, paste0("/product/forecast/area[@aac='", id, "']/forecast-period"))
  
  days <- vector(mode = 'character', length = length(nodes))
  maxTemps <- vector(mode = 'numeric', length = length(nodes))
  minTemps <- vector(mode = 'numeric', length = length(nodes))
  precipitation_range <- vector(mode = 'character', length = length(nodes))
  precis <- vector(mode = 'character', length = length(nodes))
  probability_of_precipitation <- vector(mode = 'character', length = length(nodes))
  
  upperRain <- vector(mode = 'numeric', length = length(nodes))
  lowerRain <- vector(mode = 'numeric', length = length(nodes))
  
  
  for (i in 1:length(nodes)) {
    
    atts <- xmlAttrs( nodes[[i]])
    index <- as.numeric(atts['index'])
    startTime <- as.character(atts['start-time-local'])
    days[i] <- str_split(startTime, 'T')[[1]][1]
    
    mt <- xmlValue( xpathSApply(nodes[[i]], paste0("element[@type='air_temperature_maximum']")))
    if(length(mt)==1){maxTemps[i] <- mt}else{maxTemps[i] <- NA}
    
    mint <- xmlValue( xpathSApply(nodes[[i]], paste0("element[@type='air_temperature_minimum']")))
    if(length(mint)==1){minTemps[i]  <- mint}else{minTemps[i] <- NA}
    
    precipR <- xmlValue( xpathSApply(nodes[[i]], paste0("element[@type='precipitation_range']") ))
    if(length(precipR)==1){
      precipitation_range[i]  <- precipR
      
      bits <- str_split(precipR, ' ')
      
      if(length(bits[[1]])==2){
        upperRain[i] = as.numeric(bits[[1]][1])
        lowerRain[i] = as.numeric(bits[[1]][1])
      }else if(length(bits[[1]])==4){
        
        lowerRain[i] = as.numeric(bits[[1]][1])
        upperRain[i] = as.numeric(bits[[1]][3])
      }
      
      
    }else{
      precipitation_range[i] <- NA
      lowerRain[i] = 0
      upperRain[i] = 0
    }
    
    pres <- xmlValue( xpathSApply(nodes[[i]], paste0("text[@type='precis']") ))
    if(length(pres)==1){precis[i]  <- pres}else{precis[i] <- NA}
    
    prob <- xmlValue( xpathSApply(nodes[[i]], paste0("text[@type='probability_of_precipitation']") ))
    if(length(prob)==1){probability_of_precipitation[i]  <- prob}else{probability_of_precipitation[i] <- NA}
  }
  
  df <- data.frame(Station=StationName, State=StateName, Day=days, MinTemp=minTemps, MaxTemp=maxTemps, PrecipRange=precipitation_range, 
                   Probability=probability_of_precipitation, Precis=pres, LowerRain=lowerRain, upperRain=upperRain)
}

