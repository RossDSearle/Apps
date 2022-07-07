

getVocabNames <- function(df, props){
  
  apiProps <- unique(df$ObservedProperty)
  apiProps <- apiProps[which(!is.na(apiProps))]
  cnames <- vector(mode = 'character', length = length(apiProps))
  
  for(i in 1:length(apiProps)){
    rec <- props[props$Property==apiProps[i],]
    if(nrow(rec)>0){
      if( rec$VocabURL==''){
        propName = rec$Description
      }else{
        
        SVARoot <- vocPaths[vocPaths$vocTypes==rec$PropertyType,]$vocURL
        vurl <- paste0(SVARoot,'?uri=',rec$VocabURL)
        js <- fromJSON(vurl)
        propName <- js$result$primaryTopic$prefLabel$'_value'
      }
    }
    cnames[i] <- propName
  }
  cnames <- cnames[nzchar(cnames)] #removes blanks
  pdf <- data.frame(LabMethod=apiProps, VocName=cnames)
  return(pdf)
}



getSoilSiteData <- function(siteID){
  
  bits <- str_split(siteID, ' - ')
  dataset <- bits[[1]][1]
  sid <- bits[[1]][2]
  
  url <- paste0(APIRoot,"/Site_Data?DataSet=", dataset,"&siteid=", sid, "&propertytype=LaboratoryMeasurement&tabletype=",soilTableType ,"&usr=TrustedDemo&key=jvdn64df")
  print(url)
  resp <- fromJSON(URLencode(url))
  return(resp)
}



getColor <- function(sites) {
  sapply(sites$DataSet, function(DataSet) {
    if(DataSet == 'QLDGovernment') {
      "green"
    } else if(DataSet == 'NatSoil') {
      "orange"
    } else {
      "red"
    } })
}


plotSoilProfileHBars <- function(inDF, xTitle){
  
  inDF$Value <- as.numeric(inDF$Value)
  xBound <-  max(inDF$Value)  + (max(inDF$Value) * 0.2)
  par(oma=c(0,0,0,0)) # all sides have 3 lines of space
  par(mar=c(5,4,0,0))
  plot( 0, type="n",  col.main = 'blue', cex.main=2,
        xlab= paste0( inDF$Units[1]), 
        ylab='Soil Depth (m)',
        yaxs = "i", xaxs = "i", xlim = c(0, xBound), ylim = rev(range(c(0,1.5))),
        cex.lab = 1.5,
  )
  
  for (s in 1:nrow(inDF)) {
    
    x <- c(0.1, inDF$Value[s], inDF$Value[s], 0, 0)
    y =  c( inDF$LD[s],  inDF$LD[s],  inDF$UD[s],  inDF$UD[s],  inDF$LD[s])
    polygon(x,y, col=c("brown"), border=c("black"), lwd=1, lty=c("solid"))
  }
  
}



#plotBoxPlot(vals=seq(1,10,1), obsVal=6, Title='Test')

plotBoxPlot <- function(vals, obsVal, Title){
  boxplot(vals, col = 'green')
  #stripchart(5, cex=5, pch = 18, col = 'red', vertical = TRUE, add = TRUE)
  points(c(obsVal,4),  cex=5, pch = 18, col = 'red')
}