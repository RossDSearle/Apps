

getSoilSiteData <- function(siteID){
  bits <- str_split(siteID, ' - ')
  dataset <- bits[[1]][1]
  sid <- bits[[1]][2]
  
  url <- paste0(APIRoot,"/Site_Data?DataSet=", dataset,"&siteid=", sid, "&propertytype=LaboratoryMeasurement&tabletype=",soilTableType ,"&usr=TrustedDemo&key=jvdn64df")
  print(url)
  resp <- fromJSON(URLencode(url))
  return(resp)
}



url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=QLDGovernment&siteid=SALTS_96_1&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df'
df <- fromJSON(URLencode(url))
df

xdf <- df[df$ObservedProperty=='3A1',]
odf <- data.frame(UD=xdf$UpperDepth, LD=xdf$LowerDepth, Property=xdf$ObservedProperty, Value=xdf$Value, Units=xdf$Units)
odf
inDF <- odf

plotSoilProfile <- function(inDF){
  
  sbx <- c(calibs$modLL, rev(calibs$modDUL)) * 100
  sby <- c(calibs$depth/10, rev(calibs$depth/10))
  water <- c(calibs$modLL*100, rev((calibs$modLL*100)+as.numeric(ts)))
  
  inDF$Value <- as.numeric(inDF$Value)
  
  xBound <-  max(inDF$Value)  + (max(inDF$Value) * 0.2)
  plot( 0, type="n",  col.main = 'blue', cex.main=2,
        xlab='', 
        ylab='Soil Depth (m)',
        yaxs = "i", xaxs = "i", xlim = c(0, xBound), ylim = rev(range(c(0,1.5))),
        cex.lab = 1.5
  )
  
  polygon(sbx,sby,
          col=c("navajowhite3"),
          border=c("navajowhite3"),
          lwd=1, lty=c("solid"))
  
  polygon(water,sby,
          col=c("lightskyblue"),
          border=c("lightskyblue"),
          lwd=1, lty=c("solid"))
  
  # if(!is.null(bktSize)){
  #     text(0.5,0.4, paste0("Total Bucket Size = ", round(bktSize), ' mm' ), font=2)
  # }
}