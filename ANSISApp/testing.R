library(terra)
library(dplyr)
library(jsonlite)



p <- read.csv('Z:/Ross/TERN/pars.csv', stringsAsFactors = F)

for (i in 1:nrow(p)) {
  f <- p[i,]$x
  r <- rast(paste0('/vsicurl/https://esoil.io/TERNLandscapes/Public/Products/TERN/Covariates/Mosaics/90m/', f, '.tif'))  
  plot(r, main=f)
}


props <- read.csv(paste0('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ANSISApp/Data/PropertyLookups.csv'), stringsAsFactors = F)




url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=QLDGovernment&siteid=SALTS_96_1&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df'
df <- fromJSON(URLencode(url))
df

sx=130.0567 
sy=-29.9692

sdf <- fromJSON(paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Locations?longitude=", sx,"&latitude=", sy ,"&propertytype=LaboratoryMeasurement&closest=5&usr=TrustedDemo&key=jvdn64df"))

sitename <- sdf[1,]$Location_ID 


odf<-data.frame()
for (i in 1:nrow(sdf)) {
 print(i)
  rec <- sdf[i,]
  url <- paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=', rec$DataSet ,'&siteid=', rec$Location_ID ,'&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df')
  df <- fromJSON(URLencode(url))
  if(is.null(df$error)){
      #idxs <- which(df$ObservedProperty=='4A1' & df$UpperDepth==0)
      if(nrow(df)>0){
        odf<-rbind(odf, df)
      }
  }
}

idxs <- which(odf$UpperDepth==0)
obs = as.data.frame(odf[idxs,] %>% group_by(ObservedProperty)  %>% summarise(n()))
idxxs <- which(obs[,2] >= 5)
obsToDo <- obs[idxxs,]
obsToDo

vals <- as.numeric(odf[odf$UpperDepth==0 & odf$ObservedProperty == '4A1', ]$Value)
boxplot(vals, col = 'green')
stripchart(8.5, cex=5, pch = 18, col = 'red', vertical = TRUE, add = TRUE)  




xdf <- df[df$ObservedProperty=='3A1',]
odf <- data.frame(UD=xdf$UpperDepth, LD=xdf$LowerDepth, Property=xdf$ObservedProperty, Value=xdf$Value, Units=xdf$Units)
odf


inDF <- odf
plotSoilProfileHBars <- function(inDF){
  
  inDF$Value <- as.numeric(inDF$Value)
  
  xBound <-  max(inDF$Value)  + (max(inDF$Value) * 0.2)
  par(mar=c(4,4,0,0) + 0.1)
  plot( 0, type="n",  col.main = 'blue', cex.main=2,
        xlab='', 
        ylab='Soil Depth (m)',
        yaxs = "i", xaxs = "i", xlim = c(0, xBound), ylim = rev(range(c(0,1.5))),
        cex.lab = 1.5
  )
  
  for (s in 1:nrow(inDF)) {
    
    x <- c(0.1, inDF$Value[s], inDF$Value[s], 0, 0)
    y =  c( inDF$LD[s],  inDF$LD[s],  inDF$UD[s],  inDF$UD[s],  inDF$LD[s])
    polygon(x,y,
            col=c("green"),
            border=c("black"),
            lwd=1, lty=c("solid"))
  }
  
}

idxs <- which(!is.na(df$ObservedProperty))
df[idxs, ]

if(!is.null(df$error)){
  print(df$error)
}

df[1,1]


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
      print(vurl)
      js <- fromJSON(vurl)
      propName <- js$result$primaryTopic$prefLabel$'_value'
    }
  }
  cnames[i] <- propName
}







library(reshape2)
library(ggplot2)
df <- read.csv("TestData.csv", header=T)
df.m <- melt(df, id.var = "Label")

ggplot(data = df.m, aes(x=Label, y=value)) + 
  geom_boxplot() + facet_wrap(~variable,ncol = 4)