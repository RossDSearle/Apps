library(terra)



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

xdf <- df[df$ObservedProperty=='3A1',]
odf <- data.frame(UD=xdf$UpperDepth, LD=xdf$LowerDepth, Property=xdf$ObservedProperty, Value=xdf$Value, Units=xdf$Units)
odf

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
