

props <- read.csv('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ANSISApp/Data/PropertyLookups.csv', stringsAsFactors = F)

# Chem methods - Vocab front page - https://vocabs.ardc.edu.au/viewById/634
# yellow Book - https://vocabs.ardc.edu.au/viewById/313

js <- fromJSON('https://vocabs.ardc.edu.au/repository/api/lda/ub/soil-chemical-methods-australasia/2011/resource.json?uri=http://anzsoil.org/def/au/scma/03A1')





url <-'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=NSWGovernmentASRIS&siteid=DPIE_1004750_2436&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df'
url <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=WAGovernment&siteid=501_CRA_0013_1&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df'

df <- fromJSON(url)

apiProps <- unique(df$ObservedProperty)
cnames <- vector(mode = 'character', length = length(apiProps))

for(i in 1:length(apiProps)){
  rec <- props[props$Property==apiProps[i],]
   if(nrow(rec)>0){
          if( rec$VocabURL==''){
              propName = rec$Description
          }else{
              vurl <- paste0('https://vocabs.ardc.edu.au/repository/api/lda/ub/soil-chemical-methods-australasia/2011/resource.json?uri=',rec$VocabURL)
              #print(vurl)
              js <- fromJSON(vurl)
              propName <- js$result$primaryTopic$prefLabel$'_value'
          }
   }
  cnames[i] <- propName
}
cnames <- cnames[nzchar(cnames)] #removes blanks
print(cnames)

mdf <- data.frame(LabMethod=apiProps, VocName=cnames)

item = 'Clay (%) - Not recorded'
labMethod = mdf[mdf$VocName==item,]$LabMethod

ddf <- df[df$ObservedProperty==labMethod,]
ddf
