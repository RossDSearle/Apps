

getSoilSiteData <- function(siteID){
  bits <- str_split(siteID, ' - ')
  dataset <- bits[[1]][1]
  sid <- bits[[1]][2]
  
  url <- paste0(APIRoot,"/Site_Data?DataSet=", dataset,"&siteid=", sid, "&propertytype=LaboratoryMeasurement&tabletype=",soilTableType ,"&usr=TrustedDemo&key=jvdn64df")
  print(url)
  resp <- fromJSON(URLencode(url))
  return(resp)
}
