


machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discovery') {
 # probeDBfile <- '/datasets/work/af-digiscapesm/work/Ross/SoilWaterNow/modelAveraging/SoilWaterNow.db'
  probeDBfile <- '/srv/DB/SoilWaterNow/SoilWaterNow.db'
  
}else {
  probeDBfile <- 'C:/Projects/SoilWaterNow/ModelComparisons/SoilWaterNow.db'
}

runQuery <- function(sql){
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
  res <- dbSendQuery(con, sql)
  df <- dbFetch(res)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
  return(df)
}




sendStatement <- function(sql){
  con <- dbConnect(RSQLite::SQLite(), probeDBfile )  
  res <- RSQLite::dbSendStatement(con, sql)
  dbClearResult(res)
  RSQLite::dbDisconnect(con)
}
