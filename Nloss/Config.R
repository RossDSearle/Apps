

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  
  smipsDataRoot <- '/datasets/lw-sm-forecast_work_processed_delivery/SMIPS/v1.0/SMindex'
  
  #smipsDataRoot <- '/datasets/work/lw-sm-forecast/work/processed/delivery/SMIPS/v1.0/SMindex'
  DataRoot <- '/datasets/work/lw-soildatarepo/work/Ross/ShinyData/NLoss'
}else{
  smipsDataRoot <- 'C:/Projects/NLoss/Data/SMIPS'
  DataRoot <- 'C:/Projects/NLoss/Data'
}

AppName = 'NLoss'

defWidth = 300

#####  Spatial Data
rainfallForecastLocations <- st_read(paste0(DataRoot, '/BoM/RainfallStations.shp'))
BoMRegions <- st_read(paste0(DataRoot, '/BoM/BoMRegions.shp'))

smipsFilter <- 'smips_smi_perc_'



####  Volatolisation tables
VolPotentialTable = fromJSON( 
         '{"Hudson": [[31.4472389, 27.23162654, 22.82876045, 19.69200693, 16.7063264, 1.06928839],
                     [31.44678372, 27.23068032, 22.8282461, 19.69134568, 16.70594103, 1.061797753],
                     [31.44532778, 27.22869654, 22.82792958, 19.6907077, 16.70578142, 1.046816479],
                     [31.42520988, 27.22293989, 22.83933114, 19.70941526, 16.72163536, 1],
                     [31.29101572, 27.14412719, 22.78696459, 19.65595578, 16.65786923, 0.921348315],
                     [31.20903345, 27.09121894, 22.73731394, 19.59333242, 16.57128418, 0.887640449],
                     [31.11217193, 27.03552403, 22.67804067, 19.50125693, 16.43564442, 0.842696629],
                     [31.05258209, 27.04090697, 22.65688921, 19.39758989, 16.2679539, 0.777153558],
                     [31.24823706, 27.24842101, 22.75496653, 19.37776692, 16.20392829, 0.711610487]],
  
  "Quirindi": [[27.06162121, 22.80263628, 17.40166214, 13.82522443, 10.83937423, 1.661538462],
               [27.13045553, 22.91348559, 17.52155857, 13.93539291, 10.93538468, 1.364102564],
               [27.25961501, 23.13417638, 17.74283791, 14.16260489, 11.14606021, 1.2],
               [27.45585249, 23.46668209, 17.98301995, 14.47019152, 11.51851782, 1],
               [28.66787279, 23.85387904, 18.05263383, 14.59977021, 11.80519513, 0.758974359],
               [29.58968143, 24.15516922, 18.23613777, 14.68275814, 11.83587159, 0.666666667],
               [30.61151266, 24.94255029, 18.56979845, 14.95132501, 12.02485641, 0.558974359],
               [32.1869467, 25.74142607, 19.19110502, 15.49268685, 12.57269864, 0.420512821],
               [34.27357725, 27.03182212, 19.99475984, 16.02680238, 13.17745379, 0.307692308]],
  
  "ForestHill": [[26.11170319, 21.03966884, 16.33773342, 12.86312719, 9.980784957, 1.856382979],
                  [26.19599912, 21.94382741, 16.50044496, 12.96318954, 10.07117792, 1.377659574],
                  [26.26102909, 22.05446012, 16.61006311, 13.06116321, 10.1581552, 1.20212766],
                  [26.36128988, 22.27304641, 16.74123664, 13.20081929, 10.32077403, 1],
                  [26.38102344, 22.52358467, 16.87141775, 13.37190224, 10.53867032, 0.744680851],
                  [27.05114808, 22.50236263, 16.93090158, 13.46419882, 10.6590897, 0.64893617],
                  [28.03687789, 22.80226173, 17.01977388, 13.59133549, 10.83269457, 0.537234043],
                  [29.46251123, 23.64711297, 17.25667341, 13.81563241, 11.12334116, 0.39893617],
                  [30.73344464, 24.23101005, 17.69426876, 14.10174533, 11.47081532, 0.287234043]],
  
  "Narrabri": [[31.59485018, 27.64179193, 23.59898525, 19.39753469, 15.67499419, 1.1],
               [31.57251755, 27.61627755, 23.85124715, 19.68272448, 15.76887739, 1.088888889],
               [31.50468529, 27.52159077, 24.14854077, 19.9509556, 16.1408542, 1.068888889],
               [31.30662089, 27.38204638, 23.06715547, 19.84737824, 16.51212356, 1],
               [31.24176526, 27.32212461, 22.78794653, 19.41981319, 16.36228067, 0.891111111],
               [31.40839108, 27.52048671, 22.82352143, 19.32930771, 16.16856665, 0.844444444],
               [31.78960918, 27.87514714, 22.91760159, 19.37021004, 16.17525928, 0.784444444],
               [33.09667759, 28.55286194, 23.20280434, 19.56387312, 16.30983017, 0.7],
               [34.93827627, 29.58351103, 23.70956958, 19.93579252, 16.59521388, 0.615555556]]}')


states <- c("Queensland", "New South Wales", "Northern Territory", "Victoria", "Tasmania", "Western Australia", "South Australia")


surls  <- c("ftp://ftp.bom.gov.au/anon/gen/fwo/IDQ11295.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDN11060.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDD10207.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDV10753.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDT16710.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDW14199.xml",
            "ftp://ftp.bom.gov.au/anon/gen/fwo/IDS10044.xml")

forecastURLs <- data.frame(State =states, URL = surls, stringsAsFactors = F)
forecastStoreDF <- data.frame(State=states, forecast=NA, stringsAsFactors = F)
  
 


soilTypes <- list("11"="Forest Hill",
                  "12"="Forest Hill",
                  "21"="Forest Hill",
                  "22"="Forest Hill",
                  "31"="Forest Hill",
                  "41"="Forest Hill",
                  "42"="Forest Hill",
                  "51"="Quirindi",
                  "61"="Forest Hill",
                  "71"="Forest Hill",
                  "72"="Forest Hill",
                  "81"="Quirindi",
                  "82"="Quirindi",
                  "91"="Narrabri",
                  "92"="Hudson",
                  "93"="Hudson",
                  "94"="Hudson",
                  "101"="Quirindi")





