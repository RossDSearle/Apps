
machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){

}else{
 
}

AppName = 'ANSISApp'
Auth = '&usr=TrustedDemo&key=jvdn64df'

APIRoot <- 'https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI'

soilTableType = 'narrow'


vocURLs <- c('https://vocabs.ardc.edu.au/repository/api/lda/csiro/soil-profile-classifiers/3ed/resource',
             'https://vocabs.ardc.edu.au/repository/api/lda/ub/soil-chemical-methods-australasia/2011/resource')
vocTypes <- c('FieldMeasurement', 'LaboratoryMeasurement')
vocPaths <- data.frame(vocTypes=vocTypes,vocURL=vocURLs )

defWidth = 350

devel = T

Ausminx <- 112.905
Ausminy <-  -43.735
Ausmaxx <- 154.005
Ausmaxy <- -9.005

NumberOfCompareSites = 10




