
library(shiny)
library(dygraphs)
library(xts)
library(DBI)
library(leaflet)
library(scales)

source('utils.R')

allSites <- runQuery('select * from Sites')

ui <- fluidPage(


    titlePanel("SoilWaterNow - Model Averaging"),

 
    sidebarLayout(
        sidebarPanel(
            selectInput('UI_Site', label = 'Site', choices = allSites$SiteID, selected = ),
            
            leafletOutput("siteMap",width = 350, height = 350)
    ),

        mainPanel(
           dygraphOutput('TSChart'), 
           HTML('<BR>'), HTML('<BR>'),
           tableOutput('Stats')
        )
    )
)


server <- function(input, output) {

    RV <- reactiveValues()
    RV$currentTS <- NULL
    
    output$TSChart <- renderDygraph({
       
        req(input$UI_Site)
        
        sql <- paste0("select * from ProbeData where SiteID = '", input$UI_Site, "' and UpperDepth = 1000 order by Date")
        probeData <- runQuery(sql)
        probeData$Value <- rescale(x=probeData$Value, to=c(0, 100),from=c(min(probeData$Value), max(probeData$Value)))
        pTS <- xts(x=probeData$Value, order.by = as.Date(probeData$Date))

        
        sql <- paste0("select * from ModelValues where Model = 'AWRA' and SiteID = '", input$UI_Site, "' and Product = 'TotalBucket' order by Date")
        AwraData <- na.omit(runQuery(sql))
        AwraData$Value <- rescale(x=AwraData$Value, to=c(0, 100),from=c(min(AwraData$Value), max(AwraData$Value)))
        aTS <- xts(x=AwraData$Value, order.by = as.Date(AwraData$Date))
        
        sql <- paste0("select * from ModelValues where  Model = 'SMIPS' and SiteID = '", input$UI_Site, "' and Product = 'TotalBucket' order by Date")
        SmipsData <- runQuery(sql)
        SmipsData$Value <- rescale(x=SmipsData$Value, to=c(0, 100),from=c(min(SmipsData$Value), max(SmipsData$Value)))
        sTS <- xts(x=SmipsData$Value, order.by = as.Date(SmipsData$Date))
        
        ts1 <- xts::merge.xts(pTS, aTS, join = 'inner')
        ts2 <- xts::merge.xts(ts1, sTS, join = 'inner')
        ts2 <- na.omit(ts2)
        tsAll <- ts2
        colnames(tsAll) <- c('TotalProbe', 'AWRA', 'SMIPS')
      
        
        #tsAll$stdProbe <- 
        #tsAll$stdProbe <- (tsAll$TotalProbe - min(tsAll$TotalProbe))  / (max(tsAll$TotalProbe)-min(tsAll$TotalProbe))
        #tsAll$stdAWRA <- (tsAll$AWRA - min(tsAll$AWRA))  / (max(tsAll$AWRA)-min(tsAll$AWRA))
        #tsAll$stdSMIPs <- (tsAll$SMIPS - min(tsAll$SMIPS))  / (max(tsAll$SMIPS)-min(tsAll$SMIPS))

        RV$currentTS <- tsAll

        dygraph(tsAll[,1:3]) 
    })
    
    output$Stats <- renderTable({
         s <- cor(RV$currentTS)
         print(s)
         s
    }, rownames = TRUE)
    
    output$siteMap <- renderLeaflet({
        
        req(input$UI_Site)
        
        sql <- paste0("select * from Sites where SiteID = '", input$UI_Site, "'")
        rec <- runQuery(sql)
        
        lon <- rec$Longitude
        lat <- rec$Latitude

        leaflet::leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
            setView(lon, lat, zoom = 15)  %>%
            addProviderTiles("CartoDB.PositronOnlyLabels") %>%
            addMarkers(lon, lat)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
