library(shiny)
library(shinyMobile)
library(shinyjs)
library(leaflet.extras)
library(leafem)
library(RCurl)
library(stringr)
library(jsonlite)
library(rhandsontable)
library(shiny.pwa)
library(shinybusy)



machineName <- as.character(Sys.info()['nodename'])

if(machineName=='soils-discovery'){
  appRootDir <- '/srv/shiny-server/Apps/ANSISApp'
}else{
  appRootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ANSISApp'
}

source(paste0(appRootDir, '/Config.R'))
source(paste0(appRootDir, '/Functions.R'))

props <- read.csv(paste0(appRootDir,'/Data/PropertyLookups.csv'), stringsAsFactors = F)




if(devel){
  Ausminx <- 151.7
  Ausminy <-  -26.85
  Ausmaxx <- 151.6
  Ausmaxy <- -25.9
}


########    USER INTERFACE  ########################################
####  .  ####

shiny::shinyApp(
  ui = f7Page(
    options = list(dark = F),
    title = NULL,
    
    allowPWA = F,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    
<<<<<<< HEAD
   pwa("https://shiny.esoil.io/Apps/ANSISApp/",  title = AppName, output = "www", icon='www/SoilProfile.png',
       offline_template = 'www/offline.html', offline_message='Sorry we are offline'),
=======
    pwa("https://shiny.esoil.io/Apps/ANSISApp/",  title = AppName, output = "www", icon='www/SoilProfile.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),
>>>>>>> 2e3046e5b4f88565be02e5b2544779aa92057b8e
    
    add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    busy_start_up(
      loader = tags$img(
        src = "SplashScreen.png",
        width = 200
      ),
      text = "Loading...",
      mode = "auto"
    ),
    
   #add_busy_bar(timeout = 1000,     color = "#112446",     centered = FALSE,     height = "18px"   ),
    
    f7TabLayout(
      navbar = f7Navbar(
        title = 'ANSIS Demo',
        hairline = T,
        shadow = F,
        leftPanel = T
      ),
      panels = tagList(
        f7Panel(title = "Info", side = "left", theme = "light", effect = "cover",
                
                f7Link(label = "About ANSIS", href = "https://www.csiro.au/en/research/natural-environment/land/soil/ansis")
        )),
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Profile",
          icon = f7Icon("layers_fill"),
          active = TRUE,
      
      f7Float(
        side = "left",
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          tags$div( style=paste0("width: ", defWidth),  
                    f7Card(
                      
                      title = NULL,
                      HTML('Select a soil site marker to show data'),
                      leafletOutput("mainMap", height = 470, width = 370),
                      HTML('<BR>'),
                      f7Block(
                        f7Progress(id = "pg1", value = 10, color = "blue")
                      ),
                      HTML('<BR>'),
                    ),
                    uiOutput("card_SoilPropertyData"), # This card is dynamically changed below based on the device
                   
                    f7Card(
                      title = "All Profile Data",
                      rHandsontableOutput('UI_AllSiteInfo' )
                    ),
                    
              )
          )
        )),
      
      f7Tab(
        tabName = "Compare",
        icon = f7Icon("fa-street-view")
      
      )),
     
     
      
      tags$style('.progressbar {height: 15px;}'),
      
      tags$head(tags$script('
       $(document).ready(function () {
         navigator.geolocation.getCurrentPosition(onSuccess, onError);
               
         function onError (err) {
           Shiny.onInputChange("geolocation", false);
         }
               
         function onSuccess (position) {
           setTimeout(function () {
             var coords = position.coords;
             console.log(coords.latitude + ", " + coords.longitude);
             Shiny.onInputChange("geolocation", true);
             Shiny.onInputChange("lat", coords.latitude);
             Shiny.onInputChange("long", coords.longitude);
           }, 1100)
         }
       });
               ')      )
    )
  ),

  
  
  #######   Server Code  ###########   
  
  server = function(input, output, session) {
    
    RV <- reactiveValues()

    RV$CurrentSiteInfo=NULL
    RV$CurrentSiteHeader=HTML('<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>')
    RV$CurrentProps=NULL
    RV$SoilSites=NULL
    RV$CurrentPropdata=NULL
    
     output$card_SoilPropertyData <- renderUI({
       req(input$deviceInfo)
       
      if(input$deviceInfo$desktop) {
       
        f7Card(
          title = NULL,
          selectInput('UI_SoilProps', label ='', choices = c('None'), width = 370),
          htmlOutput('UI_SoilInfoHeader'),
          plotOutput('UI_SoilProfilePlot'),
          HTML('<BR><BR>'),
          rHandsontableOutput('UI_SiteInfo' )
        )
      }else{
        f7Card(
          title = NULL,
          f7Picker(inputId='UI_SoilProps', label ='Soil Property', choices = c('None')),
         # selectInput('UI_SoilProps', label ='', choices = NULL, width = 370),
          htmlOutput('UI_SoilInfoHeader'),
          plotOutput('UI_SoilProfilePlot'),
          HTML('<BR><BR>'),
          rHandsontableOutput('UI_SiteInfo' )
        )
      }
    })
    
    observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = RV$SoilSites, {
      if(devel){
        RV$SoilSites <- read.csv(paste0(appRootDir,'/Data/soilSites.csv'), stringsAsFactors = F)
      }else{
        
        url <- paste0(APIRoot, "/Site_Locations?DataSets=NatSoil&propertytype=LaboratoryMeasurement&bbox=", Ausminx,";", Ausmaxx,";", Ausminy,";", Ausmaxy, Auth)
        RV$SoilSites <- fromJSON(url)
      }
    })
    
    
   output$UI_SoilProfilePlot <- renderPlot({
     req(RV$CurrentPropdata)
     isolate(title <- input$UI_SoilProps)
     plotSoilProfileHBars(RV$CurrentPropdata, title)})
   
    
    #####   Update attribute selection list
    observe({
      req(RV$CurrentSiteInfo)
      {
        df <- RV$CurrentSiteInfo
        
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
        RV$CurrentProps <- data.frame(LabMethod=apiProps, VocName=cnames)
        
        if(input$deviceInfo$desktop) {
        updateSelectInput(inputId = 'UI_SoilProps', choices = cnames)
        }else{
          updateF7Picker(inputId = 'UI_SoilProps', choices = cnames)
        }
      }
    })
    
    
    output$UI_AllSiteInfo = renderRHandsontable({
      req(RV$CurrentSiteInfo)
      
      if(nrow(RV$CurrentSiteInfo) > 0){
        df <- RV$CurrentSiteInfo
        odf <- data.frame(UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
        rhandsontable(odf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }
    })
    
    output$UI_SiteInfo = renderRHandsontable({
      
      req(input$UI_SoilProps)
      
      
      if(!input$UI_SoilProps=='None'){
      
          if(nrow(RV$CurrentSiteInfo) > 0){
            
            item = input$UI_SoilProps
           
            mdf <- RV$CurrentProps
            labMethod = mdf[mdf$VocName==item,]$LabMethod
            df <- RV$CurrentSiteInfo[RV$CurrentSiteInfo$ObservedProperty==labMethod,]
            odf <- data.frame(UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
            
            RV$CurrentSiteHeader=paste0('<H1 id="ResultsHeader" style="color:blue; font-size:15px; font-weight:bold">',RV$CurrentSiteInfo[1,2] ,'</H1>',
                              '<p style="color: blue; font-size:10px; text-align:left; font-weight:normal">Data source : ', RV$CurrentSiteInfo[1,1], '</p>')
            
            RV$CurrentPropdata <- odf
            
            rhandsontable(odf,   manualColumnResize = F, readOnly = TRUE, rowHeaders = F)
    
          }else{
            return(NULL)
          }
      }
    })
    
        output$UI_SoilInfoHeader <- renderText({ RV$CurrentSiteHeader})

    customIcon <- makeIcon(
      iconUrl = "icons/marker-icon.png",
      iconAnchorX = 10, iconAnchorY = 40,
    )
    
    acm_defaults <- function(map, x, y) addMarkers(map, x, y, icon = customIcon, layerId="Selected" )
    # acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    # acm_defaults <- function(map, x, y) makeAwesomeIcon(map, x, y, icon = "fire", iconColor = "black", markerColor = "blue", library = "fa", layerId="Selected" )
    
    output$mainMap <- renderLeaflet({
      

      
      req(input$lat)
      
      m <-leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satelite Image") %>%
        addMouseCoordinates()  %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to full extent",
          onClick=JS("function(btn, map){ map.setView({lon: 135, lat: -28}, 3); }"))) %>%
        fitBounds(Ausminx, Ausminy, Ausmaxx, Ausmaxy) %>%

        
        
        addMarkers(data = RV$SoilSites, lng = ~Longitude, lat = ~Latitude, 
                 #popup = ~as.character(Location_ID),
                 layerId=~paste0(DataSet, ' - ', Location_ID),
                 label = ~paste0(DataSet, ' - ', Location_ID),
                 clusterOptions = markerClusterOptions()
      )
    })
    

    
    observeEvent(input$mainMap_marker_click, { # update the map markers and view on location selectInput changes
        
      p <- input$mainMap_marker_click
        if(is.null(p))
          return()
        
       resp <- getSoilSiteData(p)
        
        if(!is.null(resp$error)){
          f7Dialog(title = "Oops", text = resp$error)
          return()
        }
        
        idxs <- which(!is.na(resp$ObservedProperty))
        RV$CurrentSiteInfo <- resp[idxs, ]
    })
    
    
    
  }
)


