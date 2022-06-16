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


url <- paste0(APIRoot, "/Site_Locations?DataSets=NatSoil&propertytype=LaboratoryMeasurement&bbox=", Ausminx,";", Ausmaxx,";", Ausminy,";", Ausmaxy, Auth)
print(url)
#SoilSites <- fromJSON(url)


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
    
    allowPWA = FALSE,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    
    pwa("https://shiny.esoil.io/ANSISApp/",  title = AppName, output = "www", icon='www/Nicon.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),
    
    add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
 
    
    f7SingleLayout(
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
      
      f7Float(
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          tags$div( style=paste0("width: ", defWidth),  
                    f7Card(
                      title = NULL,
                      HTML('Select a location by clicking on the map'),
                      leafletOutput("mainMap", height = 470, width = 370),
                      HTML('<BR>'),
                    ),
                    f7Card(
                      title = NULL,
                      selectInput('UI_SoilProps', label ='', choices = NULL, width = 370),
                      htmlOutput('UI_SoilInfoHeader'),
                      rHandsontableOutput('UI_SiteInfo' )
                    ),
                    f7Card(
                      title = "All Profile Data",
                      rHandsontableOutput('UI_AllSiteInfo' )
                    )
              )
          )
        ),
      
      
      uiOutput("ui"),
      
      
      #######   Cookie Management  ########### 
      
      tags$head(tags$script(src="js.cookie.js")),
      tags$head(tags$script(
        HTML('
        Shiny.addCustomMessageHandler ("readCookie",function (message) {
        var cookie = readCookie(message.name);
        Shiny.onInputChange("cookie", cookie);
      })

      function readCookie(name) {
        var nameEQ = name + "=";
        var ca = document.cookie.split(";");
        for(var i=0;i < ca.length;i++) {
                var c = ca[i];
                while (c.charAt(0)==" ") c = c.substring(1,c.length);
                if (c.indexOf(nameEQ) == 0) return      c.substring(nameEQ.length,c.length);
        }   
        return ""; }'))),
      
      tags$head(tags$script(
        HTML('
         Shiny.addCustomMessageHandler ("writeCookie",function (message) {
         const d = new Date();
         d.setTime(d.getTime() + (365*24*60*60*1000));
         let expires = "expires="+ d.toUTCString();
         document.cookie = "HPCAuth=" + message.name + ";" + expires + ";"           
      })
      '))        ),
     
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
    
    
    observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = RV$SoilSites, {
      
      if(devel){
        RV$SoilSites <- read.csv('c:/temp/soilSites.csv', stringsAsFactors = F)
      }else{
        RV$SoilSites <- fromJSON(url)
      }
    })
    
    
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
        updateSelectInput(inputId = 'UI_SoilProps', choices = cnames)
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
      
      if(nrow(RV$CurrentSiteInfo) > 0){
        
        item = input$UI_SoilProps
       
        mdf <- RV$CurrentProps
        labMethod = mdf[mdf$VocName==item,]$LabMethod
        df <- RV$CurrentSiteInfo[RV$CurrentSiteInfo$ObservedProperty==labMethod,]
        odf <- data.frame(UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
        
        RV$CurrentSiteHeader=paste0('<H1 id="ResultsHeader" style="color:blue; font-size:15px; font-weight:bold">',RV$CurrentSiteInfo[1,2] ,'</H1>',
                          '<p style="color: blue; font-size:10px; text-align:left; font-weight:normal">Data source : ', RV$CurrentSiteInfo[1,1], '</p>')
        
        rhandsontable(odf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
        #RV$CurrentSiteHeader <- RV$CurrentSiteInfo[1,1]
        
        
      }else{
        return(NULL)
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


