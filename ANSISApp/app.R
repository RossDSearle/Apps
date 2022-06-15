library(shiny)
library(shinyMobile)
library(shinyjs)
library(leaflet.extras)
library(RCurl)
library(sf)
library(XML)
library(stringr)
library(jsonlite)
library(lubridate)
library(rhandsontable)
library(shiny.pwa)
library(shinybusy)


#appRootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/Nloss'
#appRootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/Nloss'

machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName=='soils-discovery'){
  appRootDir <- '/srv/shiny-server/Apps/ANSISApp'
}else{
  appRootDir <- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/Apps/ANSISApp'
}

print(paste0(appRootDir, '/Config.R'))

source(paste0(appRootDir, '/Config.R'))
#source(paste0(appRootDir, '/Functions.R'))

# ptX <- 149
# ptY <- -35
# lon=ptX
# lat=ptY

devel = F




#print(StateForecast)

########    USER INTERFACE  ########################################
####  .  ####

shiny::shinyApp(
  ui = f7Page(
    options = list(dark = F),
    title = NULL,
    
    allowPWA = FALSE,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    
    pwa("https://shiny.esoil.io/NLoss/",  title = AppName, output = "www", icon='www/Nicon.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),
    
    add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    
    f7SingleLayout(
      navbar = f7Navbar(
        title = 'Nitrogen Loss Estimator',
        hairline = T,
        shadow = F,
        leftPanel = T
      ),
      panels = tagList(
        f7Panel(title = "Info", side = "left", theme = "light", effect = "cover",
                
                f7Link(label = "About BARS", href = "https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station"),
                
               # f7Slider( inputId='UI_UreaCost',label=NULL, min = 0, max=1200, value=800)
               f7Select(inputId='UI_UreaCost',label='Urea Price', choices=seq(0, 1200, 10),selected = 800, width = 100)
        )),
      
      f7Float(
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          tags$div( style=paste0("width: ", defWidth),  
                    f7Card(
                      title = NULL,
                      HTML('Select a location by clicking on the map'),
                      leafletOutput("mainMap", height = 400, width = 400),
                      HTML('<BR>'),
                      f7Select(inputId='UI_UreaRate',label='Application Rate (kg/Ha)', choices=seq(0, 300, 10),selected = 100, width = 300),
                      HTML('<BR>'),
                      f7Button("UI_Check", paste0("Check Potential Nitrogen Loss")),
                      
                    ),
                    f7Card(
                      title = NULL,
                      htmlOutput('UI_Results'),
                      rHandsontableOutput('forecastTable' )
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
    
    RV$CurrentLongitude=NULL
    RV$CurrentLatitude=NULL
    
    
    output$forecastTable = renderRHandsontable({
      req(RV$CurrentForecast)
      if(nrow(RV$CurrentForecast) > 0){
        rhandsontable(RV$CurrentForecast,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }else{
        return(NULL)
      }
    })
    
    
    observeEvent(input$UI_Check, {
      req(input$long)
     
      print (paste0("Checking N Loss risk for Longitude = ", RV$CurrentLongitude, " and Latitude = ", RV$CurrentLatitude))
     

    })
    
    output$UI_Results <- renderText({ RV$Results })
    
    observe({
      req(input$long)
      RV$CurrentLongitude=input$long
      RV$CurrentLatitude=input$lat
    })

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
        setView(lng = input$long, lat = input$lat, zoom = 12) %>%
        acm_defaults(x=input$long, y=input$lat)
      
    })
    
    
    observeEvent(input$mainMap_click, { # update the map markers and view on location selectInput changes
        p <- input$mainMap_click
        if(is.null(p))
          return()
        
        proxy <- leafletProxy("mainMap")
        proxy %>% acm_defaults(p$lng, p$lat)
        RV$CurrentLongitude=p$lng
        RV$CurrentLatitude=p$lat
    })
    
    
    
  }
)


