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
library(dplyr)


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
    #options = list(dark = F, pullToRefresh = TRUE),
    options = list(dark = F),
    title = NULL,
    
    allowPWA = F,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    

    pwa("https://shiny.esoil.io/Apps/ANSISApp/",  title = AppName, output = "www", icon='www/SoilProfile.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),

    #use_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
   # add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    busy_start_up(
      loader = tags$img(src = "SplashScreen2.png", width = 200),
      text =  "Please wait while we rangle over 60,000 soil profiles for you .....",
      mode = "manual"
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
          tags$div( style=paste0("width: ", defWidth),  
                    f7Card(
                      outline = F,
                      title = NULL,
                      HTML('Select a soil site marker to show data'),
                      shinycssloaders::withSpinner(leafletOutput("mainMap", height = 470, width = defWidth)),
                      HTML('<BR>'),
                      # f7Block(
                      #   f7Progress(id = "pg1", value = 10, color = "blue")
                      # ),
                      HTML('<BR>'),
                    ),
                    uiOutput("card_SoilPropertyData"), # This card is dynamically changed below based on the device
                   
                    f7Card(
                      title = "All Profile Data",
                      rHandsontableOutput('UI_AllSiteInfo')
                    )
                    
              )
         # )
        )),
      
      f7Tab(
        tabName = "Compare",
        icon = f7Icon("fa-street-view"),
       
        
        f7Float(
          side = "left",
          
            tags$div( style=paste0("width: ", defWidth),  
                f7Card(
                  outline = F,
                  title = NULL,
                  HTML('Select a soil site marker to compare data'),
                  leafletOutput("UI_compareMap", height = 470, width = defWidth),
                  HTML('<BR>'),
                  f7Block(
                    f7Progress(id = "UI_comparepg1", value = 0, color = "blue")
                  ),
                  HTML('<BR>'),
                ),
                uiOutput("card_compareSoilPropertyData"), # This card is dynamically changed below based on the device
                
                f7Card(
                  title = "All Profile Data",
                  rHandsontableOutput('UI_compareAllSiteInfo' )
                )
            )
        )
      )),
     
     
      tags$head(tags$script(
        'Shiny.addCustomMessageHandler("scrollToBottom",
                                  function(NULL) {
                                 
                                   //  window.scrollTo(0, document.body.scrollHeight || document.documentElement.scrollHeight);
                                    var element = document.getElementById("UI_SoilProfilePlot");
                                    element.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                                    // alert("Here");
                                  });'
      )),
      
      tags$style('.progressbar {height: 15px;}'),
      
    #  tags$style(".shinybusy-ready {z-index: 9995 !important;}"),
      
      # tags$head(tags$script('
      #  $(document).ready(function () {
      #    navigator.geolocation.getCurrentPosition(onSuccess, onError);
      #          
      #    function onError (err) {
      #      Shiny.onInputChange("geolocation", false);
      #    }
      #          
      #    function onSuccess (position) {
      #      setTimeout(function () {
      #        var coords = position.coords;
      #        console.log(coords.latitude + ", " + coords.longitude);
      #        Shiny.onInputChange("geolocation", true);
      #        Shiny.onInputChange("lat", coords.latitude);
      #        Shiny.onInputChange("long", coords.longitude);
      #      }, 1100)
      #    }
      #  });
      #          ')      )
    )
  ),

  
  
  #######   SERVER CODE  ###########  
 
  
  server = function(input, output, session) {

#####  Single Profile Data Tab  ###############################################    
    
    RV <- reactiveValues()

    RV$CurrentSiteInfo=NULL
    RV$CurrentSiteHeader= NULL #HTML('<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>')
    RV$CurrentProps=NULL
    RV$SoilSites=NULL
    RV$CurrentPropdata=NULL
    RV$CurrentSiteLocation=NULL
    RV$WaitText=NULL
    
    
    ##### _ #### 
    #### Render Dynamic GUI  ###########
    
     output$card_SoilPropertyData <- renderUI({
       req(input$deviceInfo)
       
      if(input$deviceInfo$desktop) {
       
        f7Card(
          title = NULL,
          selectInput('UI_SoilProps', label ='', choices = c('None'), width = defWidth),
          htmlOutput('UI_SoilInfoHeader'),
          shinycssloaders::withSpinner( htmlOutput('UI_wait')),
          plotOutput('UI_SoilProfilePlot'),
          HTML('<BR><BR>'),
          rHandsontableOutput('UI_SiteInfo' )
        )
      }else{
        f7Card(
          title = NULL,
          f7Picker(inputId='UI_SoilProps', label ='Soil Property', choices = c('None', 'None.'), placeholder = "Soil property values", openIn = "auto", value='None'), ## weird bug - you need to pecify a blank list to get the list items update to work
          htmlOutput('UI_SoilInfoHeader'),
          shinycssloaders::withSpinner(htmlOutput('UI_wait')),
          plotOutput('UI_SoilProfilePlot'),
          HTML('<BR><BR>'),
          rHandsontableOutput('UI_SiteInfo'  )
        )
      }
    })

    
    ##### Fetch site locations  ####
    
    observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = RV$SoilSites, {
      if(devel){
        RV$SoilSites <- read.csv(paste0(appRootDir,'/Data/soilSites.csv'), stringsAsFactors = F)
      }else{
        url <- paste0(APIRoot, "/Site_Locations?DataSets=NatSoil&propertytype=LaboratoryMeasurement&bbox=", Ausminx,";", Ausmaxx,";", Ausminy,";", Ausmaxy, Auth)
        RV$SoilSites <- fromJSON(url)
      }
    })
    
  ##### Plot profile data for an attribute ####
   output$UI_SoilProfilePlot <- renderPlot({
     req(RV$CurrentProps,RV$CurrentPropdata)
             isolate(title <- input$UI_SoilProps)
             plotSoilProfileHBars(RV$CurrentPropdata, title)
         
     })
   
    
    #####   Update attribute selection list  ####
    observe({
      req(RV$CurrentSiteInfo)
      {
        if(nrow( RV$CurrentSiteInfo)>0){
          
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

          cnames[i] <- propName
        }
        cnames <- cnames[nzchar(cnames)] #removes blanks
        RV$CurrentProps <- data.frame(LabMethod=apiProps, VocName=cnames)
        
        #print(paste0('Combo Value is ', input$UI_SoilProps))
       
        
        if(input$deviceInfo$desktop) {
        updateSelectInput(inputId = 'UI_SoilProps', choices = cnames)
        }else{
          # cnames<-c("Total S - X-ray fluorescence", "Calcium phosphate-extractable S - ICPAES")
          # print(cnames)
          updateF7Picker(inputId = 'UI_SoilProps', choices = cnames, value = cnames[1],)

        }
      }
    })
    
    ##### Render all site data table  ####
    output$UI_AllSiteInfo = renderRHandsontable({
      req(RV$CurrentSiteInfo)
      
      if(nrow(RV$CurrentSiteInfo) > 0){
        df <- RV$CurrentSiteInfo
        odf <- data.frame(UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
        rhandsontable(odf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }
    })
    
    
    ##### Render Current soil property data table  ####
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
    
    ##### Render site info header  ####
    
        output$UI_SoilInfoHeader <- renderText({ RV$CurrentSiteHeader})
    
    
    ##### Get the soil site data from the Federator  ####
        output$UI_wait <- renderText({ 
          req(RV$WaitText)
         
          req(RV$CurrentSiteLocation)
          p <- RV$CurrentSiteLocation
          
          session$sendCustomMessage(type="scrollToBottom",message=list(NULL))
          resp <- getSoilSiteData(p)
          
          if(!is.null(resp$error)){
            f7Dialog(title = "Oops", text = resp$error)
            return()
          }
          
          idxs <- which(!is.na(resp$ObservedProperty))
          RV$CurrentSiteInfo <- resp[idxs, ]
          
          paste0('')
          })

    customIcon <- makeIcon(
      iconUrl = "icons/marker-icon.png",
      iconAnchorX = 10, iconAnchorY = 40,
    )
    
    acm_defaults <- function(map, x, y) addMarkers(map, x, y, icon = customIcon, layerId="Selected" )
    # acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    # acm_defaults <- function(map, x, y) makeAwesomeIcon(map, x, y, icon = "fire", iconColor = "black", markerColor = "blue", library = "fa", layerId="Selected" )
    
    
    ##### Render the main map  ####
    output$mainMap <- renderLeaflet({
      
      remove_start_up(timeout = 200)
      
      m <-leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satellite") %>%
        addMouseCoordinates()  %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to full extent",
          onClick=JS("function(btn, map){ map.setView({lon: 135, lat: -28}, 3); }"))) %>%
        fitBounds(Ausminx, Ausminy, Ausmaxx, Ausmaxy) %>%

        addMarkers(data = RV$SoilSites, lng = ~Longitude, lat = ~Latitude, 
                 layerId=~paste0(DataSet, ' - ', Location_ID),
                 label = ~paste0(DataSet, ' - ', Location_ID),
                 clusterOptions = markerClusterOptions()
      ) %>%
    
        # addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
        #                                    autoCenter = TRUE, maxZoom = 15, 
        #                                    setView = TRUE)) %>%
      
      addLayersControl(
        baseGroups = c("Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
    })
    

    ##### Map site click event  ####
    observeEvent(input$mainMap_marker_click, { # update the map markers and view on location selectInput changes
        
      p <- input$mainMap_marker_click
        if(is.null(p))
          return()
      RV$WaitText='Getting data'
      RV$CurrentSiteLocation <- p 
    })
    
 
    
    
    
    
    
    
       
####_####    
########################################   Compare Site To Surrounding Sites  #####################################
    ####_####  
    
    RVC <- reactiveValues()

    RVC$AllSiteInfo=NULL
    RVC$compareCurrentSiteHeader=NULL
    RVC$compareCurrentProps=NULL
    RVC$BoxPlotData=NULL

########   Render Dynamic UI  #################################################
    output$card_compareSoilPropertyData <- renderUI({
      req(input$deviceInfo)

      if(input$deviceInfo$desktop) {

        f7Card(
          title = NULL,
          selectInput('UI_compareSoilProps', label ='', choices = c('None'), width = defWidth),
          htmlOutput('UI_compareSoilInfoHeader'),
          plotOutput('UI_compareBoxPlot'),
          HTML('<BR><BR>'),
          #rHandsontableOutput('UI_SiteInfo' )
        )
      }else{
        f7Card(
          title = NULL,
          f7Picker(inputId='UI_compareSoilProps', label ='Soil Property', choices = c('None', 'None.'), placeholder = "Soil property values", openIn = "auto", value='None'), ## weird bug - you need to pecify a blank list to get the list items update to work
          htmlOutput('UI_compareSoilInfoHeader'),
          plotOutput('UI_compareBoxPlot'),
          HTML('<BR><BR>'),
          #rHandsontableOutput('UI_SiteInfo' )
        )
      }
    })
#     
#     # observe({
#     #   req(RV$CurrentSiteInfo )
#     #   session$sendCustomMessage(type="scrollToBottom",message=list(NULL))
#     # })
#     
# 
##    
    #### Render the compare map   #################################
    output$UI_compareMap <- renderLeaflet({


      mc <-leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = F), group = "Satellite") %>%
        addMouseCoordinates()  %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to full extent",
          onClick=JS("function(btn, map){ map.setView({lon: 135, lat: -28}, 3); }"))) %>%
        fitBounds(Ausminx, Ausminy, Ausmaxx, Ausmaxy) %>%
        
        addMarkers(data = RV$SoilSites, lng = ~Longitude, lat = ~Latitude,
                   layerId=~paste0(DataSet, ' - ', Location_ID),
                   label = ~paste0(DataSet, ' - ', Location_ID),
                   clusterOptions = markerClusterOptions()
        ) %>%
        
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = TRUE, maxZoom = 15, 
                                           setView = TRUE)) %>%
        
        addLayersControl(
          baseGroups = c("Map", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% hideGroup("Map") %>% showGroup( "Satellite")

    })

    
    ####  Get the soil data for comparisons   #######
    observeEvent(input$UI_compareMap_marker_click, { # update the map markers and view on location selectInput changes

      p <- input$UI_compareMap_marker_click
      if(is.null(p))
        return()

      print(p)

      bits <- str_split(p, ' - ')
      dataset <- bits[[1]][1]
      sid <- bits[[1]][2]
      print(head( RV$SoilSites))

if(!devel){

    sx <- p$lng
    sy <- p$lat

    updateF7Progress(id = "UI_comparepg1", value = (10))
     sdf <- fromJSON(paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Locations?longitude=", sx,"&latitude=", sy ,"&propertytype=LaboratoryMeasurement&closest=", NumberOfCompareSites,"&usr=TrustedDemo&key=jvdn64df"))

   print(sdf)

        odf<-data.frame()
        for (i in 1:nrow(sdf)) {
          print(i)
          rec <- sdf[i,]
          url <- paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=', rec$DataSet ,'&siteid=', rec$Location_ID ,'&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df')
          df <- fromJSON(URLencode(url))
          
          if(is.null(df$error)){
            if(nrow(df)>0){
              odf<-rbind(odf, df)
              updateF7Progress(id = "UI_comparepg1", value = (i*5))
            }
          }
        }
   }else{
     odf <- read.csv('c:/temp/compData.csv', stringsAsFactors = F)
   }
      #write.csv(odf, 'c:/temp/compData.csv', row.names = F)

      updateF7Progress(id = "UI_comparepg1", value = 0)

      idxs <- which(odf$UpperDepth==0)
      print(idxs)
      obs = as.data.frame(odf[idxs,] %>% group_by(ObservedProperty)  %>% summarise(n()))
      idxxs <- which(obs[,2] >= 1)
      obsToDo <- obs[idxxs,]
      RVC$BoxPlotData <- as.numeric(odf[odf$UpperDepth==0 & odf$ObservedProperty == '4A1', ]$Value)
    })
    
    
    
    output$UI_compareBoxPlot <- renderPlot({
      print("BoxPlot")
      
      print(RVC$BoxPlotData)
      req(RVC$BoxPlotData)
     # isolate(title <- input$UI_SoilProps)
      plotBoxPlot(vals=RVC$BoxPlotData, obsVal=6, Title='Test')
    })
    
    
  }
)


