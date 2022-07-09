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
library(shinyWidgets)
library(shinyalert)


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

######  UI initilise   ####### 

shiny::shinyApp(
  ui = f7Page(
    #options = list(dark = F, pullToRefresh = TRUE),
    options = list(dark = F),
    title = AppName,
    tags$head(tags$link( rel="icon", type="image/png", href="ANSISAppLogo.png", sizes="32x32" )),
    
    allowPWA = F,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    

    pwa("https://shiny.esoil.io/Apps/ANSISApp/",  title = AppName, output = "www", icon='www/SoilProfile.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),

    #use_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
   # add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    busy_start_up(
      loader = tags$img(src = "SplashScreen2.png", width = 200),
      text =  "Please wait while we rangle over 60,000 soil profiles for you .....",
      #mode = "manual",
      #mode = "timeout",
      mode = "auto"
      #timeout = 20000
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
          
######  UI Get data for 1 site   #######       
      
f7Float(
        side = "left",
          #tags$div( style=paste0("width: ", defWidth),  
          tags$div(  
                    f7Card(
                      outline = F,
                      title = NULL,
                      HTML('Select a soil site marker to show data'),
                      shinycssloaders::withSpinner(leafletOutput("mainMap", height = 470, width = defWidth), hide.ui = FALSE),
                      HTML('<BR>'),
                      HTML('<BR>')
                    ),
                    uiOutput("card_SoilPropertyData"), # This card is dynamically changed below based on the device
                   
                    f7Card(
                      title = "All Profile Data",
                      rHandsontableOutput('UI_AllSiteInfo'),
                      HTML('<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>')
                    )
                    
              )
         # )
        )),
######  UI Compare Soil Sites   ####### 
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
                  #textOutput('UI_compareProgText'),
                  progressBar(
                    id = "pb",
                    value = 0,
                    total = NumberOfCompareSites,
                    title = "",
                    display_pct = F
                  ),
                  
                ),
                uiOutput("card_compareSoilPropertyData"), # This card is dynamically changed below based on the device
                
                f7Card(
                  title = "All Profile Data",
                  rHandsontableOutput('UI_compareAllSiteInfo' ),
                  HTML('<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>')
                )
            )
        )
      )),
     


####  Javascript   #######     
      tags$head(tags$script(
        'Shiny.addCustomMessageHandler("scrollToBottom",
                                  function(NULL) {
                                 
                                   //  window.scrollTo(0, document.body.scrollHeight || document.documentElement.scrollHeight);
                                    var element = document.getElementById("UI_SoilProfilePlot");
                                    element.scrollIntoView({behavior: "smooth", block: "end", inline: "nearest"});
                                    // alert("Here");
                                  });
        Shiny.addCustomMessageHandler("scrollToBottomCompare",
                                  function(NULL) {
                                 
                                   //  window.scrollTo(0, document.body.scrollHeight || document.documentElement.scrollHeight);
                                    var element = document.getElementById("UI_compareBoxPlot");
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

  
  ####___________________________________####
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
    
    
    ##### ___________________________ #### 
    #### Render Dynamic GUI  ###########
    
    observe({
      
      
      req(input$deviceInfo$desktop)
      if(input$deviceInfo$desktop){
        print("RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR")
        shinyalert(title = "For Your Info", type = "info", 
                   html=T,
                   text='We noticed you are running this Web App on your PC web browser. 
                   That is fine but it is optimised for viewing on mobile devices so it may look a little weird on your PC. 
                   Copy and paste this URL into your mobile device browser to install it as a Web App.')
      # report_info(
      #   title= 'FYI',
      #   text = 'This web App' ,
      #   button = "Ok",
      #   # type = "info",
      #   session = session
      # )
      }
    })
    
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
        print(paste0('App is running in development mode'))
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
          
          pdf = getVocabNames(df, props)
          RV$CurrentProps <- pdf
          print(pdf)
          
          if(input$deviceInfo$desktop) {
            updateSelectInput(inputId = 'UI_SoilProps', choices = as.character(pdf$VocName))
          }else{
            updateF7Picker(inputId = 'UI_SoilProps', choices = as.character(pdf$VocName))
          }

         # cnames[i] <- propName
        }
        # cnames <- cnames[nzchar(cnames)] #removes blanks
        # RV$CurrentProps <- data.frame(LabMethod=apiProps, VocName=cnames)
        # 
        # if(input$deviceInfo$desktop) {
        # updateSelectInput(inputId = 'UI_SoilProps', choices = cnames)
        # }else{
        #   updateF7Picker(inputId = 'UI_SoilProps', choices = cnames, value = cnames[1],)
        # }
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
      
     # remove_start_up(timeout = 200)
      
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
    
 
    
    
    
    
    
    
       
####_______________________________####    
########################################   Compare Site To Surrounding Sites  #####################################
    ####_####  
    
    RVC <- reactiveValues()

    RVC$AllSiteInfo=NULL
    RVC$compareCurrentSiteHeader=NULL
    RVC$compareCurrentProps=NULL
    RVC$BoxPlotData=NULL
    RVC$CompareCurrentSite=NULL

########   Render Dynamic UI  #################################################
    output$card_compareSoilPropertyData <- renderUI({
      req(input$deviceInfo)

      if(input$deviceInfo$desktop) {

        f7Card(
          title = NULL,
          selectInput('UI_compareSoilProps', label ='', choices = c('None'), width = defWidth),
          htmlOutput('UI_compareSoilInfoHeader'),
          plotOutput('UI_compareBoxPlot'),
          htmlOutput('UI_compareSoilInfoSummary'),
          HTML('<BR>'),
          rHandsontableOutput('UI_comparePropInfo' )
        )
      }else{
        f7Card(
          title = NULL,
          f7Picker(inputId='UI_compareSoilProps', label ='Soil Property', choices = c('None', 'None.'), placeholder = "Soil property values", openIn = "auto", value='None'), ## weird bug - you need to pecify a blank list to get the list items update to work
          htmlOutput('UI_compareSoilInfoHeader'),
          plotOutput('UI_compareBoxPlot'),
          htmlOutput('UI_compareSoilInfoSummary'),
          HTML('<BR>'),
          rHandsontableOutput('UI_comparePropInfo' )
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
      
      
      session$sendCustomMessage(type="scrollToBottomCompare",message=list(NULL))
      
print(p)
      bits <- str_split(p, ' - ')
      dataset <- bits[[1]][1]
      sid <- bits[[1]][2]

#if(!devel){

    sx <- p$lng
    sy <- p$lat

    updateF7Progress(id = "UI_comparepg1", value = (10))
    updateProgressBar(
      session = session,
      id = "pb",
      value = 2, 
      total = NumberOfCompareSites,
      title = paste0('Finding the ', NumberOfCompareSites, ' closest soil data sites...')
    )
     sdf <- fromJSON(paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Locations?longitude=", sx,"&latitude=", sy ,"&propertytype=LaboratoryMeasurement&closest=", NumberOfCompareSites+1,"&usr=TrustedDemo&key=jvdn64df"))
     rec <- sdf[1,]
     url <- paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=', rec$DataSet ,'&siteid=', rec$Location_ID ,'&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df')
     #print(url)
     ccdf <- fromJSON(URLencode(url))
     
     RVC$CompareCurrentSite <- ccdf
     #print(ccdf)
     
        odf<-data.frame()
        for (i in 2:nrow(sdf)) {

          rec <- sdf[i,]
          url <- paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=', rec$DataSet ,'&siteid=', rec$Location_ID ,'&propertytype=LaboratoryMeasurement&tabletype=narrow&usr=TrustedDemo&key=jvdn64df')
          df <- fromJSON(URLencode(url))
          
          if(is.null(df$error)){
            if(nrow(df)>0){
              odf<-rbind(odf, df)
              updateF7Progress(id = "UI_comparepg1", value = (i*as.integer(100/NumberOfCompareSites)))
              
              updateProgressBar(
                session = session,
                id = "pb",
                value = i, 
                total = NumberOfCompareSites,
                title = paste("Getting data from ", rec$Location_ID)
              )
              
            }
          }
        }
   # }else{
   #   
   #   odf <- read.csv(paste0(appRootDir,'/Data/tmp/compData.csv'), stringsAsFactors = F)
   #   
   # }
      #write.csv(odf, 'c:/temp/compData.csv', row.names = F)

      updateF7Progress(id = "UI_comparepg1", value = 0)
      updateProgressBar(
        session = session,
        id = "pb",
        value = 0, 
        total = NumberOfCompareSites,
        title = ''
      )

      idxs <- which(odf$UpperDepth==0)
      
      
      RVC$AllSiteInfo <- odf[idxs,]
      # obs = as.data.frame(odf[idxs,] %>% group_by(ObservedProperty)  %>% summarise(n()))
      # print(obs)
      # idxxs <- which(obs[,2] >= 1)
      # obsToDo <- obs[idxxs,]
      # RVC$BoxPlotData <- as.numeric(odf[odf$UpperDepth==0 & odf$ObservedProperty == '4A1', ]$Value)
    })
    
    
#######   Update Compare items combo choices  ###############
    observe({
      req(RVC$CompareCurrentSite)
      siteProps <- unique(RVC$CompareCurrentSite[RVC$CompareCurrentSite$UpperDepth==0,]$ObservedProperty)
      #print(siteProps)
      siteProps <- siteProps[!is.na(siteProps)]
      if(input$deviceInfo$desktop) {
        updateSelectInput(inputId = 'UI_compareSoilProps', choices = as.character(siteProps))
      }else{
        updateF7Picker(inputId = 'UI_compareSoilProps', choices = as.character(siteProps))
      }
    })
    

     
    
####### Plot the BoxPlot  #######    
    output$UI_compareBoxPlot <- renderPlot({
      req(input$UI_compareSoilProps, RVC$CompareCurrentSite)
      if(input$UI_compareSoilProps != 'None'){
        
        idx <- which(RVC$CompareCurrentSite$UpperDepth==0 & RVC$CompareCurrentSite$ObservedProperty == input$UI_compareSoilProps)
        siteVal <- as.numeric(RVC$CompareCurrentSite[idx,]$Value)
        print(siteVal)
        idxs <- which(RVC$AllSiteInfo$UpperDepth==0 & RVC$AllSiteInfo$ObservedProperty == input$UI_compareSoilProps)
        propData <- RVC$AllSiteInfo[idxs,]
        RVC$compareCurrentProps <- propData
        bpData=as.numeric(propData$Value)
        output$UI_compareSoilInfoSummary <- renderText({ paste0("Selected site value = ", siteVal, 
                                                                "<BR>No. Sites = ", nrow(propData),
                                                                 "<BR>Mean = ", mean(bpData),
                                                                 "<BR>Maximum = ", max(bpData),
                                                                 "<BR>Minimum = ", min(bpData)
                                                                )})
        
       plotBoxPlot(vals=bpData, obsVal=siteVal, Title='Test')
        
        
      }
    })
    
    
    
#####  Render the property data table #####
    output$UI_compareAllSiteInfo = renderRHandsontable({
      req(RVC$AllSiteInfo)
      
      if(nrow(RVC$AllSiteInfo) > 0){
        df <- RVC$AllSiteInfo
        odf <- data.frame(siteID=df$Location_ID, UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
        rhandsontable(odf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }
    })
    
#####  Render the Compare All site data table #####
    output$UI_comparePropInfo = renderRHandsontable({
      req(RVC$compareCurrentProps)
      
      if(nrow(RVC$compareCurrentProps) > 0){
        df <- RVC$compareCurrentProps
        odf <- data.frame(siteID=df$Location_ID, UD=df$UpperDepth, LD=df$LowerDepth, Property=df$ObservedProperty, Value=df$Value, Units=df$Units)
        rhandsontable(odf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
      }
    })
    
    
  }
)


