library(shiny)
library(stringr)
library(dygraphs)
library(rhandsontable)
library(xts)
library(dplyr)
library(plyr)



dataDF <- read.csv('/mnt/data/APILogs/SoilDataFederator/soilDataFederatorUsage.csv')

ui <- fluidPage(
    
    titlePanel("SDF Stats"),
    
    sidebarLayout(
        sidebarPanel( width = 2,
                      selectInput("reportType",
                                  "Report",
                                  choices = c('Number of downloads per user', 
                                              'Most popular soil attributes',
                                              'Hits per Dataset',
                                              'Downloads per Dataset',
                                              'Hits per day'),
                                  selected = 'Number of downloads per user')
                      
        ),
        
        mainPanel(
            
            tableOutput('outTable'),
            dygraphOutput("tsPlot",width = "100%", height = "800px")
        )
    )
)


server <- function(input, output, session) {
    
    RV <- reactiveValues()
    RV$currentDF <- NULL
    RV$currentTS <- NULL
    
    output$outTable <- renderTable({ 
        RV$currentDF
    })
    
    output$tsPlot <- renderDygraph({
        
        req(RV$currentTS)
        mts <- RV$currentTS
        
        p <- dygraph(mts, main="") %>%
            dyRangeSelector(dateWindow = c(start(mts), end(mts)))%>%
            dyLegend(width = 200) #%>%
            #dyOptions(colors = c('black', 'blue', 'green', 'red', 'yellow', 'gray', 'orange', 'purple'))
    })
    
    observe({
        
        req(input$reportType)
        
        if(input$reportType== 'Number of downloads per user'){
            grpStats <- ddply(dataDF, .(User), summarize, Downloads=sum(Count))
            grpStats <- grpStats[order(grpStats$Downloads, decreasing=TRUE),]
            bits <- str_split(grpStats$User, '@')
            u <- sapply(bits, function (x) x[1])
            dom <- sapply(bits, function (x) x[2])
            cu <- str_sub(u, 1,4)
            pu <- paste0(cu, ' from ', dom)
            odf <- data.frame(User = pu, Downloads=grpStats$Downloads )
            RV$currentTS <- NULL
            RV$currentDF <- odf
        }else if(input$reportType== 'Most popular soil attributes'){
            grpStats <- ddply(dataDF, .(Attribute), summarize, Downloads=sum(Count))
            grpStats <- grpStats[order(grpStats$Downloads, decreasing=TRUE),]
            RV$currentTS <- NULL
            RV$currentDF <- grpStats
        }else if(input$reportType== 'Downloads per Dataset'){
            grpStats <- ddply(dataDF, .(Dataset), summarize, Downloads=sum(Count))
            grpStats <- grpStats[order(grpStats$Downloads, decreasing=TRUE),]
            RV$currentTS <- NULL
            RV$currentDF <- grpStats
        }else if(input$reportType== 'Hits per Dataset'){
            grpStats <- ddply(dataDF, .(Dataset), summarize, Downloads=length(Count))
            grpStats <- grpStats[order(grpStats$Downloads, decreasing=TRUE),]
            RV$currentTS <- NULL
            RV$currentDF <- grpStats
        }else if(input$reportType== 'Hits per day'){
            bits <- str_split(dataDF$DateTime, ' ')
            dts <- sapply(bits, function (x) x[1])
            odf <- cbind(dataDF, dts)
            print(head(odf))
            grpStats <- ddply(dataDF, .(dts), summarize, Downloads=length(Count))
            TS <- xts(grpStats[, -1], order.by = as.Date(grpStats$dts))
            #grpStats <- grpStats[order(grpStats$Downloads, decreasing=TRUE),]
            RV$currentDF <- NULL
            RV$currentTS <- TS
        }
       

    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
