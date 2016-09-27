MonthlyLFSx1 <- readRDS("Data/data1.rds")
MonthlyLFSx2 <- readRDS("Data/data2.rds")

library(dplyr)
library(ggplot2)
library(CANSIM2R) # this package downloads CANSIM tables
library(Hmisc) # need this package to read labels
library(zoo) #to deal with dates that only have year and month, with no days
library(shiny)
library(DT)
library(car)
library(rmarkdown)
library(rCharts)
library(rsconnect)

server <- function(input, output) {
  
  output$LFSTable <- DT::renderDataTable(MonthlyLFSx1 %>% 
                                           filter(t==input$RefMonth),
                                         # colnames = c('Organization ID','Engagement',
                                         #              'Count 2013','Count 2015',
                                         #              'Net Gain/Loss','% Change',
                                         #              'Organization'), # headers
                                         rownames = FALSE,
                                         extensions = 'Buttons',
                                         class = 'cell-border stripe hover', #styling opts.
                                         options=list(
                                           dom='Bt ', # only show buttons and table, hence Bt
                                           buttons = 
                                             list('copy', 'print', list(
                                               extend = 'collection',
                                               buttons = c('csv', 'excel'),
                                               text = 'Download Data'
                                             )),
                                           columnDefs = list(
                                             list(targets={{1}},visible = FALSE)
                                           ) # hide second column
                                         ) 
  ) 
  
  
  output$ReferenceMonth <- renderText( # displaying reactive output
    {
      
      paste("The reference month is:", input$RefMonth)
      
    }
  )
  
  output$UR <- renderText( 
    {
      
      paste("The current unemployment rate is: ", MonthlyLFSx1 %>% 
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(UnRate),
            "%",sep="")
      
      
    }
  )
  
  output$URLast <- renderText(
    {
      
      paste("The unemployment rate last month was: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(URLast),
            "%",sep="")
      
      
    }
  )
  
  output$URLastYear <- renderText(
    {
      
      paste("The unemployment rate last year was: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(URLastYear),
            "%",sep="")
      
      
    }
  )
  
  output$EmpChangeMonth <- renderText(
    {
      
      paste("since last month, employment changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmp15Over),
            sep="")
      
      
    }
  )
  
  output$LFChangeMonth <- renderText(
    {
      
      paste("since last month, labour force changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMLF15Over),
            sep="")
      
      
    }
  )
  
  output$EmpChangeYear <- renderText(
    {
      
      paste("since last year, employment changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(YoYEmp15Over),
            sep="")
      
      
    }
  )
  
  output$LFChangeYear <- renderText(
    {
      
      paste("since last year, labour force changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(YoYLF15Over),
            sep="")
      
      
    }
  )
  
  output$FTMonth <- renderText(
    {
      
      paste("since last month, full-time jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpFT15Over),
            sep="")
      
      
    }
  )
  
  output$PTMonth <- renderText(
    {
      
      paste("since last month, part-time jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpPT15Over),
            sep="")
      
      
    }
  )
  
  output$FTMonth1524 <- renderText(
    {
      
      paste("since last month, full-time jobs for 15-24 changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpFT1524),
            sep="")
      
      
    }
  )
  
  output$FTMonth2554 <- renderText(
    {
      
      paste("since last month, full-time jobs for 25-54 changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpFT2554),
            sep="")
      
      
    }
  )
  
  output$FTMonth55Over <- renderText(
    {
      
      paste("since last month, full-time jobs for 55 & over changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpFT54Over),
            sep="")
      
      
    }
  )
  
  output$Public <- renderText(
    {
      
      paste("since last month, public sector jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpPublic),
            sep="")
      
      
    }
  )
  
  output$Private <- renderText(
    {
      
      paste("since last month, private sector jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpPrivate),
            sep="")
      
      
    }
  )
  
  output$Self <- renderText(
    {
      
      paste("since last month, self employment changed by : ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="British Columbia") %>%
              select(MoMEmpSelf),
            sep="")
      
      
    }
  )
  
  output$CanUR <- renderText(
    {
      
      paste("In Canada, the current unemployment rate is: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(UnRate), "%",
            sep="")
      
      
    }
  )
  
  output$CanURLast <- renderText(
    {
      
      paste("In Canada, the unemployment rate last month was: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(URLast), "%",
            sep="")
      
      
    }
  )
  
  output$CanMoMEmp <- renderText(
    {
      
      paste("In Canada, since last month, employment changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(MoMEmp15Over),
            sep="")
      
      
    }
  )
  
  output$CanFT <- renderText(
    {
      
      paste("In Canada, full-time jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(MoMEmpFT15Over),
            sep="")
      
      
    }
  )
  
  output$CanPT <- renderText(
    {
      
      paste("In Canada, part-time jobs changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(MoMEmpPT15Over),
            sep="")
      
      
    }
  )
  
  output$CanLF <- renderText(
    {
      
      paste("In Canada, labour force changed by: ", MonthlyLFSx1 %>%
              filter(t==input$RefMonth) %>%
              filter(i=="Canada") %>%
              select(MoMLF15Over),
            sep="")
      
      
    }
  )
  
  # Shiny code to enter charts and graphs
  output$URPlot <- renderChart({
    
    # Render a discrete bar graph using rChart and the java script package nvd3
    
    n1 <- nPlot(x= 'i', y = 'UnRate', data = MonthlyLFSx1 %>% filter(t==input$RefMonth), 
                type = 'discreteBarChart', dom = 'URPlot')
    n1$yAxis(axisLabel = "%", width=40) # width can't exceed 64; won't show otherwise
    n1$chart(color = c('#263359', '#335926', '#592633', '#263359', '#263359', '#263359', '#263359', '#263359', 
                       '#263359', '#263359', '#263359'))
    
    # Commented code below is handy for multile bar chart type as opposed to discrete
    #n1$chart(color = "#! function(d){ return 'blue'} !#")
    #n1$chart(reduceXTicks = FALSE, showLegend = FALSE, showControls = FALSE) # this is only applicable for multipleBarCharts 
    
    n1$xAxis(staggerLabels = TRUE) # Staggering axis labels; otherwise some labels won't show
    n1
    
    
  })
  
  output$TimeSeries <- renderChart({
    
    # Render a discrete bar graph using rChart and the java script package nvd3
    
    # r1 <- Rickshaw$new()
    # r1$layer(V922 ~ Var2, data = MonthlyLFSx2, group = "i", type="line")
    # r1$set(slider = TRUE, dom="TimeSeries")
    # return(r1)
    
    
    n2 <- nPlot(UnRate ~ Date, 
                data = MonthlyLFSx2 %>% filter(i=="British Columbia" | i=="Canada"), group = "i", 
                type = 'lineWithFocusChart', dom = "TimeSeries")
    n2$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#" )
    n2
    
  })
  
}

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("LFS Highlights Helper"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("RefMonth", "Reference Month", 
                  unique(as.character(MonthlyLFSx1$t)),selected = "2016/01"),
      hr(),
      textOutput("ReferenceMonth"), 
      textOutput("UR"),
      textOutput("URLast"),
      textOutput("EmpChangeMonth"),
      textOutput("LFChangeMonth"),
      textOutput("EmpChangeYear"),
      textOutput("LFChangeYear"),
      textOutput("URLastYear"),
      hr(),
      textOutput("FTMonth"),
      textOutput("PTMonth"),
      textOutput("FTMonth1524"),
      textOutput("FTMonth2554"),
      textOutput("FTMonth55Over"),
      hr(),
      textOutput("Public"),
      textOutput("Private"),
      textOutput("Self"),
      hr(),
      textOutput("CanUR"),
      textOutput("CanURLast"),
      textOutput("CanMoMEmp"),
      textOutput("CanFT"),
      textOutput("CanPT"),
      textOutput("CanLF")
    ),
    
    # The main panel
    mainPanel(
      tabsetPanel( # Inserting tabs
        
        # First tab
        
        tabPanel("Bar Graph",
                 h3("Unemployment Rate",align="center"),
                 showOutput("URPlot", lib="nvd3") # The bar graph for all provinces
        ), 
        
        # Second tab
        
        tabPanel("Line Graph",
                 h3("Unemployment Rate",align="center"),
                 showOutput("TimeSeries", lib="nvd3") # The line graph for BC and Canada
        ),
        
        # Third tab
        
        tabPanel("Data Table", DT::dataTableOutput("LFSTable"))  # The main datatable
        
        
      )
      
    )
    
  )
)


shinyApp(ui = ui, server = server)