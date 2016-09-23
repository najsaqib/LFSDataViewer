library(DT)
library(shiny)
library(dplyr)
library(rCharts, lib.loc = 'H:/R/win-library/3.3')

# Use a fluid Bootstrap layout
fluidPage(    
  
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
        
        tabPanel("Data Table", DT::dataTableOutput("LFSTable"))  # The main datatable
    
     
                  )
     
            )
    
  )
)
