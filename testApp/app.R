library(shiny)
library(tidyverse)
library(agridat)  # The package where the data comes from
library(DT)

# load the data ----
# barley <- as.data.frame(beaven.barley)
# d <- data.frame(val = rnorm(100))

# ui.R ----
ui <- fluidPage(
  titlePanel("Distributions"),  
  sidebarLayout(  
    sidebarPanel(
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      
      br(),
      
      sliderInput("n",
                  "Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000)
    ),  # close sidebar panel
    
    mainPanel(
      tabsetPanel(
      type = "tabs",
      tabPanel("Plot", plotOutput("myPlot")),
      tabPanel("Summary", verbatimTextOutput("mySummary")),
      tabPanel("Table", DTOutput("myTable"))
      ) 
    )
    
  )
)

# server ----
server <- function(input, output) {
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    dat <- data.frame(val = dist(input$n))
    return(dat)
    
  })
  
  output$myPlot <- renderPlot(
    d() %>%
      ggplot(aes(x = val)) + # Create object called `output$plot` with a ggplot inside it
      geom_histogram(bins = input$bin,  
                     fill = "green",
                     colour = "black") +
      theme_classic()
  )   
  output$mySummary <- renderPrint(
    summary(d())
  )
  
  # output$myTable <- renderTable(d(), filter = "top")
  output$myTable <- DT::renderDT(mtcars, filter = "top", 
                                 options = list(pageLength = 5))

}


shinyApp(ui = ui, server = server)

