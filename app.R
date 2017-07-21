#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)
library(lazyeval)
source("C:\\Users\\LLipsey\\Documents\\GitHub\\hamre\\R\\plot_functions.R")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "breakout",
        "breakout:",
        choices = c("None", "Vendor.Size", "Simple"),
        selected = "None"),
      selectInput(
        "facet",
        "facet:",
        choices = c("None", "Vendor.Size", "Simple"),
        selected = "None")
    ),
    
    mainPanel(
      plotOutput(
        "fpds_plot", 
        hover = hoverOpts(id = "plot_hover", delay = 80)),
      uiOutput("hover_info")
    )
  )
)


server <- function(input, output) {
  
  load("FPDS.Rda")  
  
  output$fpds_plot <- renderPlot({
    
    if(input$breakout == "None"){
      plot <- ggplot(
        data = dataset(),
        aes(x = Fiscal.Year, y = Action.Obligation)) +
        geom_bar(stat = "identity")
    } else {
      plot <- ggplot(
        data = dataset(),
        aes_string(
          x = "Fiscal.Year",
          y = "Action.Obligation",
          fill = input$breakout)) +
        geom_bar(stat = "identity")
    }
    
    if(input$facet != "None") plot <- plot + facet_wrap(input$facet)
    
    plot <- plot + scale_y_continuous(labels = money_label)
    
    return(plot)
  })
  
  dataset <- reactive({
    choices <- c(input$breakout, input$facet, "Fiscal.Year")
    choices <- choices[choices != "None"]
    return(FPDS %>% sum_to(choices))
  })
  
  
  
  output$hover_info <- renderUI({
    
    hover_tip(
      input$plot_hover,
      hover_data(
        chart_data = dataset(),
        hover_object = input$plot_hover,
        chart_type = "bar"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

