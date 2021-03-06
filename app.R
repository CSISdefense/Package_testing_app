# Demonstration application for shiny tools from csis360 

library(shiny)
library(tidyverse)
library(magrittr)
library(lazyeval)

# should work with csis360 once it is updated and compiled locally
#library(csis360)

# until then just read the functions from local script directly - remove and
# replace this with library(csis360) later.
source("shiny_functions.R")

ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "breakout",
        "breakout:",
        choices = c("None", "Shiny.VendorSize", "SubCustomer"),
        selected = "None"),
      selectInput(
        "facet",
        "facet:",
        choices = c("None", "Shiny.VendorSize", "SubCustomer"),
        selected = "None"),
      selectInput(
        "filter",
        "filter by Competition.sum:",
        choices = c("Unlabeled","3+ Offers","2 Offers","No Comp.","1 Offer"),
        selected = c("Unlabeled","3+ Offers","2 Offers","No Comp.","1 Offer"),
        selectize = FALSE,
        multiple = TRUE),
      radioButtons(
        "chart",
        "chart:",
        choices = c("bar chart", "line chart"),
        selected = "bar chart",
        inline = TRUE)
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

  load("2016_unaggregated_FPDS.Rda")

  output$fpds_plot <- renderPlot({
    
    if(input$breakout == "None"){
      plot <- ggplot(
        data = dataset(),
        aes(x = Fiscal.Year, y = Action.Obligation.2016))
    } else {
      plot <- ggplot(
        data = dataset(),
        aes_(
          x = as.name("Fiscal.Year"),
          y = as.name("Action.Obligation.2016"),
          fill = as.name(input$breakout),
          color = as.name(input$breakout)))}
    
    if(input$chart == "bar chart") plot <- plot + geom_bar(stat = "identity")
    if(input$chart == "line chart") plot <- plot + geom_line()
    if(input$facet != "None") plot <- plot + facet_wrap(input$facet)

    # money_label() turns a number into a currency-formatted string.  It can
    # take single numbers or vectors, and works either as a ggplot labeling
    # function or on it's own.  See ?money_label for formatting options.
    plot <- plot + scale_y_continuous(labels = money_label)
    
    # add_diigtheme() adds diigtheme!
    plot %<>% add_diigtheme()

    return(plot)
  })

  dataset <- reactive({
    
  # filter_by() filters a dataframe by specified levels of a specified variable.
  # Both variable and level names are passed as strings, so you can
  # read them directly from Shiny inputs.
    
  # sum_to() aggregates a data frame to the level of the variables specified
  # in the group_by argument, by summming observations.  All arguments are
  # passed as strings, so you can read them directly from Shiny inputs.
    
    choices <- c(input$breakout, input$facet, "Fiscal.Year")
    choices <- choices[choices != "None"]
    return(
      def_data %>% 
        filter_by(
          var_name = "Competition.sum",
          level_names = input$filter) %>%
        sum_to(
          group_by = choices,
          y_var = "Action.Obligation.2016"))
  })



  output$hover_info <- renderUI({

  # hover_data() returns a single-row dataframe containing the observation
  # the user is hovering - it works for line, bar, scatter, stacked bar, and
  # area charts, including charts with facets.
    
  # hover_tip() creates a customizable tooltip from a HTML string.  If you pass
  # it a single-row dataframe like those generated by hover_data(), it will
  # create the HTML string for you.  See ?hover_tip for customization options.
    hover_tip(
      input$plot_hover,
      hover_data(
        chart_data = dataset(),
        hover_object = input$plot_hover,
        chart_type = ifelse(input$chart == "bar chart", "bar", "line")))

  })

}

# Run the application
shinyApp(ui = ui, server = server)

