#' Converts numbers into character strings formatted as currency values,
#' with reasonable abbreviations and significant digits.  Is vectorized and
#' can be used with ggplot's axis scaling functions - see examples.
#'
#' @param money_values A numeric or integer value or vector of values, to
#' convert to currency abbreviations
#' @param cur A character or string to prepend to the labels.  Likely a
#' currency sign - "$" by default.  Use "" for no prepended string.
#'
#' @return a character vector of formatted labels
#' @usage scale_y_continuous(labels = money_label)
#' @examples
#' ggplot(
#'  data = FPDS %>%
#'    group_by(Fiscal.Year, Vendor.Size) %>%
#'    summarize(Action.Obligation = sum(Action.Obligation)),
#'  aes(
#'    x = Fiscal.Year,
#'    y = Action.Obligation,
#'    color = Vendor.Size)) +
#' geom_line() +
#' scale_y_continuous(labels = money_label)
#'
#' @export

money_label <- function(money_values, cur = "$"){

  if(class(money_values) == "character"){
    warning(
      paste("money_label() expects the axis to be a numeric variable",
            "but the axis is a character variable.  Coercing to numeric."))
    money_values <- as.numeric(money_values)
  } else if(class(money_values) != "numeric" & class(money_values)!= "integer"){
    stop(paste(
      "money_labels() expected a numeric axis, but got:",
      class(money_values)))
  }

  money_format <- function(a_value, max_value, sig, currency) {
    if(is.na(a_value)) return(NULL)

    logged <- ceiling(log10(abs(max_value)))

    if(logged > 15) return(paste0(cur, a_value))
    lab <- switch(
      logged,
      {paste0(cur, formatC(a_value, max(sig, 2), format = "f"))},
      {paste0(cur, formatC(a_value, max(sig, 1), format = "f"))},
      {paste0(cur, formatC(a_value, max(sig, 0), format = "f"))},
      {paste0(cur, formatC(a_value/1e3, max(sig, 3), format = "f"), "k")},
      {paste0(cur, formatC(a_value/1e3, max(sig, 1), format = "f"), "k")},
      {paste0(cur, formatC(a_value/1e3, max(sig, 0), format = "f"), "k")},
      {paste0(cur, formatC(a_value/1e6, max(sig, 2), format = "f"), "M")},
      {paste0(cur, formatC(a_value/1e6, max(sig, 1), format = "f"), "M")},
      {paste0(cur, formatC(a_value/1e6, max(sig, 0), format = "f"), "M")},
      {paste0(cur, formatC(a_value/1e9, max(sig, 2), format = "f"), "B")},
      {paste0(cur, formatC(a_value/1e9, max(sig, 1), format = "f"), "B")},
      {paste0(cur, formatC(a_value/1e9, max(sig, 0), format = "f"), "B")},
      {paste0(cur, formatC(a_value/1e12, max(sig, 2), format = "f"), "T")},
      {paste0(cur, formatC(a_value/1e12, max(sig, 1), format = "f"), "T")},
      {paste0(cur, formatC(a_value/1e12, max(sig, 0), format = "f"), "T")})

    return(lab)
  }
  
  if(length(money_values) > 1){
    axis_range <-
      max(money_values, na.rm = TRUE) - min(money_values, na.rm = TRUE)
    sig_digits <-  floor(log10(max(abs(money_values), na.rm = TRUE))) -
      round(log10(axis_range))
    
    return(sapply(
      money_values,
      money_format,
      max(abs(money_values), na.rm = TRUE),
      sig_digits,
      simplify = "vector"))
  } else if(is.na(money_values) || is.null(money_values)){
    return(NULL)} else {
      return(suppressWarnings(money_format(
        money_values,
        money_values,
        2 - (floor(log10(abs(money_values))) %% 3))))
    }
}





# csis_palettes <- function()
# {
#   BarPalette <- scale_fill_manual(
#     values = c(
#       "#004165",
#       "#0065a4",
#       "#0095AB",
#       "#66c6cb",
#       "#75c596",
#       "#0faa91",
#       "#51746d",
#       "#607a81",
#       "#252d3a",
#       "#353535",
#       "#797979"))
# 
#   LinePalette <- scale_color_manual(
#     values = c(
#       "#004165",
#       "#75c596",
#       "#b24f94",
#       "#0095ab",
#       "#0a8672",
#       "#e22129",
#       "#66c6cb",
#       "#51746d",
#       "#797979",
#       "#788ca8",
#       "#353535"))
# }




#' A convenience function for summation using quoted variable names
#'
#' @param data_frame The data frame
#' @param group_by The variables you want to keep as breakouts in the
#' aggregated data, as a character vector of variable names.  This should
#' include any variable you plan to use on an X-axis (e.g. Fiscal.Year).
#' @param y_vars The variables you want to aggregate, as a character vector
#' of variable names.  By default, all numeric or integer variables that are
#' not listed in the group_by argument.  Variables that are not listed in
#' either group_by or y_vars will be rolled up in the aggregation and absent
#' from the returned data frame.
#'
#' @return An aggregated data frame
#'
#' @examples
#' \dontrun{
#' shown %<>% sum_to(
#'   group_by = c(input$breakout_variable, input$facet_variable, "Fiscal.Year"),
#'   y_vars = input$y_variable)
#' }
#' @export

sum_to <- function(
  data_frame,
  group_by,
  y_vars = "use_all_numeric_vars"
){
  if(y_vars == "use_all_numeric_vars"){
    y_vars <- which(
      sapply(data_frame, class, simplify ="vector") %in%
        c("integer", "numeric"))
    y_vars <- names(data_frame)[y_vars]
    y_vars <- y_vars[!y_vars %in% group_by]
  }

  updated_frame <- list()
  return_frame <- tibble()
  
  for(i in 1:length(y_vars)){
    updated_frame <-
      data_frame %>%
        ungroup() %>%
        group_by_(.dots = group_by) %>%
        summarize_(
          auto_sum_placeholder = interp(
            ~sum(var, na.rm = TRUE), var = as.name(y_vars[i])))
    
    names(updated_frame)[which(names(updated_frame) == "auto_sum_placeholder")] <-
      y_vars[i]
    
    if(i > 1){return_frame <- suppressMessages(
      left_join(return_frame, updated_frame))
    } else return_frame <- updated_frame
  }

  return(updated_frame)

}


#' A convenience function for filtering using quoted variable names
#'
#' @param data_frame The data frame
#' @param var_name The quoted name of the variable to use for filtering.
#' @param level_names The values (typically factor levels) of the variable
#' that you want to keep in the data, as vector of quoted value names.
#' Or, if you set exclude = TRUE, the variables you want to throw away.
#' @param exclude If TRUE, throw out rows with values listed in level_names.
#' If FALSE, keep rows with values listed in level_names.
#'
#' @return A filtered data frame
#'
#' @examples
#' \dontrun{
#' shown %<>% filter_by(
#'   var_name = "Vendor.Size",
#'   level_names = input$vendor_size_filter)
#' }
#' @export



filter_by <- function(
  data_frame,
  var_name,
  level_names,
  exclude = FALSE
){
  if(length(level_names) > 1){
    level_names <- paste0("'", level_names, "',")
    string <- paste0(level_names, collapse = " ")
    string <- paste0(var_name, " %in% c(", string)
    string <- sub(",$",")", string)
  } else string <- paste0(var_name, " == '", level_names, "'")

  if(exclude) string <- paste0("!", string)

  return(data_frame %>% filter_(string))
}

#' Sets the working directory to the folder holding the script from which it
#' is called.  This is kludgey and will only work if:
#'
#' 1. You are using RStudio, and
#' 2. You run it directly from a script instead of the console or a source file.
#'
#' It is intended to go at the start of data processing files
#'
#' @examples
#' \dontrun{
#' # Data processing for FPDS 3.0
#' library(tidyverse)
#' library(csis360)
#' set_wd_here()
#' }

set_wd_here <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


add_diigtheme <- function(
  plot
  ){
  plot <- plot +
    theme(
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color="white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="lightgray"),
      panel.grid.minor.y = element_line(size=.1, color="lightgray"),
      axis.ticks = element_blank()
    ) +
    theme(plot.title = element_text(
      family = "Open Sans",
      color = "#554449",
      face="bold",
      margin=margin(20,0,20,0),
      hjust = 0.5)) +
    theme(axis.text.x = element_text(
      family = "Open Sans",
      vjust = 7,
      margin = margin(0,0,0,0))) +
    theme(axis.text.y = element_text(
      family = "Open Sans",
      color ="#554449",
      margin = margin(0,5,0,0))) +
    theme(axis.title.x = element_text(
      face = "bold",
      color = "#554449",
      family = "Open Sans",
      margin = margin(15,0,0,60))) +
    theme(axis.title.y = element_text(
      face = "bold",
      color = "#554449",
      family = "Open Sans",
      margin = margin(0,15,0,0))
    ) +
    theme(legend.text = element_text(
      family = "Open Sans",
      color = "#554449")) +
    theme(legend.title = element_blank()) +
    theme(legend.position = 'bottom') +
    theme(legend.background = element_rect(fill = "white")) +
    theme(strip.background = element_rect(
      fill = "white",
      color = "#554449",
      size = 0.4))
return(plot)
  }


#' Retrieve the data for the current hover location
#'
#' @param chart_data A data frame.  The same data used to create the chart.
#' @param hover_object The Shiny input object containing the hover info.
#' @param chart_type Either "line" or "bar".  For scatterplot, use
#' "line", for area or stacked bar use "bar". Other types are not currently
#' supported and will require manual tips.
#' @param max_distance For line and scatter charts: a distance, in pixels.  If
#' the mouse is more than this distance from any point, the function will
#' return a null data frame.
#'
#' @return A data frame of
#'
#' @examples
#' \dontrun{
#' output$hover_info <- renderUI({
#'
#'   shown <- hover_data(
#'     chart_data = dataset(),
#'     hover_object = input$plot_hover,
#'     chart_type = "stacked bar")
#' })
#' }
#' @export

hover_data <- function(
  chart_data,
  hover_object,
  chart_type,
  max_distance = 15
){
  
  if(is.null(hover_object$x)) return(NULL)
  if(is.null(hover_object$y)) return(NULL)

  chart_data %<>% ungroup()

  if(chart_type == "line"){
    row <- nearPoints(
      chart_data,
      hover_object,
      xvar = hover_object$mapping$x,
      yvar = hover_object$mapping$y,
      threshold = max_distance,
      maxpoints = 1)
    if(nrow(row) == 0) return(NULL)

  } else if(chart_type == "bar"){
    
    # filter to rows in the correct facet(s)
    if(with(hover_object$mapping, exists("panelvar1"))){
      if(!with(hover_object, exists("panelvar1"))) return(NULL)
      
      if(with(hover_object$mapping, exists("panelvar2"))){
        if(!with(hover_object, exists("panelvar2"))) return(NULL)
        chart_data %<>% filter_(
          paste0(
            hover_object$mapping$panelvar2,
            " == '",
            hover_object$panelvar2,
            "'"))
      }
      
      chart_data %<>% filter_(
        paste0(
          hover_object$mapping$panelvar1,
          " == '",
          hover_object$panelvar1,
          "'"))
    }
    
    # filter to rows with the correct value of the X axis variable
    x_class <- class(chart_data[[hover_object$mapping$x]])
    if(x_class == "factor"){
      row <- chart_data %>%
        filter(
          chart_data[[hover_object$mapping$x]] ==
            levels(chart_data[[hover_object$mapping$x]])[round(hover_object$x)])
    } else if(x_class == "character"){
      row <- chart_data %>%
        filter(
          chart_data[[hover_object$mapping$x]] ==
            levels(factor(chart_data[[hover_object$mapping$x]]))[
              round(hover_object$x)])
    } else if(x_class == "numeric" | x_class == "integer"){
      row <- chart_data %>%
        filter_(
          paste(
            hover_object$mapping$x,
            "==",
            round(hover_object$x)))
    } else stop(paste0("Error in hover_data: X variable must be factor,",
                       "character, numeric, or integer"))
    if(nrow(row) == 0) return(NULL)
    
    
    # filter to rows with the correct level of fill breakout
    # (for area or stacked bar)
    if("fill" %in% names(hover_object$mapping)){
      
      breakout <- chart_data[[hover_object$mapping$fill]]
      if(!(class(breakout) == "factor")) breakout %<>% factor()
      
      # find which breakout section the cursor is in
      y_breaks <- row %>%
        group_by_(hover_object$mapping$fill) %>%
        summarize_(
          interp(
            ~sum(var, na.rm = TRUE),
            var = as.name(hover_object$mapping$y))) %>%
        .[[2]] %>%
        rev() %>%
        cumsum()
      
      #y_breaks <- cumsum(rev(pull(y_breaks, 2)))
      
      cursor_level <- base::Position(
        function(x) x > hover_object$y,
        y_breaks)
      
      if(is.na(cursor_level)) return(NULL)
      row %<>% filter_(
        paste0(
          hover_object$mapping$fill,
          " == '",
          rev(levels(breakout))[cursor_level],
          "'"))
    } else if(hover_object$y > row[[hover_object$mapping$y]]) return(NULL)
      
  } else stop("chart_type must be 'line' or 'bar'")
  
  return(row)

}


#' Create a tooltip for a ggplot in a Shiny application
#'
#' @param hover_object The Shiny input object containing the hover info.
#' @param content *Either* a single-row data frame from which to automatically
#' generate a tooltip, *or* a string with the full contents of a custom
#' tooltip, formatted for HTML.
#' @param background_color A hex value, as string, to form the
#' background color of the tooltip
#' @param alpha A number between 0 and 1, to set the opacity of the
#' tooltip background
#' @param preferred_side One of c("right", "left")
#' @param h_just The horizontal distance, in pixels, to move the tooltip
#' to the right.  Negative values will move the tooltip to the left.
#' @param v_just The vertical distance, in pixels, to move the tooltip
#' up.  Negative values will move the tooltip down.
#' @param minimum_h The minimum horizonal width, in pixels, of the tooltip
#' panel.  The panel will squish to be smaller as the cursor approaches
#' the edge of the plotting area, but will not squish smaller than minimum_h.
#' @param minimum_v The minimum vertical width, in pixels, of the tooltip
#' panel.  The panel will "squish" to be smaller as the cursor approaches
#' the bottom of the plotting area, but will not squish smaller than minimum_v.
#'
#' @return A wellPanel-based on-hover tooltip
#'
#' @usage hover_tip(input$plot_hover, "<b> Hello World </b>")
#'
#' @examples
#'  \dontrun{
#' output$hover_info <- renderUI({
#'
#'   hover_tip(
#'     hover_object = input$plot_hover,
#'     content = paste0(
#'       "<b> Fiscal Year: </b>", shown$Fiscal.Year, "<br/>",
#'       "<b> Vendor Size: </b>", shown$Vendor.Size, "<br/>",
#'       "<b> Amount: </b>", money_label(shown$Action.Obligation)))
#' })
#'
#' output$hover_info <- renderUI({
#'   hover_tip(
#'     hover_object = input$plot_hover,
#'     content = hover_data(
#'       chart_data = dataset(),
#'       hover_object = input$plot_hover,
#'       chart_type = "bar"))
#' })
#'
#' }

hover_tip <- function(
  hover_object,
  content,
  background_color = "#f5f5f5",
  alpha = 0.85,
  preferred_side = "right",
  h_just = 0,
  v_just = 0,
  minimum_h = 100,
  minimum_v = 100
  ){

  hover <- hover_object
  if(is.null(content)) return(NULL)
  if(is.null(hover_object$x)) return(NULL)
  if(is.null(hover_object$y)) return(NULL)

  rgb <- col2rgb(background_color)[,1]
  if(alpha > 1 | alpha < 0) stop("alpha must be between 0 and 1")

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left +
    (((hover$x - hover$domain$left) /
    (hover$domain$right - hover$domain$left)) *
    (hover$range$right - hover$range$left)) + h_just
  top_px <- hover$range$top +
    (((hover$domain$top - hover$y) /
    (hover$domain$top - hover$domain$bottom)) *
    (hover$range$bottom - hover$range$top)) - v_just

  if(preferred_side == "right"){
    if((hover$range$right - left_px) < minimum_h){
      left_px <- (hover$range$right - minimum_h)}
    if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
      top_px <- (hover$range$bottom - minimum_v)}
    style <- paste0(
      "position:absolute; z-index:100; background-color: ",
      "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
      "left:", left_px, "px; top:", top_px, "px;")

  } else if (tolower(preferred_side) == "left") {
    if((left_px - hover$range$left) < minimum_h){
      left_px <- hover$range$left + minimum_h}
    if(((hover$range$top - top_px) + hover$range$bottom) < minimum_v){
      top_px <- (hover$range$top - (hover$range$bottom + mimimum_v))}
    style <- paste0(
      "position:absolute; z-index:100; background-color: ",
      "rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, "); ",
      "right:", hover$range$right - left_px, "px; top:", top_px, "px;")

  } else stop("preferred_side must be 'right' or 'left'")

  if(any(class(content) == "data.frame")){
    if(nrow(content) > 1){
      stop("content must be a HTML string or a single-row data frame")}
    
    content %<>% sapply(
      function(x){
        if(class(x) == "numeric" && 
           any(!is.na(x)) && (max(x) > 2050 | min(x) < 1850)){
          return(money_label(x, cur = ""))
        } else if(class(x) == "factor"){
          return(as.character(x))
        } else return(x)
      },
      simplify = "vector")
    
    strings <- character()
    for(i in 1:length(content)){
      strings[i] <- paste0("<b>", names(content)[i], ": </b>", content[i])}
    content <-paste0(strings, collapse = "<br/>")
  }

  return(wellPanel(style = style, p(HTML(content))))
}


