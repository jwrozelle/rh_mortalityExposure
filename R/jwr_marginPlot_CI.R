

jwr_marginPlot_CI <- function(data, 
                              term = "coefLabel",
                              estimate = "estimate", 
                              confLL = "conf.low",
                              confUL = "conf.high",
                              fontFamily = "Times New Roman", 
                              axisLimits = c(-.1, .2),
                              axisInterval.minor = 0.01,
                              axisInterval.major = 0.05,
                              roundBy = 1
                              ) {
  
  require(ggplot2)
  require(extrafont)
  require(rlang)
  require(forcats)
  loadfonts(device = "win") |> suppressMessages()
  
  
  fSize <- function(size) {
    return(size*0.36)
  }
  
  
  # label for confidence intervals
  #   get names as strings
  
  data$ameLabel <- NA
  data$ameLabel <- paste0(
    round(data[[estimate]]*100, roundBy), " (",
    round(data[[confLL]]*100, roundBy), ", ",
    round(data[[confUL]]*100, roundBy), ")"
    )
  
  
  # set major graphical boundaries
  h_limits <- axisLimits
  
  # create scaling data.frame
  lightLines <- seq(h_limits[1], h_limits[2], axisInterval.minor)
  lightLines.df <- data.frame(light = lightLines)
  darkLines.df <- seq(h_limits[1], h_limits[2], axisInterval.major)
  # set aside some labels
  axisLabels.df <- data.frame(x = darkLines.df, xLabs = paste0(darkLines.df*100, "%"))
  
  darkLines.df <- data.frame(dark = darkLines.df[darkLines.df != 0])
  
  grid_height <- c(1-0.2, nrow(data)+0.2) #!!!
  column_head_height <- grid_height[2] + 0.3
  
  
  # make marginalPlot
  ggplot(data = data) +
    # make it horizontal
    coord_flip() +
    scale_y_continuous() + # continuous values
    scale_x_discrete(
      #"type", expand = c(1.1, 0) # categorical labels
    ) + 
    
    #   # light lines
    geom_segment(data = lightLines.df, aes(x = grid_height[1], xend = grid_height[2], y = light, yend = light, linetype="solid"), color = "lightgrey") +
    scale_linetype(guide = "none") +
    # darklines
    geom_segment(data = darkLines.df, aes(x = grid_height[1], xend = grid_height[2], y = dark, yend = dark, linetype = "solid"), color = "darkgrey", size = 1) +
    # axis Labels
    geom_text(aes(y = x, x=grid_height[1], label=xLabs), data=axisLabels.df, vjust = 1, hjust = 0.5, family = fontFamily, size = fSize(9)) +
    
    # add point estimates
    geom_point(aes(x = fct_rev(factor(.data[[term]])), y = .data[[estimate]]), size = 2.2) +
    # add errobars
    geom_errorbar(aes(x = fct_rev(factor(.data[[term]])), ymin = .data[[confLL]], ymax = .data[[confUL]]), width =0.3, size = 1.1) +
    
    # add vertical zero line
    geom_segment(aes(x = grid_height[1], xend = grid_height[2],y = 0, yend = 0), linetype = "dashed", size = 1) +
    
    # add information columns
    # pvalues
    geom_text(aes(x = column_head_height, y = h_limits[1]-0.075), label="P Values", hjust="left", vjust= 0, family = fontFamily, fontface = "bold") +
    geom_text(data = data, aes(x = fct_rev(factor(.data[[term]])), y = h_limits[1]-0.075, label = pvalue.lab), hjust = "left", family = fontFamily) +
    # term labels
    geom_text(aes(x = column_head_height, y = h_limits[1]-0.44), label="Term", hjust = "left", vjust= 0, family = fontFamily, fontface="bold") +
    geom_text(data = data, aes(x = fct_rev(factor(.data[[term]])), y = h_limits[1]-0.44, label = .data[[term]]), hjust = "left", family = fontFamily) +
    # estimate and confidence intervals
    geom_text(aes(x = column_head_height, y = h_limits[1]-0.225), label="AME Est\n% (95% CI)", hjust = "left", vjust= 0, family = fontFamily, fontface="bold") +
    geom_text(data = data, aes(x = fct_rev(factor(.data[[term]])), y = h_limits[1]-0.225, label = ameLabel), hjust = "left", family = fontFamily) +
    
    
    # add x label
    xlab(NULL) +
    # ylab("Percent point difference") +
    
    # set blank text for canvas size
    ## Left extent
    geom_text(aes(y=-h_limits[1]-.2, x=0, label="")) +
    ## Right extent
    geom_text(aes(y=h_limits[2], x = 0, label="")) +
    ## Top extent
    geom_text(aes(x=column_head_height+1.3, y=0, label="")) +
    ## Bottom extent
    geom_text(aes(x=-0.1, y=0, label="")) +
    
    # set limits and set scale to percent
    # scale_y_continuous(
    #   limits = h_limits, 
    #   labels = percent,
    #   breaks = c(darkLines,0),
    #   minor_breaks = c(0.5, 0.2) # set canvas limits
    #   ) +
    
    
    
  # THEME
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      legend.position = "none",
      legend.text = element_blank(),
      legend.title = element_blank(),
      legend.key = element_blank(),
      title = element_blank(),
      text = element_text(family = fontFamily)
    )
}