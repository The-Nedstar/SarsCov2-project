###########################################
# Functions for Sars Covid 19 assignment
# author: anonymous
###########################################

###Logistic Growth
logistic_growth <- function(t, s, f0) {
  (f0 * exp(s * t)) / (1 + f0 * (exp(s * t) - 1)) # defining the logistic growth equation
}

### stacked area plot
stacked_area_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Fill, Title, File) {
  ## creating the plot
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis, fill = Fill)) +
    geom_area() + # setting to area plot
    scale_fill_jco() + # setting the palette
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title)
  ## saving as a png
  png(filename = here("Figures", File), height = 10, width = 20, units = "cm", res = 400)
  print(temp)
  dev.off()
}

#### lineage plots
### single
single_lineage_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title) {
  ## creating plot
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis)) +
    geom_point() + # points
    geom_line() + # line between points on top
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title)
  ## return the plot for further modification rather than save as png
  return(temp)
}

### double
multi_lineage_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Colour, ColTitle, Title, LegPos) {
  ## creating plot
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis,
                   colour = Colour)) +
    geom_point() + # points
    geom_line() + # line between points on top
    scale_colour_jco() + # setting palette
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title, Colour = ColTitle) +
    theme(legend.position = LegPos, # defining the legend position
          legend.background = element_rect(fill = "white", color = "black"))
  ## return the plot for further modification rather than save as png
  return(temp)
}

### combining plots into a grid
grid_plot <- function(Filename, Plot1, Plot2, Plot3, Plot4){
  ## saving as png
  png(filename = here("Figures", Filename), height = 20, width = 25, units = "cm", res = 600)
  # defining positions
  grid.arrange(Plot1, Plot2, Plot3, Plot4, ncol = 2)
  dev.off()
}