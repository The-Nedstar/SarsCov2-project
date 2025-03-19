###########################################
# Functions for Sars Covid 19 assignment
# author: anonymous
###########################################

###Logistic Growth
logistic_growth <- function(t, s, f0) {
  (f0 * exp(s * t)) / (1 + f0 * (exp(s * t) - 1))
}

### stacked area plot
stacked_area_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Fill, Title, File) {
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis, fill = Fill)) +
    geom_area() +
    scale_fill_brewer(palette = "Set3") +
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title)
  
  png(filename = here("Figures", File), height = 10, width = 20, units = "cm", res = 400)
  print(temp)
  dev.off()
}

### lineage plots
## single
single_lineage_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Title) {
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title)
  return(temp)
}

##double
multi_lineage_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Colour, ColTitle, Title) {
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis,
                   colour = Colour)) +
    geom_line() +
    geom_point() +
    scale_colour_brewer(palette = "Dark2") +
    theme_bw() +
    labs(x = Xtitle, y = Ytitle, title = Title, Colour = ColTitle) +
    theme(legend.position = c(0.85, 0.75),
          legend.background = element_rect(fill = "white", color = "black"))
  return(temp)
}

## combining plots into a grid
grid_plot <- function(Filename, Plot1, Plot2, Plot3, Plot4){
  png(filename = here("Figures", Filename), height = 20, width = 25, units = "cm", res = 400)
  # defining positions
  grid.arrange(Plot1, Plot2, Plot3, Plot4, ncol = 2)
  dev.off()
}