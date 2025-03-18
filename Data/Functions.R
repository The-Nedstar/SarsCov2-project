###########################################
# Functions for Sars Covid 19 assignment
# author: anonymous
###########################################

### stacked area plot
stacked_area_plot <- function(Data, Xaxis, Xtitle, Yaxis, Ytitle, Fill, Title, File) {
  temp <- ggplot(Data, aes(x = Xaxis, y = Yaxis, fill = Fill)) +
    geom_area() +
    scale_fill_brewer(palette = "Set3") +
    theme_bw() +
    ggtitle(Title) +
    ylab(Ytitle) +
    xlab(Xtitle)
  png(filename = here("Figures", File), height = 10, width = 20, units = "cm", res = 400)
  print(temp)
  dev.off()
}