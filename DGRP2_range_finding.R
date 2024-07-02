#This R code creates the range finding plots using the DGRP2 data.
#This code uses R version 4.3.3 (2024-02-29 ucrt) and RStudio version 2023.12.01 "Ocean Storm" Release.
#Clear the working environment.

rm(list=ls())

#Import necessary libraries - if an unexplained issue is occurring, you may need to update
#your RStudio application or R version.

library(ggplot2)
library(cowplot)
library(gtable)


#Read in data. Please update the file path as needed.

data = read.csv("/path/to/working/file.csv")
data2 = read.csv("/path/to/working/file.csv")
data3 = read.csv("/path/to/working/file.csv")
data4 = read.csv("/path/to/working/file.csv")

#Set the factor levels for the Line column.
#This is based on the 14 DGRP2 lines tested.

lines_order <- c("DGRP-41", "DGRP-59", "DGRP-83", "DGRP-129", "DGRP-195", "DGRP-217", "DGRP-228", "DGRP-367", "DGRP-371", "DGRP-491", "DGRP-508", "DGRP-799", "DGRP-808", "DGRP-900")

data$Line <- factor(data$Line, levels = lines_order)
data2$Line <- factor(data2$Line, levels = lines_order)
data3$Line <- factor(data3$Line, levels = lines_order)
data4$Line <- factor(data4$Line, levels = lines_order)

#Create individual graphs. These will later be combined onto a single plot.
#A graph will be made for each concentration and sex combination.

create_plot <- function(data, title) {
  ggplot(data, aes(x = Exposure, y = Mean, color = Line)) +
    geom_point(size = 1.0) +
    geom_line(linewidth = 0.25) +
    geom_errorbar(aes(x = Exposure, ymin = Mean - StError, ymax = Mean + StError), width = 0.8, linewidth = 0.2) +
    labs(x = "Exposure (hours)", y = "Mean survival proportion", title = title, color = "DGRP2 line") +
    scale_x_continuous("Exposure (hours)", breaks = c(0, 24)) +
    scale_y_continuous(limits = c(0, 1.0)) +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "black"),
          axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "black"),
          axis.title.x = element_text(size = 10.36),
          axis.title.y = element_text(size = 10.36),
          axis.text.y = element_text(size = 10.36),
          axis.text.x = element_text(size = 10.36),
          panel.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5, size = 13.88))
}

p1 <- create_plot(data, "Female 15 mM")
p2 <- create_plot(data2, "Female 30 mM")
p3 <- create_plot(data3, "Male 15 mM")
p4 <- create_plot(data4, "Male 30 mM")

#Combine plots and remove the legend from each graph. A single legend representing
#the whole plot will be added later in the code.

prow <- plot_grid(p1 + theme(legend.position = "none"),
                  p2 + theme(legend.position = "none"),
                  p3 + theme(legend.position = "none"),
                  p4 + theme(legend.position = "none"),
                  align = 'vh',
                  labels = c("A.", "B.", "C.", "D."),
                  label_size = 13.88,
                  hjust = -1,
                  nrow = 2)

#Extract the legend from one of the graph. The legend should be the same for each graph.

extract_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- gtable_filter(g, "guide-box")
  return(legend)
}

legend_p <- extract_legend(p1 + theme(legend.position = "right"))

#Combine the graphs and legend onto one plot.

p <- plot_grid(prow, legend_p, nrow = 1, rel_widths = c(0.07, 0.03))

#Print to check the plot's appearance.

print(p)

#To save the plot in a specific file format and adjust the aspect ratio, you can tinker with the following line of code.
#Be aware that aspect ratio adjustments may interfere with the appearance of fonts and line thickness.
#If you need to move the legend over more, the plot can be changed into a different file type and
#imported into Adobe Illustrator.

ggsave("DGRP2_range_finding.tiff", units = "in", width = 6.5, height = 5.0, dpi = 600)

