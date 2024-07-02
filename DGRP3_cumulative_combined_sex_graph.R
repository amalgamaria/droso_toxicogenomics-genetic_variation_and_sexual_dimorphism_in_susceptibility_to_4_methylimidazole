#This R code creates a graph using the DGRP3 data. This code generates a graph
#that displays the data for each sex on the same graph.
#This code uses R version 4.3.3 (2024-02-29 ucrt) and RStudio version 2023.12.01 "Ocean Storm" Release.
#Clear the working environment.

rm(list=ls())

#Import necessary libraries - if an unexplained issue is occurring, you may need to update
#your RStudio application or R version.

library(ggplot2)
library(gridExtra)

#Read in data. Please update the file path as needed.

data = read.csv("/Users/Katelynne/Desktop/updated_codes/DGRP3/4methyl_204_newse.csv")

#The following code will separate the male and female data into two different data sets.
#Use print to check that the data has separated correctly.

ls(data)
data$Sex
data %>% select(Sex, Line, Mean)
femaledata <- data %>%
  filter(Sex == "F") %>%
  select(Sex, Line, Mean, StError)
print(femaledata)
maledata <- data %>%
  filter(Sex == "M") %>%
  select(Sex, Line, Mean, StError)
print(maledata)


wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Create plot using the ggplot2 library.

library(ggplot2)
p1 <- ggplot() + 
  geom_point(data = femaledata, aes(x = reorder(Line, Mean), y = Mean, color = Sex), size=0.80) +
  geom_errorbar(data = femaledata, aes(x = reorder(Line, Mean), ymin=Mean-StError, ymax=Mean+StError, color = Sex), width=0.80, size = 0.2,
                position=position_dodge(0.34)) +
  geom_point(data = maledata, aes(x = reorder(Line, Mean), y = Mean, color = Sex), size=0.80) +
  geom_errorbar(data = maledata, aes(x = reorder(Line, Mean), ymin=Mean-StError, ymax=Mean+StError, color = Sex), width=0.80, size = 0.2,
                position=position_dodge(0.34)) +
  scale_color_manual(values = c("F" = "orange", "M" = "purple")) +
  scale_y_continuous(limits = c(-0.015,1.03), expand = c(0,0)) +
  labs(title=(expression(paste("Variation in survival after 4-methylimidazole exposure in the DGRP")))) +
  xlab(wrapper("DGRP3 line (organized by increasing female mean)", width = 49)) + ylab("Mean survival proportion") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(linewidth = 0.34, linetype = "solid", colour = "black"),
        axis.line.y = element_line(linewidth = 0.34, linetype = "solid", colour = "black"),
        axis.title.x = element_text(size = 9.67), axis.title.y = element_text(size = 9.67), axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.text.y= element_text(size = 9.67),
        plot.title = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position="none")

#Print to check the plot's appearance.

print(p1)

#To save the plot in a specific file format and adjust the aspect ratio, you can tinker with the following line of code.
#Be aware that aspect ratio adjustments may interfere with the appearance of fonts and line thickness.

ggsave("DGRP3_combined_sex_graph.tiff", plot = p1, width = 6.5, height = 4.0, units = "in", dpi = 600)


