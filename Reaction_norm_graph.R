#This R code creates a reaction norm graph using the DGRP3 acute screen data.
#This code uses R version 4.3.3 (2024-02-29 ucrt) and RStudio version 2023.12.01 "Ocean Storm" Release.
#Clear the working environment.

rm(list=ls())

#Import necessary libraries - if an unexplained issue is occurring, you may need to update
#your RStudio application or R version.

library(ggplot2)

#Read in data. Please update the file path as needed.

data = read.csv("/Users/Katelynne/Desktop/updated_codes/DGRP3/4methyl_204_newse.csv")

#Check that the data has imported correctly.

head(data)
tail(data)

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


#The following line of code is a conditional statement.
#It creates a new column to visualize the graph
#more efficiently.

data$SexPos <- ifelse(data$Sex == "F", 1, 2)

#Create plot using the ggplot2 library.

p1 <- ggplot(data, aes(x = SexPos, y = Mean, group = Line, color = Line)) +
  geom_line(linewidth=0.25) +
  geom_point(size=1.2) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Female", "Male")) +
  scale_y_continuous(limits = c(-0.015,1.03), expand = c(0,0)) +
  labs(title = "Reaction Norm Diagram", x = "Sex", y = "Mean survival proportion") +
  theme(legend.position = "none", panel.border = element_blank(),
              axis.line.x = element_line(linewidth = 0.34, linetype = "solid", colour = "black"),
              axis.line.y = element_line(linewidth = 0.34, linetype = "solid", colour = "black"),
              axis.text.x=element_text(size=9.37), axis.text.y=element_text(size=9.37),
              panel.background = element_rect(fill = "white"),
              axis.title.x = element_text(size=9.37),
              axis.title.y = element_text(size=9.37),
              plot.title = element_blank())
        
#Print graph to check its appearance.

print(p1)

#To save the plot in a specific file format and adjust the aspect ratio, you can tinker with the following line of code.
#Be aware that aspect ratio adjustments may interfere with the appearance of fonts and line thickness.

ggsave("Reaction_norm_graph.tiff", plot = p1, width = 6.5, height = 8.0, units = "in", dpi = 600)
