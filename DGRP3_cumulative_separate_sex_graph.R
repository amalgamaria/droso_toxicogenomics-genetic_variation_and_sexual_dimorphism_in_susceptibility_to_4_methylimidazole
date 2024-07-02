#This R code creates a graph using the DGRP3 data. This code generates a graph
#that displays the data for each sex on different graphs.
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
#p1 is the graph showcasing the female data.

library(ggplot2)
p1 <- ggplot() +
  geom_point(data = femaledata, aes(x = reorder(Line, Mean), y = Mean, color = Sex), size=0.4) +
  geom_errorbar(data = femaledata, aes(x = reorder(Line, Mean), ymin=as.numeric(Mean)-as.numeric(StError), ymax=as.numeric(Mean)+as.numeric(StError), color = Sex), width=0.8, size = 0.2,
                position=position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.015,1.03), expand = c(0,0)) +
  scale_color_manual(values=c("orange")) +
  labs(title=(expression(paste("Female")))) +
  xlab(wrapper("DGRP3 line (organized by increasing female mean)", width = 49)) + ylab("Mean survival proportion") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.4, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.4, linetype = "solid", colour = "black"),
        axis.title.x = element_text(size = 6.527), axis.title.y = element_text(size = 6.527), axis.text.x = element_blank(),axis.ticks.x = element_blank(), axis.text.y= element_text(size = 6.527),
        plot.title = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position="none")
        
#p2 is the plot showcasing male data.

p2 <- ggplot() +
  geom_point(data = maledata, aes(x = reorder(Line, Mean), y = Mean, color = Sex), size=0.4) +
  geom_errorbar(data = maledata, aes(x = reorder(Line, Mean), ymin=as.numeric(Mean)-as.numeric(StError), ymax=as.numeric(Mean)+as.numeric(StError), color = Sex), width=0.8, size = 0.2,
                position=position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.015,1.03), expand = c(0,0)) +
  scale_color_manual(values=c("purple")) +
  labs(title=(expression(paste("Male")))) +
  xlab(wrapper("DGRP3 line (organized by increasing male mean)", width = 49)) + ylab("Mean survival proportion") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.4, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.4, linetype = "solid", colour = "black"),
        axis.title.x = element_text(size = 6.527), axis.title.y = element_text(size = 6.527), axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.text.y= element_text(size = 6.527),
        plot.title = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position="none")
    
#Combine p1 and p2 on the same plot.

combined_plot <- grid.arrange(p1,p2,nrow=1)

#Print to check the plot's appearance.

print(combined_plot)

#To save the plot in a specific file format and adjust the aspect ratio, you can tinker with the following line of code.
#Be aware that aspect ratio adjustments may interfere with the appearance of fonts and line thickness.
#Please import the plot into Adobe Illustrator to add figure labels.

ggsave("DGRP3_separate_sex_graph.tiff", plot = combined_plot, width = 6.5, height = 3.5, units = "in", dpi = 600)



