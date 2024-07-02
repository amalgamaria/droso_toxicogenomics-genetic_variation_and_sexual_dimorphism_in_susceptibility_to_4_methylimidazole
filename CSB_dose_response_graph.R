#This R code creates a dose response graph using the CSB data.
#This code uses R version 4.3.3 (2024-02-29 ucrt) and RStudio version 2023.12.01 "Ocean Storm" Release.
#Clear the working environment.

rm(list=ls())

#Import necessary libraries - if an unexplained issue is occurring, you may need to update
#your RStudio application or R version.

library(ggplot2)

#Read in data. Please update the file path as needed. 

data = read.csv("/Users/Katelynne/Desktop/updated_codes/CSB/csb_doseresponse_newse.csv")

#Check that the data has imported correctly.

head(data)
tail(data)

#The following code will separate the male and female data into two different data sets.
#Use print to check that the data has separated correctly.

ls(data)
data$Sex
data %>% select(sex, concentration)
femaledata <- data %>%
  filter(sex == "F") %>%
  select(sex, concentration, mean, sterror)
print(femaledata)
maledata <- data %>%
  filter(sex == "M") %>%
  select(sex, concentration, mean, sterror)
print(maledata)

wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Create Figure using the ggplot2 library.

library(ggplot2)
p1 <- ggplot() +
  geom_line(data = femaledata, aes(x = concentration, y = mean, color = sex), size=0.5) +
  geom_errorbar(data = femaledata, aes(x = concentration, ymin=mean-sterror, ymax=mean+sterror, color = sex), width=1.0, linewidth=0.35,
                position=position_dodge(0.1)) +
  geom_point(data= femaledata, aes(x = concentration, y = mean, color = sex), size = 1.0) +
  geom_line(data = maledata, aes(x = concentration, y = mean, color = sex), size=0.5) +
  geom_errorbar(data = maledata, aes(x = concentration, ymin=mean-sterror, ymax=mean+sterror, color = sex), width=1.0, linewidth=0.35,
                position=position_dodge(0.1)) +
  geom_point(data=maledata, aes(x = concentration, y = mean, color = sex), size = 1.0) +
  scale_color_manual(values = c("F" = "orange", "M" = "purple"), name = "Sex") +
  scale_x_continuous("Concentration of 4-methylimidazole (mM)", limits=c(3,61), breaks = c(0, 10, 20, 30, 40, 50, 60), expand = c(0,2.0)) +
  scale_y_continuous(limits = c(0,1.0), expand = c(0,0.015)) +
  xlab(wrapper("Concentration of 4-methylimidazole", width = 21)) + ylab("Mean survival proportion") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(linewidth = 0.4, linetype = "solid", colour = "black"),
        axis.line.y = element_line(linewidth = 0.4, linetype = "solid", colour = "black"),
        axis.title.x = element_text(size = 9.37), axis.title.y = element_text(size = 9.37), axis.text.y= element_text(size = 9.37),plot.title = element_blank(),
        panel.background = element_rect(fill = "white"), axis.text.x= element_text(size = 9.37), legend.position="none")

#Print graph to check its appearance.

print(p1)

#To save the plot in a specific file format and adjust the aspect ratio, you can tinker with the following line of code.
#Be aware that aspect ratio adjustments may interfere with the appearance of fonts and line thickness. 

ggsave("CSB_Dose_Response.tiff", units="in",width=6.5, height=4.0,dpi=600)

