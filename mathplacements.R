setwd("/Users/student/Desktop/R_Workspace/OakOverlap")
library(ggplot2)
library(plotly)

#Math Placement Data Plots
#Lucy Liu
#July 2017


#to set up the dataset
mp.data <- read.csv("math_placements.csv")
x <- c("Geometry I/II", "Mathematical Investigations I/II", 
       "Mathematical Investigations II", "Mathematical Investigations III",
       "Mathematical Investigations IV", "BC Calculus (I, II, or III)",
       "Beyond BC Calculus")
mathcourses <- rev(x)
#order levels in factor
mp.data$Course_Name <- factor(mp.data$Course_Name, levels = mathcourses)

#distribution of math placements by graduating class and gender
ggplot(data = mp.data, 
       aes(x = mp.data$IMSA_SchedYearofGraduation, y = , fill = mp.data$Course_Name)) +
  geom_bar(position = "dodge") + scale_x_discrete(limits = (2014:2019)) + 
  ggtitle("Sophomore Math Placements") 
  ggplot(data = mp.data, aes(x = Course_Name, fill = Gender)) +
  geom_bar(position = "stack") + 
  coord_flip() +
  facet_grid(. ~ IMSA_SchedYearofGraduation) +
  labs(title = "Sophomore Math Placements", 
       subtitle = "By Gender and Graduating Year",
       x = "Course", y = "Number of Students", fill = "") +
  theme(legend.position = "top")
#gender distribution of math placements
ggplot (data = mp.data[mp.data$IMSA_SchedYearofGraduation == "2017",],
        aes (x = Course_Name, y = , fill = Gender)) +
  scale_x_discrete(limits = mathcourses) +
  geom_bar(width = .4, position = "dodge") + coord_flip()
#divide all y-counts by 6 to get the average by year
ggplot (data = mp.data,
        aes (x = Course_Name, y = , fill = Gender)) +
  scale_x_discrete(limits = x) +
  geom_bar(aes(y = (..count../6)),width = .6, position = "dodge") + 
  coord_flip() +
  labs(title = "An Average Year of IMSA Sophomore Math Placements", 
       y = "Number of Students", x = "Course", fill = "") +
  theme(legend.position = c(.8,.8))
#  theme(axis.text = element_text(size = 12))

#gender distribution in MI4 over the years
ggplot(mp.data[mp.data$Course_Name == "Mathematical Investigations IV",],
       aes(x = IMSA_SchedYearofGraduation, y = , fill = Gender)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = 2014:2019)
