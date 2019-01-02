setwd("/Users/student/Desktop/R_Workspace/OakOverlap")
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(reshape)
library(reshape2)

#Work with actual grade averages, etc. 

#Create dataframe with average grade + sd for each course
gf1.data <- read.csv("Grades_Filtered.csv")

gf2.data <- aggregate(gf1.data[,"Grade_Point"], 
                      by = list(gf1.data[,"Course_Name"]),
                      FUN = mean, na.rm = T)
gf3.data <- aggregate(gf1.data[,"Grade_Point"], 
                      by = list(gf1.data$"Course_Name"),
                      FUN = sd, na.rm = T)
Course_Name <- gf2.data$Group.1
Average_Grade <- gf2.data$x
Standard_Deviation <- gf3.data$x
gf.data <- data.frame(Course_Name, Average_Grade, Standard_Deviation)

#Distribution of IMSA courses'average grades
#ggplot(data = gf.data, aes(x = gf.data$Course_Name, y = gf.data$Average_Grade)) + 
#        geom_col() + coord_flip()
ggplot(data = gf.data, aes(x = gf.data$Average_Grade)) + 
  geom_histogram(bins = 40) +
  xlab("Grade Point Average") + 
  ylab("Number of Courses") + 
  ggtitle("IMSA Average Course Grades")

#GPA change as semesters pass
gf4.data <- gf1.data[!(gf1.data$IMSA_SchedYearofGraduation %in% c("2012", "2013")) & 
                       gf1.data$Anomaly == "N",]
avgpa.data <- aggregate(gf4.data[,"Grade_Point_Solid"],
                        by = gf4.data[,c("IMSA_SchedYearofGraduation", "Student_SemesterAtIMSA")],
                        FUN = mean, na.rm = T)
colnames(avgpa.data)[3] <- "Average_GPA"
avgpa.data$IMSA_SchedYearofGraduation <- as.factor(avgpa.data$IMSA_SchedYearofGraduation)
gpatime <- 
  ggplot(data = avgpa.data, 
       aes(x = Student_SemesterAtIMSA, y = Average_GPA, color = IMSA_SchedYearofGraduation)) +
  geom_line(size = .7) + 
  geom_point() +
  scale_x_discrete(limits = (1:6)) + 
  ylim(c(2,4)) +
  ggtitle("Average GPA vs. Semester at IMSA") +
  labs(color = "(+'s and -'s \n Ignored)") + 
  xlab("Semester Spent at IMSA") + 
  ylab("Average GPA Earned")
#+ scale_color_brewer(palette = "Set1")
ggplotly(gpatime)
api_create(gpatime, filename = "semestergpa", fileopt = "overwrite", sharing = "public")
#Linear regression 
ggplot(data = avgpa.data, 
       aes(x = Student_SemesterAtIMSA, y = Average_GPA)) + 
  geom_point() + geom_smooth(method=lm, se = FALSE) + ylim(c(0,4))
#https://plot.ly/~lliu12/38

#Grade Distributions
gf1.data$Grade <- factor(gf1.data$Grade, levels = c("D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A"))
ggplot(data = gf1.data, aes(x = Grade, color = Grade)) + 
  geom_bar(aes(y = 100 * ..count.. / sum(..count..))) +
  labs(title = "IMSA Grade Distributions", subtitle = "For Fall 2012 - Spring 2017") +
  ylab("Percentage of Grades Earned") +
  theme(legend.position = "none")

#for all courses but rejected ones 
gf5.data <- read.csv("Grades_By_Course.csv")
gf5.data$Grade <- factor(gf5.data$Grade, levels = c("D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A"))
gmelt.data <- melt(data = gf5.data, id.vars = c("Course_Name", "Grade_Point_Average", "Grade_Point_Solid_Average", "Student_Total_Count", "Course_Coarse_Category", "Course_Fine_Category"), variable.name = "Letter_Grade")
write.csv(gmelt.data, file = "Melted_Grades.csv")
gmeltclean.data <- read.csv("Melted_Grades_Clean.csv")
gmeltclean.data$Letter_Grade <- factor(gmeltclean.data$Letter_Grade, levels = c("D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A"))
rejected <- c("European History", "Physics: Applied Mechanics", "Tennis And Badminton", "Lifeguarding and Water Polo", "Survey of Organic Chemistry", "Web Technologies II", "Medieval Societies", "Film Study: History and Criticism", "Microeconomics", "Macroeconomics", "IMSATube: Non-Fiction Film Study", "Advanced Study in Chemistry", "Independent Study in English", "Advanced Study in English", "Advanced Study in German", "Advanced Study in History/Social Science", "Advanced Study in Physics", "Advanced Study in Music", "Seminar in Biology: Animal Behavior", "Advanced Topics in Mathematics","Seminar in Biology: Bioinformatics", "Seminar in Biology: Neurobiology", "Seminar in Biology: Stem Cell Biology", "Seminar in Biology: Virology", "Sci Inquiries - Molecular Genetics", "Sci Inquiries - Organisms and Ecosystems", "Topics in Recent United States History", "Advanced Ceramics", "Tennis and Badminton", "Polyhedra and Geometric Sculpture", "International Relations")
gmeltclean2.data = subset(gmeltclean.data, !(Course_Name %in% rejected))
gdplot <- ggplot(data = gmeltclean2.data, 
                 aes(x = Letter_Grade, 
                     y = Percentage, 
                     group = Course_Name, 
                     color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("Grade Distributions By Course") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned In Course")
ggplotly(gdplot, width = 700, height = 500)

mathplot <- ggplot(data = gmeltclean.data[gmeltclean.data$Course_Coarse_Category == "Math",], 
                   aes(x = Letter_Grade,
                       y = Percentage,
                       group = Course_Name,
                       color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("Math Course Grade Distributions") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned in Course")
ggplotly(mathplot, width = 700, height = 500)

englishplot <- ggplot(data = gmeltclean.data[gmeltclean.data$Course_Coarse_Category == "English",], 
                   aes(x = Letter_Grade,
                       y = Percentage,
                       group = Course_Name,
                       color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("English Course Grade Distributions") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned in Course")
ggplotly(englishplot, width = 700, height = 500)

scienceplot <- ggplot(data = gmeltclean.data[gmeltclean.data$Course_Coarse_Category == "Science",], 
                      aes(x = Letter_Grade,
                          y = Percentage,
                          group = Course_Name,
                          color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("Science Course Grade Distributions") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned in Course")
ggplotly(scienceplot, width = 700, height = 500)

hisplot <- ggplot(data = gmeltclean.data[gmeltclean.data$Course_Coarse_Category == "History",], 
                      aes(x = Letter_Grade,
                          y = Percentage,
                          group = Course_Name,
                          color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("History Course Grade Distributions") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned in Course")
ggplotly(hisplot, width = 700, height = 500)

langplot <- ggplot(data = gmeltclean.data[gmeltclean.data$Course_Coarse_Category == "Foreign Language",], 
                  aes(x = Letter_Grade,
                      y = Percentage,
                      group = Course_Name,
                      color = Course_Name)) +
  geom_line(alpha = .4) +
  ggtitle("Language Course Grade Distributions") +
  xlab("Letter Grade") +
  ylab("Percentage of Grades Earned in Course")
ggplotly(langplot, width = 700, height = 500)

#math course grades male vs female
mathcourselist = c("Mathematical Investigations I/II", "Mathematical Investigations II", "Geometry", "Mathematical Investigations III", "Mathematical Investigations IV", "BC Calculus I", "BC Calculus II", "BC Calculus III", "BC Calculus I/II", "BC Calculus II/III", "Multivariable Calculus", "AB Calculus I", "AB Calculus II", "Number Theory", "Theory of Analysis", "Differential Equations")
mathgrades.data <- gf1.data[gf1.data$Course_Name %in% mathcourselist,]
gendermath.data <- aggregate(mathgrades.data[,"Grade_Point_Solid"],
          by = mathgrades.data[,c("Gender", "Course_Name")],
          FUN = mean, na.rm = T)
ggplot(data = gendermath.data, aes(x = Course_Name, y = x, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") + coord_flip()

#Soph grades male vs female
introlist = rev(c("Methods in Scientific Inquiry", "Computer Science Inquiry", "Scientific Inquiries - Physics", "Scientific Inquiries - Biology", "Scientific Inquiries - Chemistry", "Literary Explorations I", "American Studies", "Moving and Learning"))
introgrades.data <- gf1.data[gf1.data$Course_Name %in% introlist,]
genderintro.data <- aggregate(introgrades.data[,"Grade_Point_Solid"],
                             by = introgrades.data[,c("Gender", "Course_Name")],
                             FUN = mean, na.rm = T)
genderintro.data$x <- round(genderintro.data$x, digits = 2)
genderintro.data$Course_Name <- factor(genderintro.data$Course_Name, levels = introlist)

#needed to fit plot_ly syntax
gim.data <- melt(genderintro.data, id.vars = c("Course_Name", "x"))
gic.data <- cast(genderintro.data, Course_Name ~ Gender)


#plot_ly version of soph course grades by gender
sc <- plot_ly(gic.data, x = ~Female, type = "bar", orientation = "h",
              y = ~Course_Name, name = "Female", marker = list(color = ("rgba(248, 118, 109, 1)"))) %>%
  add_trace(x = ~Male, name = "Male", marker = list(color = ("rgba(0, 191, 196, 1)"))) %>%
  layout(title = "Sophomore Course Grades by Gender", yaxis = list(title = "Course"), 
         xaxis = list(title = "Grade Point Average", range = c(0,4)), 
         barmode = "group", margin = list(l = 220)) 
#  %>%layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

#ggplotly version of soph course grades by gender
ggplot(data = genderintro.data, aes(x = Course_Name, y = x, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") +
   labs(title = "Sophomore Course Grades By Gender", y = "Grade Point Average", x = "Course") +
  ylim(0,4) + coord_flip() +
  geom_text(aes(label = x), position = position_dodge(width=0.9), vjust=-0.0, hjust = 0)

api_create(sc, 
           filename = "soph-courses-gpa",
           fileopt = "overwrite", 
           sharing = "public")

#Try all grades male vs female
#Too many classes to be functional
gendergrades.data <- aggregate(gf1.data[,"Grade_Point_Solid"],
                              by = gf1.data[,c("Gender", "Course_Name")],
                              FUN = mean, na.rm = T)
ggplot(data = gendergrades.data, aes(x = Course_Name, y = x, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") + coord_flip() +
  coord_fixed(ratio = 50)
#Grade dis bar graphs - load gmeltclean first

#Gender grades dis. for any class (followed by more specific cases)
ggp.data <- read.csv("grades_gender_percentages.csv")
colorlist <- c("plum", "plum4", "steelblue2", "steelblue", "steelblue4", "palegreen", "palegreen3", "seagreen", "lightgoldenrod1")
ggp2.data <- melt(data = ggp.data, id.vars = c("Course", "Gender"), variable.name = "Grade")
#write.csv(ggp2.data, "ggp.csv")
ggp3.data <- read.csv("ggp3.csv")
ggp3.data$Grade <- factor(ggp3.data$Grade, levels = rev(c("D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")))
ggplot(data = ggp3.data[ggp3.data$Course == "Scientific Inquiries - Physics",], aes(x = Gender, y = Percentage, fill = Grade)) +
  geom_bar(position = "stack", stat = "identity") + scale_fill_manual(values = colorlist) +
  labs(title = "SI Physics Grades by Gender", x = "Gender", y = "Percentage", fill = " ")
#interesting ones: Compusci
course <- "Multi-Variable Calculus"
p <- ggplot(data = ggp3.data[ggp3.data$Course == course,], aes(x = Gender, y = Percentage, fill = Grade)) +
  geom_bar(position = "stack", stat = "identity", width = .85) + scale_fill_manual(values = colorlist) +
  labs(title = paste0(course, " Grades by Gender"), x = "Gender", y = "Percentage", fill = " ")
pp <- ggplotly(p, dynamicTicks = FALSE) #%>% layout(xaxis = list(type = "category"))
ggp4.data <- read.csv("ggp4.csv")

pp <- plot_ly(ggp4.data[ggp4.data$Course == course,], x = ~Gender, type = "bar",
              y = ~D, name = "D", color = I("lightgoldenrod")) %>%
  add_trace(y = ~Cminus, name = "C-", color = I("seagreen")) %>%
  add_trace(y = ~C, name = "C", color = I("palegreen3")) %>%
  add_trace(y = ~Cplus, name = "C+", color = I("palegreen")) %>%
  add_trace(y = ~Bminus, name = "B-", color = I("steelblue4")) %>%
  add_trace(y = ~B, name = "B", color = I("steelblue")) %>%
  add_trace(y = ~Bplus, name = "B+", color = I("steelblue2")) %>%
  add_trace(y = ~Aminus, name = "A-", color = I("plum4")) %>%
  add_trace(y = ~A, name = "A", color = I("plum")) %>%
  layout(title = paste0("SI Physics: Grades by Gender"), xaxis = list(title = "Gender"), yaxis = list(title = "Percentage"), barmode = 'stack')

api_create(pp, 
          filename = "siphys-grades",
           fileopt = "overwrite", 
           sharing = "public")

#Male vs. Female grades scatterplot for all classes
mvsf.data <- aggregate(gf1.data[,"Grade_Point_Solid"],
                       by = gf1.data[,c("Gender", "Course_Name")],
                       FUN = mean, na.rm = T)
#mvsfm.data <- melt(mvsf.data, id.vars = c("Course_Name", "x"))
mvsfc.data <- cast(mvsf.data, Course_Name ~ Gender)
#write.csv(mvsfc.data, "mvsf.csv") DONT DO THIS ITLL MESS THE CSV UP
mf.data <- read.csv("mvsf.csv")
#ggplot(data = mf.data, aes(x = Female, y = Male)) + geom_point()
mf.data$Female <- round(mf.data$Female, digits = 2)
mf.data$Male <- round(mf.data$Male, digits = 2)
mf.data$Course_Category_Detailed <- factor(mf.data$Course_Category_Detailed, levels = c("Default", "Biology", "Chemistry", "Computer Science", "Physics", "Other Science"))
mf1 <- ggplot(data = mf.data, aes(x = Female, y = Male, color = Course_Category, text = paste("Course:", Course_Name))) +
  geom_point(alpha = .6) + geom_abline(intercept = 0, slope = 1, alpha = .3) + xlim(2.3, 4) + ylim(2.3,4) +
  labs(x = "Female Grade Point Average", y= "Male Grade Point Average", color = " ", title = "Course Performance by Gender") 
mf2 <- ggplotly(mf1, width = 600, height = 400)
api_create(mf2, 
           filename = "grades-comparison",
           fileopt = "overwrite", 
           sharing = "public")
