setwd("/Users/student/Desktop/R_Workspace/OakOverlap")
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(reshape2)

#graphs to be used in article

#Gender Grades
gf1.data <- read.csv("Grades_Filtered.csv")

introlist = rev(c("Methods in Scientific Inquiry", "Computer Science Inquiry", "Scientific Inquiries - Physics", "Scientific Inquiries - Biology", "Scientific Inquiries - Chemistry", "Literary Explorations I", "American Studies", "Moving and Learning"))
introgrades.data <- gf1.data[gf1.data$Course_Name %in% introlist,]
genderintro.data <- aggregate(introgrades.data[,"Grade_Point_Solid"],
                              by = introgrades.data[,c("Gender", "Course_Name")],
                              FUN = mean, na.rm = T)
genderintro.data$x <- round(genderintro.data$x, digits = 2)
genderintro.data$Course_Name <- factor(genderintro.data$Course_Name, levels = introlist)
colnames(genderintro.data)[3] <- "Grade_Point_Average"
#write.csv(genderintro.data, "genderintro.csv")
sc.data <- read.csv("genderintro.csv")
sc.data$Course_Name <- factor(sc.data$Course_Name, levels = c("AmStud", "LitEx I", "M&L", "CSI", "MSI", "SI Bio", "SI Chem", "SI Physics"))
sc <- ggplot(data = sc.data, aes(x = Course_Name, y = Grade_Point_Average, fill = Gender, 
                           text = paste("Full Course Name:", Full_Course_Name))) +
  geom_bar(position = "dodge", stat = "identity", width = .85) +
  labs(title = "Average Grade Point Earned in Sophomore Courses", fill = " ", y = "Grade Point Average", x = "Course") +
  ylim(0,4)
#+ coord_flip() +
# geom_text(aes(label = Grade_Point_Average), position = position_dodge(width=0.9), vjust=-0.0, hjust = 0)
sc1 <- ggplotly(sc, width = 650, height = 450)
api_create(sc1, 
           filename = "soph-courses-gpa", 
           fileopt = "overwrite", 
           sharing = "public")



#Gender course plot
grades.data <- read.csv("Grades_Enrollment.csv")
grades2.data <- read.csv("Grades_Enrollment_2.csv")

allcourses <- 
  ggplot(data = grades2.data, 
         aes(x = F_S_Average, y = M_S_Average, color = Subject, 
             text = paste("Course:", Course_Name))) +
  geom_point(alpha = .6) + 
  geom_abline(intercept = 0, slope = 1, alpha = .3) + 
  geom_vline(xintercept = 0, alpha = .3) + 
  geom_hline(yintercept = 0, alpha = .3) +
  xlim(0,120) + ylim(0,120)  +
  ggtitle("Distribution of IMSA Courses by Sex") + 
  labs(color = "Course Category") +
  xlab("Female Students Per Semester (Mean)") + 
  ylab("Male Students Per Semester (Mean)") 
ggplotly(allcourses, width = 600, height = 410)

#mathplacements
mp.data <- read.csv("math_placements.csv")
x <- c("Geometry I/II", "Mathematical Investigations I/II", 
       "Mathematical Investigations II", "Mathematical Investigations III",
       "Mathematical Investigations IV", "BC Calculus (I, II, or III)",
       "Beyond BC Calculus")
mathcourses <- rev(x)
#order levels in factor
mp.data$Course_Name <- factor(mp.data$Course_Name, levels = x)
mp <- ggplot (data = mp.data[mp.data$IMSA_SchedYearofGraduation == "2017",],
        aes (x = Course_Name, y = , fill = Gender)) +
  scale_x_discrete(limits = mathcourses) +
  geom_bar(width = .4, position = "dodge") + coord_flip()
ggplotly(mp)
#plotly version
count6 <- function(x) { 
  length(na.omit(x)) / 6 
}
mathag.data <- aggregate(mp.data[,"Grade_Point_Solid"],
                              by = mp.data[,c("Gender", "Course_Name")],
                              FUN = "count6")
mathag2.data <- cast(mathag.data, Course_Name ~ Gender)
ma <- plot_ly(mathag2.data, x = ~Female, type = "bar", orientation = "h",
              y = ~Course_Name, name = "Female", marker = list(color = ("rgba(248, 118, 109, 1)"))) %>%
  add_trace(x = ~Male, name = "Male", marker = list(color = ("rgba(0, 191, 196, 1)"))) %>%
  layout(title = "An Average Year of Sophomore Math Placements", yaxis = list(title = "Course"), xaxis = list(title = "Number of Students"), 
         barmode = "group", margin = list(l = 220)) 
api_create(ma, 
           filename = "math-placements", 
           fileopt = "overwrite", 
           sharing = "public")
