setwd("/Users/student/Desktop/R_Workspace/OakOverlap")
library(ggplot2)
library(plotly)
library(RColorBrewer)

#Course gender distributions
#July 2017


grades.data <- read.csv("Grades_Enrollment.csv")
grades2.data <- read.csv("Grades_Enrollment_2.csv")
#grades.data$Grade_TermID <- as.factor(grades.data$Grade_TermID)

unique(grades.data$Course_Name)
#gender dis. over the years for any course in the dataset
ggplot(data = grades.data[grades.data$Course_Name == "Multi-Variable Calculus",], 
       aes(x = Grade_Year, fill = Gender)) + geom_histogram(bins = 30) +
  xlab(label="Year") + ylab("# of Students") + 
  ggtitle("Gender Distribution in Multi-Variable Calculus")

#total gender dis. for selected courses (this is for compsci rn)
ggplot(data = grades.data[grades.data$Course_Name == "Advanced Programming" |
                            grades.data$Course_Name == "Computer Science Inquiry" |
                            grades.data$Course_Name == "Computational Science" |
                            grades.data$Course_Name == "Robotics Programming" |
                            grades.data$Course_Name == "Web Technologies I" |
                            grades.data$Course_Name == "Web Technologies II" |
                            grades.data$Course_Name == "Object Oriented Programming",], 
       aes(x = Course_Name, fill = Gender)) + geom_bar() +
  xlab(label="Class") + ylab("# of Students") + 
  ggtitle("CS Classes")


#Scatterplots of average female vs. male enrollment in a class for each semester it is run.
grades2.data$F_S_Average <- round(grades2.data$F_S_Average, digits = 2)
grades2.data$M_S_Average <- round(grades2.data$M_S_Average, digits = 2)
allcourses <- 
  ggplot(data = grades2.data, 
         aes(x = F_S_Average, y = M_S_Average, color = Subject, 
             text = paste("Course:", Course_Name))) +
  geom_point(alpha = .6) + 
  geom_abline(intercept = 0, slope = 1, alpha = .3) + 
  geom_vline(xintercept = 0, alpha = .3) + 
  geom_hline(yintercept = 0, alpha = .3) +
  xlim(0,120) + ylim(0,120)  +
  ggtitle("IMSA Course Demographics") + 
  labs(color = " ") +
  xlab("Female Students Per Semester (Mean)") + 
  ylab("Male Students Per Semester (Mean)") 
ac <- ggplotly(allcourses, width = 600, height = 420)

#upload to plotly
api_create(ac, 
           filename = "allcourse-genderdis", 
           fileopt = "overwrite", 
           sharing = "public")

sciences <- ggplot(data = grades2.data[grades2.data$Subject == "Science",], 
                   aes(x = F_S_Average, y = M_S_Average, 
                       color = Subject_Fine, text = paste("Course:", Course_Name))) +
  geom_abline(intercept = 0, slope = 1, alpha = .6) + geom_point(alpha = .6) + xlim(0,120) + ylim(0,120) +
  ggtitle("Distribution of IMSA Science Courses by Sex") + labs(color = "Subject") + 
  xlab("Female Students Per Semester (Mean)") + ylab("Male Students Per Semester (Mean)")
ggplotly(sciences, width = 600, height = 410) 

languages <- ggplot(data = grades2.data[grades2.data$Subject == "Foreign Language",], 
                    aes(x = F_S_Average, y = M_S_Average, 
                        color = Subject_Fine, text = paste("Course:", Course_Name))) +
  geom_abline(intercept = 0, slope = 1, alpha = .6) + geom_point(alpha = .6) + xlim(0,120) + ylim(0,120) +
  ggtitle("Distribution of IMSA Language Courses by Sex") + labs(color = "Language") + 
  xlab("Female Students Per Semester (Mean)") + ylab("Male Students Per Semester (Mean)")
ggplotly(languages, width = 600, height = 410)

count <- function(x) { 
  length(na.omit(x)) 
}
#Use this vector to filter out certain courses.
selected <- c("Adv. Chemistry--Structure and Properties", "Creative Writing Workshop", "Microbes and Disease", "Outdoor and Indoor Games", "Modern Theater", "Spanish III", "French III", "Physics: Sound and Light", "Object Oriented Programming", "Molecular and Cellular Biology", "Modern World Fiction", "Engineering", "Organic Chemistry I", "Physics: Calculus-based Mechanics", "Modern Physics", "Physiology and Disease", "Biochemistry", "Discrete Mathematics", "Speculative Fiction Studies", "Evolution, Biodiversity, and Ecology", "Multi-Variable Calculus", "Planetary Science", "Environmental Chemistry", "AB Calculus I", "Movement and Relaxation", "Statistical Exploration and Description", "Computational Science", "Differential Equations", "BC Calculs I", "BC Calculus II", "BC Calculus III", "BC Calculus I/II", "German I", "Japanese I", "Chinese I")
rejected <- c("European History", "Seminar in Biology: Animal Behavior", "Advanced Topics in Mathematics","Seminar in Biology: Bioinformatics", "Seminar in Biology: Neurobiology", "Seminar in Biology: Stem Cell Biology", "Seminar in Biology: Virology", "Sci Inquiries - Molecular Genetics", "Sci Inquiries - Organisms and Ecosystems", "Topics in Recent United States History", "Advanced Ceramics", "Tennis and Badminton", "Polyhedra and Geometric Sculpture", "International Relations", "Power and Authority in History")
grades3.data = subset(grades.data, !(Course_Name %in% rejected))

grades4.data = grades3.data[grades3.data$Student_GradeLevel != "NULL",]
#grades4.data = grades.data[grades.data$Student_GradeLevel != "NULL",]
demographics.data <- aggregate(grades4.data[,"Grade_Point"], 
                               by = grades4.data[,c("Course_Name", "Gender", "Student_GradeLevel")],
                               FUN = "count")

#save demographics.data
write.csv(demographics.data, "Demographics")
colnames(demographics.data)[4] <- "Student_Count"
Students_Enrolled_Over_6_Year_Period <- demographics.data$Student_Count
demographics.data$Gender <- as.factor(demographics.data$Gender)
demographicsplot <- 
  ggplot(data = demographics.data, 
       aes(x = Gender, y = Student_GradeLevel, color = Course_Name)) +
  geom_point(aes(size = Students_Enrolled_Over_6_Year_Period)) + 
  scale_size_continuous(range = c(1, 40)) + 
  ggtitle("IMSA Course Demographics, Fall 2011-Spring 2017") +
  labs(size = " ", color = " ") + 
  xlab("Sex: 1 = Female, 2 = Male") +
  ylab("Student Grade Level")

d4upload <- ggplotly(demographicsplot, width = 700, height = 500) %>%
  layout(legend = list(x = 110, y = 0))
api_create(d4upload, filename = "allcourse-dis", fileopt = "overwrite", sharing = "public")

