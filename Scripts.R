library(readxl)

data <- read_excel(file.choose())

data$Gender <- as.factor(data$Gender)
data$College <- as.factor(data$College)
data$Academic <- as.factor(data$Academic)
data$Origin <- as.factor(data$Origin)
data$Living <- as.factor(data$Living)

set.seed(88673)
sample_data<-data.frame(data[sample(1:dim(data)[1], size=100),])

################## Gender #######################
print("Gender: [M=1;F=2]:")
print("Barplot of Gender")
summary(sample_data$Gender)
barplot(table(sample_data$Gender), main="Gender description of students in STA201 S22: ",xlab="Number Of Students:",ylab="Gender of Student:")

print("Percentage of Females over Males")
print(41)
print("Percentage of Males over Females")
print(100 - 41)


################## Academic Status #######################

print("Academic Status: [Freshman=1;Sophomore=2;Junior=3;Senior=4]: ")
print("Summary statistics for Academic Status: ")
summary(sample_data$Academic)
print("Barchart for Academic status: ")
barplot(table(sample_data$Academic), main = "Academic Status of Students in STA 201 S22",xlab="Academic Status", ylab = "Number of Students")

print("Percentage of Freshman:")
print(35)
print("Percentage of Sophomores:")
print(19)
print("Percentage of Juniors:")
print(28)
print("Percentage of Seniors:")
print(18)


################## Region of Origin #######################
print("Region of Living: [GCC=1;Africa=2;ME=3;Other=4]:")
print("Summary statistics for Region of Living:")
summary(sample_data$Origin)


print("Barplot for Origin:")
barplot(table(sample_data$Origin),main="Region of Living of students in STA 201 S22",xlab="Region of Living", ylab="Number of students")

print("Percentage of GCC:")
print(27)
print("Percentage of Africa:")
print(21)
print("Percentage of ME:")
print(38)
print("Percentage of Other:")
print(14)


################## Place of Living #######################
print("Place of Living: [On campus=1;Off campus=2]")
print("Summary Statistics for Place of Living:")
summary(sample_data$Living)
print("Barplot for Place of Living")
barplot(table(sample_data$Living), main = "Place of living of students in STA 201 S22",xlab="Place of living", ylab="Number of studnets")

print("Percentage of Students on Campus")
print(43)
print("Percentage of Students off Campus")
print(57)




################## Comparing GPAs #######################
gpa_males   <- sample_data$GPA[sample_data$Gender == 1]
gpa_females <- sample_data$GPA[sample_data$Gender == 2]

print("Boxplot of Males and Female GPA")
boxplot(gpa_males, gpa_females, names = c("Males", "Females"), main = "Boxplot of Gender and GPA", xlab = "Gender", ylab = "GPA")

print("Summary of Male GPA:")
summary(gpa_males)
print("Summary of Female GPA: ")
summary(gpa_females)
print("Test for homogeneity of Variance")
var.test(gpa_females, gpa_males)
print("Two Samples t-test between Male and Female GPA")
t.test(gpa_females, gpa_males, conf.level=0.95, alternative = "greater", var.equal=F)




################## Average number of Hours studied for on vs off campus #######################

studytime_on      <- sample_data$Sudying_Time[sample_data$Living == 1]
studytime_off     <- sample_data$Sudying_Time[sample_data$Living == 2]

print("Test for Homogeneity of variance:")
var.test(studytime_on, studytime_off)

print("Two sample t-test between students on vs off campus on Studying Time:")
t.test(studytime_on, studytime_off, conf.level=0.95,var.equal=F)


################## 0.75 of students from non GCC origin #######################

origins_of_gcc <- sample_data$Origin[sample_data$Origin == 1]
origins_of_nongcc <- 100 - length(origins_of_gcc)
print("Number of people with GCC origin:")
length(origins_of_gcc)
print("Number of people without GCC origin:")
length(origins_of_nongcc)
print("One sample t-test for Region of Origin within GCC")
prop.test(origins_of_nongcc, n=100, p=0.75, alternative = "greater", conf.level = 0.95, correct = "FALSE")



################## Comparing Studying Time & GPA #######################

summary(lm(sample_data$Sudying_Time~sample_data$GPA))

scatter.smooth(x = sample_data$GPA, y = sample_data$Sudying_Time, ylab="Hours of Studying", main = "Relationship between hours of studying & GPA")


