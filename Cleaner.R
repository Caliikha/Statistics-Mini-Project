library(readxl)
library(writexl)

data <- read_excel("./ProjectData.xlsx")
length(data$ID)

Q1_of_height        <-  quantile(na.omit(data$Height), prob=c(.25))
median_of_height    <-  quantile(na.omit(data$Height), prob=c(.5))
Q3_of_height        <-  quantile(na.omit(data$Height), prob=c(.75))

lower_bound_height  <-  Q1_of_height - (1.5 * Q3_of_height - Q1_of_height)
upper_bound_height  <-  Q3_of_height + (1.5 * Q3_of_height - Q1_of_height)

Q1_of_weight        <-  quantile(na.omit(data$Weight), prob=c(.25))
median_of_weight    <-  quantile(na.omit(data$Weight), prob=c(.5))
Q3_of_weight        <-  quantile(na.omit(data$Weight), prob=c(.75))

lower_bound_weight  <-  Q1_of_weight - (1.5 * Q3_of_weight - Q1_of_weight)
upper_bound_weight  <-  Q3_of_weight + (1.5 * Q3_of_weight - Q1_of_weight)

Q1_of_allowance     <-  quantile(na.omit(data$Allowance), prob=c(.25))
median_of_allowance <-  quantile(na.omit(data$Allowance), prob=c(.5))
Q3_of_allowance     <-  quantile(na.omit(data$Allowance), prob=c(.75))

lower_bound_allowance  <-  Q1_of_allowance - (1.5 * Q3_of_allowance - Q1_of_allowance)
upper_bound_allowance  <-  Q3_of_allowance + (1.5 * Q3_of_allowance - Q1_of_allowance)

Q1_of_StudyTime     <-  quantile(na.omit(data$Sudying_Time), prob=c(.25))
median_of_StudyTime <-  quantile(na.omit(data$Sudying_Time), prob=c(.5))
Q3_of_StudyTime     <-  quantile(na.omit(data$Sudying_Time), prob=c(.75))

lower_bound_StudyTime  <-  Q1_of_StudyTime - (1.5 * Q3_of_StudyTime - Q1_of_StudyTime)
upper_bound_StudyTime  <-  Q3_of_StudyTime + (1.5 * Q3_of_StudyTime - Q1_of_StudyTime)

data <- na.omit(data)
data
new_data <- data[ which((data$Gender <= 2 | data$Gender >= 1)
                       &(data$College >=1 | data$College <=4)
                       &(data$Academic>=1 | data$Academic<=4)
                       &(data$Origin>= 1 | data$Origin <= 4)
                       &(data$Living ==1 | data$Living ==2)
                       &(data$GPA %in% c(0:4))
                       &(data$Height %in% c(lower_bound_height:upper_bound_height))
                       &(data$Weight %in% c(lower_bound_weight:upper_bound_weight))
                       &(data$Allowance %in% c(lower_bound_allowance:upper_bound_allowance))
                       &(data$Sudying_Time %in% c(lower_bound_StudyTime:upper_bound_StudyTime))), ]


new_data
write_xlsx(new_data, "NewProjectData.xlsx")
