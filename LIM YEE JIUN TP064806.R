# LIM YEE JIUN TP064806

# IMPORT DATA
student_pred = read.csv("C:\\Users\\Athelene\\OneDrive\\PFDA\\student_prediction.csv", header = TRUE)

# CLEANING DATA - remove duplication
# ASSIGN HEADERS
names(student_pred) = c("STUDENT_ID", "AGE", "SEX", "GRAD_HIGH_SCHOOL_TYPE", "SCHOLARSHIP", 
                        "ADDITIONAL_WORK", "REGULAR_ART_SPORT_ACTIVITY", "PARTNER", "SALARY", 
                        "TRANSPORTATION", "ACCOMMODATION", "MOTHER_EDU", "FATHER_EDU", "SIBLINGS", 
                        "PARENTAL_STATUS", "MOTHER_OCCUPATION", "FATHER_OCCUPATION", "WEEKLY_STUDY_HRS", 
                        "READ_FREQ_NON_SCI", "READ_FREQ_SCI", "ATTENDANCE_DEPT", "PROJECT_IMPACT", "ATTENDANCE_CLASS", 
                        "PREP_STUDY_WITH", "PREP_STUDY_WHEN", "NOTES", "LISTEN", "DISCUSSION", 
                        "FLIP-CLASSROOM", "CUML_GPA", "EXPECTED_GPA", "COURSEID", "GRADE")

# DELETE ROWS
student_pred <- na.omit(student_pred)

# EXCLUDE DUPLICATE
student_pred <- unique(student_pred)

# CHECK MISSING VALUES  
colSums(is.na(student_pred)) 

# PRE-PROCESSING DATA
# CONSOLE
student_pred

# MAX PRINT
student_pred = getOption("max.print")
student_pred
options(max.print = 800000)

# VIEW IN TABLE FORM
View(student_pred) 

# CHECK COLUMN HEADING
names(student_pred)

# DATA EXPLORATION
# HOW DATA STORED
class(student_pred)

# DATA TYPES OF VARIABLES (COLUMN)
str(student_pred) 

# NUMBER OF COLUMNS
length(student_pred) # 33
ncol(student_pred) # 33

# NUMBER OF ROWS
nrow(student_pred) # 1534

# EXPLORING THE DIMENSIONS OF DATA SET
dim(student_pred) # 1534 33

# SUMMARY OF DATA SET
summary(student_pred)

# DATA TRANSFORMATION
# ARRANGE COLUMN IN ACCENDING ORDER
GRADE <- student_pred %>% arrange(GRADE)
print("Arranged Data Frame")
print(GRADE)

# ARRANGE COLUMN IN DESCENDING ORDER
GRADE <- student_pred %>% arrange(desc(GRADE))
print("Arranged Data Frame")
print(GRADE)

# SELECT THE COLUMN NEEDED
ATTRIBUTES <- student_pred %>% select(AGE, WEEKLY_STUDY_HRS, NOTES, GRAD_HIGH_SCHOOL_TYPE, GRADE)
print(ATTRIBUTES)
View(ATTRIBUTES)

# FILTER HIGH GRADE STUDENT
HIGHGRADE <- ATTRIBUTES %>% filter(GRADE == 'AA' | GRADE == 'BA' | GRADE == 'BB' | GRADE == 'BC')
print(HIGHGRADE)

# FILTER LOW GRADE STUDENT
LOWGRADE <- ATTRIBUTES %>% filter(GRADE == 'CC' | GRADE == 'DC' | GRADE == 'DD' | GRADE == 'Fail')
print(LOWGRADE)

# CHANGE NUMERICAL VALUE TO CHARACTERS
# AGE
student_pred['AGE'][student_pred['AGE'] == 1] <- '18 - 21'
student_pred['AGE'][student_pred['AGE'] == 2] <- '22 - 25'
student_pred['AGE'][student_pred['AGE'] == 3] <- 'Above 26'

# GENDER
student_pred['SEX'][student_pred['SEX'] == 1] <- 'Female'
student_pred['SEX'][student_pred['SEX'] == 2] <- 'Male'

# HIGHSCHOOL
student_pred['GRAD_HIGH_SCHOOL_TYPE'][student_pred['GRAD_HIGH_SCHOOL_TYPE'] == 1] <- 'Private'
student_pred['GRAD_HIGH_SCHOOL_TYPE'][student_pred['GRAD_HIGH_SCHOOL_TYPE'] == 2] <- 'State'
student_pred['GRAD_HIGH_SCHOOL_TYPE'][student_pred['GRAD_HIGH_SCHOOL_TYPE'] == 3] <- 'Other'

# SCHOLARSHIP
student_pred['SCHOLARSHIP'][student_pred['SCHOLARSHIP'] == 1] <- 'None'
student_pred['SCHOLARSHIP'][student_pred['SCHOLARSHIP'] == 2] <- '25%'
student_pred['SCHOLARSHIP'][student_pred['SCHOLARSHIP'] == 3] <- '50%'
student_pred['SCHOLARSHIP'][student_pred['SCHOLARSHIP'] == 4] <- '75%'
student_pred['SCHOLARSHIP'][student_pred['SCHOLARSHIP'] == 5] <- 'Full'

# ADDITIONAL_WORK
student_pred['ADDITIONAL_WORK'][student_pred['ADDITIONAL_WORK'] == 1] <- 'Yes'
student_pred['ADDITIONAL_WORK'][student_pred['ADDITIONAL_WORK'] == 2] <- 'No'

# ACTIVITY
student_pred['REGULAR_ART_SPORT_ACTIVITY'][student_pred['REGULAR_ART_SPORT_ACTIVITY'] == 1] <- 'Yes'
student_pred['REGULAR_ART_SPORT_ACTIVITY'][student_pred['REGULAR_ART_SPORT_ACTIVITY'] == 2] <- 'No'

# PARTNER
student_pred['PARTNER'][student_pred['PARTNER'] == 1] <- 'Yes'
student_pred['PARTNER'][student_pred['PARTNER'] == 2] <- 'No'

# SALARY
student_pred['SALARY'][student_pred['SALARY'] == 1] <- '135 - 200'
student_pred['SALARY'][student_pred['SALARY'] == 2] <- '201 - 270'
student_pred['SALARY'][student_pred['SALARY'] == 3] <- '271 - 340'
student_pred['SALARY'][student_pred['SALARY'] == 4] <- '341 - 410'
student_pred['SALARY'][student_pred['SALARY'] == 5] <- 'Above 410'

# TRANSPORTATION
student_pred['TRANSPORTATION'][student_pred['TRANSPORTATION'] == 1] <- 'Bus'
student_pred['TRANSPORTATION'][student_pred['TRANSPORTATION'] == 2] <- 'Private car/taxi'
student_pred['TRANSPORTATION'][student_pred['TRANSPORTATION'] == 3] <- 'Bicycle'
student_pred['TRANSPORTATION'][student_pred['TRANSPORTATION'] == 4] <- 'Other'

# ACCOMMODATION
student_pred['ACCOMMODATION'][student_pred['ACCOMMODATION'] == 1] <- 'Rental'
student_pred['ACCOMMODATION'][student_pred['ACCOMMODATION'] == 2] <- 'Dormitory'
student_pred['ACCOMMODATION'][student_pred['ACCOMMODATION'] == 3] <- 'With family'
student_pred['ACCOMMODATION'][student_pred['ACCOMMODATION'] == 4] <- 'Other'

# MOTHER_EDU
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 1] <- 'Primary School'
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 2] <- 'Secondary School'
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 3] <- 'High School'
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 4] <- 'University'
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 5] <- 'MSc.'
student_pred['MOTHER_EDU'][student_pred['MOTHER_EDU'] == 6] <- 'Ph.D.'

# FATHER_EDU
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 1] <- 'Primary School'
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 2] <- 'Secondary School'
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 3] <- 'High School'
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 4] <- 'University'
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 5] <- 'MSc.'
student_pred['FATHER_EDU'][student_pred['FATHER_EDU'] == 6] <- 'Ph.D.'

# NUM_SIBLINGS
student_pred['SIBLINGS'][student_pred['SIBLINGS'] == 5] <- '5 or above'

# PARENTAL_STATUS
student_pred['PARENTAL_STATUS'][student_pred['PARENTAL_STATUS'] == 1] <- 'Married'
student_pred['PARENTAL_STATUS'][student_pred['PARENTAL_STATUS'] == 2] <- 'Divorced'
student_pred['PARENTAL_STATUS'][student_pred['PARENTAL_STATUS'] == 3] <- 'Died - one of them or both'

# MOTHER_OCCUPATION
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 1] <- 'Retired'
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 2] <- 'Housewife'
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 3] <- 'Government officer'
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 4] <- 'Private sector employee'
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 5] <- 'Self-employment'
student_pred['MOTHER_OCCUPATION'][student_pred['MOTHER_OCCUPATION'] == 6] <- 'Other'

# FATHER_OCCUPATION
student_pred['FATHER_OCCUPATION'][student_pred['FATHER_OCCUPATION'] == 1] <- 'Retired'
student_pred['FATHER_OCCUPATION'][student_pred['FATHER_OCCUPATION'] == 2] <- 'Government officer'
student_pred['FATHER_OCCUPATION'][student_pred['FATHER_OCCUPATION'] == 3] <- 'Private sector employee'
student_pred['FATHER_OCCUPATION'][student_pred['FATHER_OCCUPATION'] == 4] <- 'Self-employment'
student_pred['FATHER_OCCUPATION'][student_pred['FATHER_OCCUPATION'] == 5] <- 'Other'

# STUDY_HOURS
student_pred['WEEKLY_STUDY_HRS'][student_pred['WEEKLY_STUDY_HRS'] == 1] <- 'None'
student_pred['WEEKLY_STUDY_HRS'][student_pred['WEEKLY_STUDY_HRS'] == 2] <- '< 5 hours'
student_pred['WEEKLY_STUDY_HRS'][student_pred['WEEKLY_STUDY_HRS'] == 3] <- '6 - 10 hours'
student_pred['WEEKLY_STUDY_HRS'][student_pred['WEEKLY_STUDY_HRS'] == 4] <- '11 - 20 hours'
student_pred['WEEKLY_STUDY_HRS'][student_pred['WEEKLY_STUDY_HRS'] == 5] <- '> 20 hours'

# READ_FREQ_NON_SCI
student_pred['READ_FREQ_NON_SCI'][student_pred['READ_FREQ_NON_SCI'] == 1] <- 'None'
student_pred['READ_FREQ_NON_SCI'][student_pred['READ_FREQ_NON_SCI'] == 2] <- 'Sometimes'
student_pred['READ_FREQ_NON_SCI'][student_pred['READ_FREQ_NON_SCI'] == 3] <- 'Often'

# READ_FREQ_SCI
student_pred['READ_FREQ_SCI'][student_pred['READ_FREQ_SCI'] == 1] <- 'None'
student_pred['READ_FREQ_SCI'][student_pred['READ_FREQ_SCI'] == 2] <- 'Sometimes'
student_pred['READ_FREQ_SCI'][student_pred['READ_FREQ_SCI'] == 3] <- 'Often'

# ATTENDANCE_DEPT
student_pred['ATTENDANCE_DEPT'][student_pred['ATTENDANCE_DEPT'] == 1] <- 'Yes'
student_pred['ATTENDANCE_DEPT'][student_pred['ATTENDANCE_DEPT'] == 2] <- 'No'

# PROJECT_IMPACT
student_pred['PROJECT_IMPACT'][student_pred['PROJECT_IMPACT'] == 1] <- 'Positive'
student_pred['PROJECT_IMPACT'][student_pred['PROJECT_IMPACT'] == 2] <- 'Negative'
student_pred['PROJECT_IMPACT'][student_pred['PROJECT_IMPACT'] == 3] <- 'Neutral'

# ATTENDANCE_CLASS
student_pred['ATTENDANCE_CLASS'][student_pred['ATTENDANCE_CLASS'] == 1] <- 'Always'
student_pred['ATTENDANCE_CLASS'][student_pred['ATTENDANCE_CLASS'] == 2] <- 'Sometimes'
student_pred['ATTENDANCE_CLASS'][student_pred['ATTENDANCE_CLASS'] == 3] <- 'Never'

# PREP_STUDY_WITH
student_pred['PREP_STUDY_WITH'][student_pred['PREP_STUDY_WITH'] == 1] <- 'Alone'
student_pred['PREP_STUDY_WITH'][student_pred['PREP_STUDY_WITH'] == 2] <- 'With friends'
student_pred['PREP_STUDY_WITH'][student_pred['PREP_STUDY_WITH'] == 3] <- 'Not applicable'

# PREP_STUDY_WHEN
student_pred['PREP_STUDY_WHEN'][student_pred['PREP_STUDY_WHEN'] == 1] <- 'Closest date to the exam'
student_pred['PREP_STUDY_WHEN'][student_pred['PREP_STUDY_WHEN'] == 2] <- 'Regularly during the semester'
student_pred['PREP_STUDY_WHEN'][student_pred['PREP_STUDY_WHEN'] == 3] <- 'Never'

# TAKE_NOTES
student_pred['NOTES'][student_pred['NOTES'] == 1] <- 'Never'
student_pred['NOTES'][student_pred['NOTES'] == 2] <- 'Sometimes'
student_pred['NOTES'][student_pred['NOTES'] == 3] <- 'Always'

# ATTENTION
student_pred['LISTEN'][student_pred['LISTEN'] == 1] <- 'Never'
student_pred['LISTEN'][student_pred['LISTEN'] == 2] <- 'Sometimes'
student_pred['LISTEN'][student_pred['LISTEN'] == 3] <- 'Always'

# DISCUSSION
student_pred['DISCUSSION'][student_pred['DISCUSSION'] == 1] <- 'Never'
student_pred['DISCUSSION'][student_pred['DISCUSSION'] == 2] <- 'Sometimes'
student_pred['DISCUSSION'][student_pred['DISCUSSION'] == 3] <- 'Always'

# CLASSROOM
student_pred['FLIP-CLASSROOM'][student_pred['FLIP-CLASSROOM'] == 1] <- 'Never'
student_pred['FLIP-CLASSROOM'][student_pred['FLIP-CLASSROOM'] == 2] <- 'Sometimes'
student_pred['FLIP-CLASSROOM'][student_pred['FLIP-CLASSROOM'] == 3] <- 'Always'

# CGPA(/4.00)
student_pred['CUML_GPA'][student_pred['CUML_GPA'] == 1] <- '< 2.00'
student_pred['CUML_GPA'][student_pred['CUML_GPA'] == 2] <- '2.00 - 2.49'
student_pred['CUML_GPA'][student_pred['CUML_GPA'] == 3] <- '2.50 - 2.99'
student_pred['CUML_GPA'][student_pred['CUML_GPA'] == 4] <- '3.00 - 3.49'
student_pred['CUML_GPA'][student_pred['CUML_GPA'] == 5] <- 'Above 3.49'

# EXPECTED_CGPA(/4.00)
student_pred['EXPECTED_GPA'][student_pred['EXPECTED_GPA'] == 1] <- '< 2.00'
student_pred['EXPECTED_GPA'][student_pred['EXPECTED_GPA'] == 2] <- '2.00 - 2.49'
student_pred['EXPECTED_GPA'][student_pred['EXPECTED_GPA'] == 3] <- '2.50 - 2.99'
student_pred['EXPECTED_GPA'][student_pred['EXPECTED_GPA'] == 4] <- '3.00 - 3.49'
student_pred['EXPECTED_GPA'][student_pred['EXPECTED_GPA'] == 5] <- 'Above 3.49'

# GRADE
student_pred['GRADE'][student_pred['GRADE'] == 0] <- 'Fail'
student_pred['GRADE'][student_pred['GRADE'] == 1] <- 'DD'
student_pred['GRADE'][student_pred['GRADE'] == 2] <- 'DC'
student_pred['GRADE'][student_pred['GRADE'] == 3] <- 'CC'
student_pred['GRADE'][student_pred['GRADE'] == 4] <- 'CB'
student_pred['GRADE'][student_pred['GRADE'] == 5] <- 'BB'
student_pred['GRADE'][student_pred['GRADE'] == 6] <- 'BA'
student_pred['GRADE'][student_pred['GRADE'] == 7] <- 'AA'

# VIEW HIGH GRADE STUDENTS
HIGH_GRADE = student_pred[student_pred$GRADE == 'AA' | student_pred$GRADE == 'BA' | student_pred$GRADE == 'BB' | student_pred$GRADE == 'CB', ]
View(HIGH_GRADE)
nrow(HIGH_GRADE) # 463

# PRINT HIGH GRADE STUDENTS
subset(student_pred, GRADE == 'AA' | GRADE == 'BA' | GRADE == 'BB' | GRADE == 'CB', select = c(STUDY_HOURS, AGE, TAKE_NOTES, HIGHSCHOOL))

# VIEW LOW GRADE STUDENTS
LOW_GRADE = student_pred[student_pred$GRADE == 'CC' | student_pred$GRADE == 'DC' | student_pred$GRADE == 'DD' | student_pred$GRADE == 'Fail', ]
View(LOW_GRADE)
nrow(LOW_GRADE) # 1071

# PRINT LOW GRADE STUDENTS
subset(student_pred, GRADE == 'CC' | GRADE == 'DC' | GRADE == 'DD' | GRADE == 'Fail', select = c(WEEKLY_STUDY_HRS, AGE, NOTES, GRAD_HIGH_SCHOOL_TYPE))

# VIEW STUDY HOURS OF HIGH GRADE STUDENTS
STUDY_HRS = HIGH_GRADE[(HIGH_GRADE$WEEKLY_STUDY_HRS == 'None'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 102

STUDY_HRS = HIGH_GRADE[(HIGH_GRADE$WEEKLY_STUDY_HRS == '< 5 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 154

STUDY_HRS = HIGH_GRADE[(HIGH_GRADE$WEEKLY_STUDY_HRS == '6 - 10 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 97

STUDY_HRS = HIGH_GRADE[(HIGH_GRADE$WEEKLY_STUDY_HRS == '11 - 20 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 109

STUDY_HRS = HIGH_GRADE[(HIGH_GRADE$WEEKLY_STUDY_HRS == '> 20 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 1

# VIEW STUDY HOURS OF LOW GRADE STUDENTS
STUDY_HRS = LOW_GRADE[(LOW_GRADE$WEEKLY_STUDY_HRS == 'None'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 246

STUDY_HRS = LOW_GRADE[(LOW_GRADE$WEEKLY_STUDY_HRS == '< 5 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 320

STUDY_HRS = LOW_GRADE[(LOW_GRADE$WEEKLY_STUDY_HRS == '6 - 10 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 238

STUDY_HRS = LOW_GRADE[(LOW_GRADE$WEEKLY_STUDY_HRS == '11 - 20 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 264

STUDY_HRS = LOW_GRADE[(LOW_GRADE$WEEKLY_STUDY_HRS == '> 20 hours'), ]
View(STUDY_HRS)
nrow(STUDY_HRS) # 3

# STUDY HOURS < 5 HOURS TABLE
SH5 = student_pred[(student_pred$WEEKLY_STUDY_HRS == '< 5 hours'), ]
View(SH5)
nrow(SH5) # 474

# STUDY HOURS < 5 HOURS + TAKE NOTES HG
SH5_TN = SH5[(SH5$NOTES == 'Always' | SH5$NOTES == 'Sometimes') & (SH5$GRADE == 'AA' | SH5$GRADE == 'BA' | SH5$GRADE == 'BB' | SH5$GRADE == 'CB'), ]
View(SH5_TN)
nrow(SH5_TN) # 152

SH5_TN = SH5[(SH5$NOTES == 'Never') & (SH5$GRADE == 'AA' | SH5$GRADE == 'BA' | SH5$GRADE == 'BB' | SH5$GRADE == 'CB'), ]
View(SH5_TN)
nrow(SH5_TN) # 2

# STUDY_HRS < 5 HOURS + TAKE_NOTES LG
SH5_TN = SH5[(SH5$NOTES == 'Always' | SH5$NOTES == 'Sometimes') & (SH5$GRADE == 'CC' | SH5$GRADE == 'DC' | SH5$GRADE == 'DD' | SH5$GRADE == 'Fail'), ]
View(SH5_TN)
nrow(SH5_TN) # 308

SH5_TN = SH5[(SH5$NOTES == 'Never') & (SH5$GRADE == 'CC' | SH5$GRADE == 'DC' | SH5$GRADE == 'DD' | SH5$GRADE == 'Fail'), ]
View(SH5_TN)
nrow(SH5_TN) # 12

# STUDY HOURS < 5 HOURS + TAKE_NOTES ALWAYS & SOMETIMES TABLE
SH5_HTN = student_pred[(student_pred$WEEKLY_STUDY_HRS == '< 5 hours') & (student_pred$NOTES == 'Always' | student_pred$NOTES == 'Sometimes'), ]
View(SH5_HTN)
nrow(SH5_HTN) # 460

# STUDY HOURS < 5 HOURS + TAKE NOTES + AGE HG
SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == '18 - 21') & (SH5_HTN$GRADE == 'AA' | SH5_HTN$GRADE == 'BA' | SH5_HTN$GRADE == 'BB' | SH5_HTN$GRADE == 'CB'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 65

SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == '22 - 25') & (SH5_HTN$GRADE == 'AA' | SH5_HTN$GRADE == 'BA' | SH5_HTN$GRADE == 'BB' | SH5_HTN$GRADE == 'CB'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 70

SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == 'Above 26') & (SH5_HTN$GRADE == 'AA' | SH5_HTN$GRADE == 'BA' | SH5_HTN$GRADE == 'BB' | SH5_HTN$GRADE == 'CB'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 70

# STUDY HOURS < 5 HOURS + TAKE NOTES + AGE LG
SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == '18 - 21') & (SH5_HTN$GRADE == 'CC' | SH5_HTN$GRADE == 'DC' | SH5_HTN$GRADE == 'DD' | SH5_HTN$GRADE == 'Fail'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 65

SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == '22 - 25') & (SH5_HTN$GRADE == 'CC' | SH5_HTN$GRADE == 'DC' | SH5_HTN$GRADE == 'DD' | SH5_HTN$GRADE == 'Fail'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 126


SH5_HTN_AGE = SH5_HTN[(SH5_HTN$AGE == 'Above 26') & (SH5_HTN$GRADE == 'CC' | SH5_HTN$GRADE == 'DC' | SH5_HTN$GRADE == 'DD' | SH5_HTN$GRADE == 'Fail'), ]
View(SH5_HTN_AGE)
nrow(SH5_HTN_AGE) # 85

# STUDY HOURS < 5 HOURS + TAKE_NOTES ALWAYS & SOMETIMES + AGE 22 - 25 TABLE
SH5_HTN_AGE22 = student_pred[(student_pred$WEEKLY_STUDY_HRS == '< 5 hours') & (student_pred$NOTES == 'Always' | student_pred$NOTES == 'Sometimes') & (student_pred$AGE == '22 - 25'), ]
View(SH5_HTN_AGE22)
nrow(SH5_HTN_AGE22) # 196

# STUDY_HRS < 5 HOURS + TAKE_NOTES ALWAYS & SOMETIMES + AGE 22 - 25 + HIGHSCHOOL HG
SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'Private') & (SH5_HTN_AGE22$GRADE == 'AA' | SH5_HTN_AGE22$GRADE == 'BA' | SH5_HTN_AGE22$GRADE == 'BB' | SH5_HTN_AGE22$GRADE == 'CB'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 14

SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'State') & (SH5_HTN_AGE22$GRADE == 'AA' | SH5_HTN_AGE22$GRADE == 'BA' | SH5_HTN_AGE22$GRADE == 'BB' | SH5_HTN_AGE22$GRADE == 'CB'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 33

SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'Other') & (SH5_HTN_AGE22$GRADE == 'AA' | SH5_HTN_AGE22$GRADE == 'BA' | SH5_HTN_AGE22$GRADE == 'BB' | SH5_HTN_AGE22$GRADE == 'CB'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 23

# STUDY_HRS < 5 HOURS + TAKE_NOTES ALWAYS & SOMETIMES + AGE 22 - 25 + HIGHSCHOOL LG
SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'Private') & (SH5_HTN_AGE22$GRADE == 'CC' | SH5_HTN_AGE22$GRADE == 'DC' | SH5_HTN_AGE22$GRADE == 'DD' | SH5_HTN_AGE22$GRADE == 'Fail'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 31

SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'State') & (SH5_HTN_AGE22$GRADE == 'CC' | SH5_HTN_AGE22$GRADE == 'DC' | SH5_HTN_AGE22$GRADE == 'DD' | SH5_HTN_AGE22$GRADE == 'Fail'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 48

SH5_HTN_AGE22_HS = SH5_HTN_AGE22[(SH5_HTN_AGE22$GRAD_HIGH_SCHOOL_TYPE == 'Other') & (SH5_HTN_AGE22$GRADE == 'CC' | SH5_HTN_AGE22$GRADE == 'DC' | SH5_HTN_AGE22$GRADE == 'DD' | SH5_HTN_AGE22$GRADE == 'Fail'), ]
View(SH5_HTN_AGE22_HS)
nrow(SH5_HTN_AGE22_HS) # 47

########################################## DATA VISUALIZATION ##########################################################

# 3D pie chart
# install for 3D pie chart
install.packages("plotrix")
library(plotrix)

# install for color palettes
install.packages("RColorBrewer")
library("RColorBrewer")

# create a pie chart in order to analyse the STUDY HOURS count based on the data set
a = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "None", ])
b = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "< 5 hours", ])
c = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours", ])
d = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours", ])
e = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "> 20 hours", ])
z = c(a, b, c, d, e) # create vector z by combining the variables a, b, c, d and e
n = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours") # for label

pie3D(z, radius = 0.75, height = 0.1, # size of the chart
      col = brewer.pal(n = 5, name = "RdBu"), border = "white", shade = 1, # color of the chart
      labels = paste0(n, "\n", z, ", ", round(z/sum(z) * 100, 2), "%")) # label of the chart

# Add a title above the pie chart
title(main = "Study Hours Distribution", line = 0.01, col.main = "blue", font.main = 4)

# donut chart
install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

# filter the student who getting high grade
HIGH_GRADE = student_pred[student_pred$GRADE == 'AA' | student_pred$GRADE == 'BA' | 
                                  student_pred$GRADE == 'BB' | student_pred$GRADE == 'CB', ]

v = nrow(HIGH_GRADE[HIGH_GRADE$WEEKLY_STUDY_HRS == "None", ])
w = nrow(HIGH_GRADE[HIGH_GRADE$WEEKLY_STUDY_HRS == "< 5 hours", ])
x = nrow(HIGH_GRADE[HIGH_GRADE$WEEKLY_STUDY_HRS == "6 - 10 hours", ])
y = nrow(HIGH_GRADE[HIGH_GRADE$WEEKLY_STUDY_HRS == "11 - 20 hours", ])
z = nrow(HIGH_GRADE[HIGH_GRADE$WEEKLY_STUDY_HRS == "> 20 hours", ])

df <- data.frame(value = c(v, w, x, y, z),
                 group = paste0(c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours")))

# hole size of the donut chart
hsize <- 4

df <- df %>% 
  mutate(x = hsize)

ggplot(df, aes(x = hsize, y = value, fill = group)) +
  geom_col(color = "black") + # border color
  scale_fill_manual(values = c("#FFF7FB", "#000099", "#74A9CF", "#0570B0", "#D0D1E6")) + # color fill
  geom_text(aes(label = paste0(value, "\n", round(value/sum(value) * 100, 2), "%")),
            position = position_stack(vjust = 0.5)) + # adding labels
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  guides(fill = guide_legend(title = "STUDY HOURS")) # legend title

# Bar chart
install.packages("ggplot2")
library(ggplot2)

# install for getting 2 decimal places percentage
install.packages("scales")
library(scales)

# filter the student who getting low grade
LOW_GRADE = student_pred[student_pred$GRADE == 'CC' | student_pred$GRADE == 'DC' | 
                                 student_pred$GRADE == 'DD' | student_pred$GRADE == 'Fail', ]
View(student_pred)
# Count the number of students in each category
a <- table(LOW_GRADE$WEEKLY_STUDY_HRS)

# Create a data frame for plotting
df <- data.frame(WEEKLY_STUDY_HRS = names(a), count = as.numeric(a))

# Plot the bar chart
ggplot(df, aes(x = WEEKLY_STUDY_HRS, y = count, fill = WEEKLY_STUDY_HRS)) +
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  scale_fill_manual(values = c('None' = '#FFCC99', '< 5 hours' = '#FF9966', 
                               '6 - 10 hours' = '#FF6633', '11 - 20 hours' = '#FF3300',
                               '> 20 hours' = '#CC0000')) +
  geom_text(aes(label = paste0(count, "\n", round(count/sum(count) * 100, 2), "%")),
            color = "black",
            position = position_stack(vjust = 1.026)) +
  ylab("Number of students") +
  ggtitle("Study Hours Distribution for Low Grades Students")

# Bar chart
# Find the distribution of weekly study hours among the habits of taking notes in class.
library(ggplot2)
library(scales)

a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                              (student_pred$NOTES == "Always"), ])
b = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                              (student_pred$NOTES == "Always"), ])
c = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                              (student_pred$NOTES == "Always"), ])
d = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                              (student_pred$NOTES == "Always" ), ])
e = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                              (student_pred$NOTES == "Always"), ])

f = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                        (student_pred$NOTES == "Sometimes"), ])
g = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                        (student_pred$NOTES == "Sometimes"), ])
h = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                        (student_pred$NOTES == "Sometimes"), ])
i = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                        (student_pred$NOTES == "Sometimes"), ])
j = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                        (student_pred$NOTES == "Sometimes"), ])

k = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                        (student_pred$NOTES == "Never"), ])
l = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                        (student_pred$NOTES == "Never"), ])
m = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                        (student_pred$NOTES == "Never"), ])
n = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                        (student_pred$NOTES == "Never"), ])
o = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                        (student_pred$NOTES == "Never"), ])
z = c(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

df <- data.frame(value = z,
                 group = rep(c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours"), 3),
                 NOTES = rep(c("Always", "Sometimes", "Never"), each = 5))

# Create the bar plot
ggplot(data = df, aes(x = group, y = value, fill = NOTES)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Study Hours and Habits of Taking Notes Relationship",
       x = "Weekly Study Hours",
       y = "Number of Students") +
  geom_text(aes(label = paste(value, " (", percent(value/sum(value), accuracy = 0.01), ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +  # Adjust the position and size of labels
  scale_fill_manual(values = c("Always" = "#CCFFFF", "Sometimes" = "#99CCFF", "Never" = "#3399FF"))


# chord diagram
install.packages("circlize")
library(circlize)

# VIEW HIGH GRADE STUDENTS
HIGH_GRADE = student_pred[student_pred$GRADE == 'AA' | student_pred$GRADE == 'BA' | 
                            student_pred$GRADE == 'BB' | student_pred$GRADE == 'CB', ]

# Create a contingency table
high_grade <- table(HIGH_GRADE$WEEKLY_STUDY_HRS, HIGH_GRADE$NOTES)

# Display the table to check its structure
print(high_grade)

chordDiagram(
  high_grade,
  grid.col = c('Always' = "#FFFF33", 'Never' = "#FFCC33", 'Sometimes' = "#FF9966",
               'None' = "#CCFFCC", '< 5 hours' = "#006600", '6 - 10 hours' = "#99FF33", 
               '11 - 20 hours' = "#66CC00", '> 20 hours' = "#CCFF99"), )

# chord diagram
install.packages("circlize")
library(circlize)

mb <- matrix("#003300", nrow = 1, ncol = ncol(high_grade))
rownames(mb) <- rownames(high_grade)[1] # First row
colnames(mb) <- colnames(high_grade)

chordDiagram(
  high_grade,
  grid.col = c('Always' = "#FFFF33", 'Never' = "#FFCC33", 'Sometimes' = "#FF9966",
               'None' = "#CCFFCC", '< 5 hours' = "#006600", '6 - 10 hours' = "#99FF33", 
               '11 - 20 hours' = "#66CC00", '> 20 hours' = "#CCFF99"),
  link.lwd = 2, # line width
  link.lty = 2, # line type
  link.border =  mb) # border color

# chord diagram
install.packages("circlize")
library(circlize)

chordDiagram(
  high_grade,
  grid.col = c('Always' = "#FFFF33", 'Never' = "#FFCC33", 'Sometimes' = "#FF9966",
               'None' = "#CCFFCC", '< 5 hours' = "#006600", '6 - 10 hours' = "#99FF33", 
               '11 - 20 hours' = "#66CC00", '> 20 hours' = "#CCFF99"),
  row.col = c("#FF000080", "#00FF0010", "#00FF0030", "#0000FF10", "#0000FF20"))

# Mosaic Plot
# Install and load necessary packages
install.packages("vcd")
library(vcd)

# Filter the students that get low grade
LOW_GRADE = student_pred[student_pred$GRADE == 'CC' | student_pred$GRADE == 'DC' | 
                           student_pred$GRADE == 'DD' | student_pred$GRADE == 'Fail', ]

mosaic( ~ NOTES + WEEKLY_STUDY_HRS, data = LOW_GRADE,
      # fill in the color based on take notes
      highlighting = "NOTES",
      highlighting_fill = c("#CCFFFF", "#99CCFF", "#3399FF"),
      # change the direction of graph (take notes is in vertical, study hours is in horizontal)
      direction = c("v","h"),
      main = "Relationship between study hours and habits of taking notes",
      # to label the number of students
      labeling = labeling_values())
  
# 3D pie chart
# install for 3D pie chart
install.packages("plotrix")
library(plotrix)

# install for color palettes
install.packages("RColorBrewer")
library("RColorBrewer")

# create a pie chart in order to analyse the STUDY HOURS count based on the data set
a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") &
                              (student_pred$AGE == "18 - 21"), ])
b = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "< 5 hours" &
                              (student_pred$AGE == "18 - 21"), ])
c = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours" &
                              (student_pred$AGE == "18 - 21"), ])
d = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours" &
                              (student_pred$AGE == "18 - 21"), ])
e = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "> 20 hours" &
                              (student_pred$AGE == "18 - 21"), ])
z = c(a, b, c, d, e) # create vector z by combining the variables a, b, c, d and e
n = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours") # for label

pie3D(z, radius = 0.75, height = 0.1,
      col = brewer.pal(n = 5, name = "RdBu"),
      border = "white", shade = 1,
      labels = paste0(n, "\n", z, ", ", round(z/sum(z) * 100, 2), "%"),
      main = "Study Hours Distribution for Age between 18 - 21",  # Add a title above the pie chart
      col.main = "blue", font.main = 4)

# 3D pie chart
# install for 3D pie chart
install.packages("plotrix")
library(plotrix)

# install for color palettes
install.packages("RColorBrewer")
library("RColorBrewer")

# create a pie chart in order to analyse the STUDY HOURS count based on the data set
a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") &
                        (student_pred$AGE == "22 - 25"), ])
b = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "< 5 hours" &
                        (student_pred$AGE == "22 - 25"), ])
c = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours" &
                        (student_pred$AGE == "22 - 25"), ])
d = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours" &
                        (student_pred$AGE == "22 - 25"), ])
e = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "> 20 hours" &
                        (student_pred$AGE == "22 - 25"), ])
z = c(a, b, c, d, e) # create vector z by combining the variables a, b, c, d and e
n = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours") # for label

pie3D(z, radius = 0.75, height = 0.1,
      col = brewer.pal(n = 5, name = "RdBu"),
      border = "white", shade = 1,
      labels = paste0(n, "\n", z, ", ", round(z/sum(z) * 100, 2), "%"),
      main = "Study Hours Distribution for Age between 22 - 25",  # Add a title above the pie chart
      col.main = "blue", font.main = 4)

# Tree map
# Installing required packages 
install.packages("ggplot2") 
install.packages("treemapify")
install.packages("RColorBrewer")

# Importing required library 
library(ggplot2) 
library(treemapify)
library("RColorBrewer")

a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") &
                        (student_pred$AGE == "Above 26"), ])
b = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "< 5 hours" &
                        (student_pred$AGE == "Above 26"), ])
c = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours" &
                        (student_pred$AGE == "Above 26"), ])
d = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours" &
                        (student_pred$AGE == "Above 26"), ])
e = nrow(student_pred[student_pred$WEEKLY_STUDY_HRS == "> 20 hours" &
                        (student_pred$AGE == "Above 26"), ])
z = c(a, b, c, d, e) # create vector z by combining the variables a, b, c, d and e

# Creating Data frame 
df<- data.frame(WEEKLY_STUDY_HRS = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours"), 
                num = z)

# Plotting the Tree Map
ggplot2::ggplot(df, aes(area = num, fill = WEEKLY_STUDY_HRS, label = WEEKLY_STUDY_HRS)) + 
  treemapify::geom_treemap(layout = "squarified") + 
  treemapify::geom_treemap(aes(area = num, fill = WEEKLY_STUDY_HRS), color = "black", size = 0.5) +
  labs(title = "Study Hours Dirstribution for Students Above 26 Years Old") +
  scale_fill_brewer(palette = "Pastel1") +  # set the color scale
  geom_treemap_text(aes(label = paste0(WEEKLY_STUDY_HRS, "\n", z, "\n", round(z/sum(z) * 100, 2), "%")),
                    place = "centre", size = 12, color = "black") # adding labels

# Mosaic Plot
# Install and load necessary packages
install.packages("vcd")
library(vcd)

# Filter the students based on the grade
student_pred['GRADE'][student_pred['GRADE'] == 'Fail'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DD'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BA'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'AA'] <- 'HIGH GRADE'

mosaic( ~ AGE + WEEKLY_STUDY_HRS + GRADE, data = student_pred,
        # fill in the color based on take notes
        highlighting = "AGE",
        highlighting_fill = c("#CCFFFF", "#99CCFF", "#3399FF"),
        # change the direction of graph (age is in vertical, study hours is in horizontal)
        direction = c("v","h", "v"),
        main = "Relationship between age of the students and study hours",
        # to label the number of students
        labeling = labeling_values())

# Lollipop plot
# Load ggplot2  
library(ggplot2)

a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Private"), ])
b = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Private"), ])
c = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Private"), ])
d = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Private"), ])
e = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Private"), ])

z = c(a, b, c, d, e)

df <- data.frame(name = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours"), 
                 value = z)  

# Create lollipop plot 
ggplot(df, aes(x = name, y = value)) + 
  geom_segment(aes(x = name, xend = name, y = 0, yend = value), color = "#FF9933", size = 1) + 
  geom_point(size = 4, color = "#FFCC99") +
  labs(title = "Study hours dirstribution among the students that graduate from private high school") +
  geom_label(
    aes(x = name, y = value, label = paste0(value, "\n", round(value/sum(value) * 100, 2), "%")),  
    colour = "darkred",
    nudge_x = 0.35,
    size = 4)

# Lollipop plot
# Load ggplot2  
library(ggplot2)

a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "State"), ])
b = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "State"), ])
c = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "State"), ])
d = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "State"), ])
e = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "State"), ])

z = c(a, b, c, d, e)

df <- data.frame(name = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours"), 
                 value = z)  

# Create lollipop plot 
ggplot(df, aes(x = name, y = value)) + 
  geom_segment(aes(x = name, xend = name, y = 0, yend = value), color = "#FF9933", size = 1) + 
  geom_point(size = 4, color = "#FFCC99") +
  labs(title = "Study hours dirstribution among the students that graduate from state high school") +
  geom_label(
    aes(x = name, y = value, label = paste0(value, "\n", round(value/sum(value) * 100, 2), "%")),  
    colour = "darkred",
    nudge_x = 0.35,
    size = 4)

# Lollipop plot
# Load ggplot2  
library(ggplot2)

a = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "None") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Other"), ])
b = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "< 5 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Other"), ])
c = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "6 - 10 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Other"), ])
d = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "11 - 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Other"), ])
e = nrow(student_pred[(student_pred$WEEKLY_STUDY_HRS == "> 20 hours") & 
                        (student_pred$GRAD_HIGH_SCHOOL_TYPE == "Other"), ])

z = c(a, b, c, d, e)

df <- data.frame(name = c("None", "< 5 hours", "6 - 10 hours", "11 - 20 hours", "> 20 hours"), 
                 value = z)  

# Create lollipop plot 
ggplot(df, aes(x = name, y = value)) + 
  geom_segment(aes(x = name, xend = name, y = 0, yend = value), color = "#FF9933", size = 1) + 
  geom_point(size = 4, color = "#FFCC99") +
  labs(title = "Study hours dirstribution among the students that graduate from other high school") +
  geom_label(
    aes(x = name, y = value, label = paste0(value, "\n", round(value/sum(value) * 100, 2), "%")),  
    colour = "darkred",
    nudge_x = 0.35,
    size = 4)

# Mosaic Plot
# Install and load necessary packages
install.packages("vcd")
library(vcd)

# Filter the students based on the grade
student_pred['GRADE'][student_pred['GRADE'] == 'Fail'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DD'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BA'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'AA'] <- 'HIGH GRADE'

mosaic( ~ GRAD_HIGH_SCHOOL_TYPE + WEEKLY_STUDY_HRS + GRADE, data = student_pred,
        # fill in the color based on take notes
        highlighting = "GRAD_HIGH_SCHOOL_TYPE",
        highlighting_fill = c("#CCFFFF", "#99CCFF", "#3399FF"),
        # change the direction of graph
        # high school is in vertical, study hours is in horizontal, grade is in vertical
        direction = c("v", "h", "v"),
        main = "Relationship between types of graduate high school of the students and study hours",
        # to label the number of students
        labeling = labeling_values())

# Tree Map
student_pred['GRADE'][student_pred['GRADE'] == 'Fail'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DD'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BA'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'AA'] <- 'HIGH GRADE'

# Filter the data
SH5_HTN <- student_pred[(student_pred$WEEKLY_STUDY_HRS == '< 5 hours') & 
                                (student_pred$NOTES %in% c('Always', 'Sometimes')), ]

# Count the number of observations for each combination of AGE and GRADE
count_data <- table(SH5_HTN$AGE, SH5_HTN$GRADE)

# Convert the count_data to a data frame
df <- as.data.frame.table(count_data)

# Rename the columns
colnames(df) <- c("AGE", "GRADE", "num")

# Plotting the Tree Map
library(ggplot2)
library(treemapify)

ggplot(df, aes(area = num, fill = GRADE, label = AGE)) + 
  treemapify::geom_treemap(layout = "squarified") + 
  treemapify::geom_treemap(aes(area = num, fill = GRADE), color = "black", size = 0.5) +
  labs(title = "Age of Students with < 5 Hours of Study Hours and Take Notes in classes") +
  scale_fill_brewer(palette = "Pastel1") +  # set the color scale
  geom_treemap_text(aes(label = paste0(AGE, "\n", num, "\n", round(num/sum(num) * 100, 2), "%")),
                    place = "centre", size = 12, color = "black") # adding labels

# Mosaic Plot
# Install and load necessary packages
install.packages("vcd")
library(vcd)

student_pred['GRADE'][student_pred['GRADE'] == 'Fail'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DD'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'DC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CC'] <- 'LOW GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'CB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BB'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'BA'] <- 'HIGH GRADE'
student_pred['GRADE'][student_pred['GRADE'] == 'AA'] <- 'HIGH GRADE'

# STUDY HOURS < 5 HOURS + TAKE_NOTES ALWAYS + AGE 22 - 25 TABLE
SH5_HTN_AGE22 = student_pred[(student_pred$WEEKLY_STUDY_HRS == '< 5 hours') & 
                                     (student_pred$NOTES == 'Always' | student_pred$NOTES == 'Sometimes') & 
                                     (student_pred$AGE == '22 - 25'), ]
View(SH5_HTN_AGE22)
nrow(SH5_HTN_AGE22) # 196

mosaic( ~ GRAD_HIGH_SCHOOL_TYPE + AGE + WEEKLY_STUDY_HRS + GRADE, data = SH5_HTN_AGE22,
        # fill in the color based on take notes
        highlighting = "AGE",
        highlighting_fill = c("#CCFFFF", "#99CCFF", "#3399FF"),
        # change the direction of graph (age is in vertical, study hours is in horizontal)
        direction = c("h", "v","h", "v"),
        main = "High School of Students",
        # to label the number of students
        labeling = labeling_values())

# Chi-square Test
test1 = table(student_pred$WEEKLY_STUDY_HRS, student_pred$NOTES)

# applying chisq.test() function 
print(chisq.test(test1))

# Chi-square Test
test2 = table(student_pred$WEEKLY_STUDY_HRS, student_pred$AGE)

# applying chisq.test() function 
print(chisq.test(test2))

# Chi-square Test
test3 = table(student_pred$WEEKLY_STUDY_HRS, student_pred$GRAD_HIGH_SCHOOL_TYPE)

# applying chisq.test() function 
print(chisq.test(test3))

# Chi-square Test
test4 = table(student_pred$WEEKLY_STUDY_HRS, student_pred$GRADE)

# applying chisq.test() function 
print(chisq.test(test4))


