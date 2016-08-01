# Important git commands
# git remote add origin https://github.com/ewenharrison/test.git
# git config remote.origin.url git@github.com:ewenharrison/test.git
# git pull -u origin master
# git push -u origin master






# To play with R in action: 
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership<-data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5, stringsAsFactors = F)
leadership
?within
leadership$age[leadership$age==99]<-NA
leadership<-within(leadership,{
    agecat<-NA
    agecat[age>75] <- "Elder"
    agecat[age<=75 &age>=55] <-"Middle Age"
    agecat[age<55] <- "Young" 
})
?car
?fix
fix(leadership)
leadership<-within(leadership,q4[manager==5]<-2)

rm(x)
newDAta<-na.omit(leadership)
?difftime
?format

today<-Sys.Date()
format(dob,format="%d")
dob<-as.Date("1956-10-12")
with(leadership,leadership[order(gender),])
!names(leadership)%in%c("manager")

leadership$date<-as.Date(leadership$date,"%m/%d/%y")

startDate<- as.Date("2009-01-01")
endDate<- as.Date("2009-12-31")

newdata<-with(leadership,leadership[date>=startDate & date<=endDate,])
?subset

newdata<-subset(leadership,age>=35|age<24,select=c(q1:q5))
newdata<-subset(leadership,gender=="M"&age>25,select=gender:q4)

library(sqldf)
# install.packages("sqldf")
?sqldf
newdata<- sqldf("select * from mtcars order by mpg",row.names=T)


# Advanced data management


?option
# Challenge
# You want to combine these scores in order to determine a single performance
# indicator for each student. Additionally, you want to assign an A to the top
# 20% of students, a B to the next 20%, and so on. Finally, you want to sort the
# students alphabetically.




options(digits=2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster<-data.frame(Student,Math,Science,English,stringsAsFactors = F)


# Scale the datadata and get the performance metric
z<-scale(roster[,2:4])
score<-apply(z,1,mean)
roster<-mutate(roster,score=score)
y<-quantile(score,probs=c(0.8,0.6,0.4,0.2))
roster<-within(roster,{
    grade<-NA
    grade[score>=y[1]]<-'A'
    grade[score>=y[2] &score<y[1]]<-'B'
    grade[score>=y[3] & score<y[2]]<-'C'
    grade[score>=y[4] & score<y[3]]<-'D'
    grade[score<y[4]]<-'F'
})

names<- strsplit(roster$Student," ")
FirstName<-sapply(names,"[", 1)
LastName<-sapply(names,"[", 2)
# roster<-mutate(roster,FirstName)
# roster<-mutate(roster,LastName)
roster<-cbind(FirstName,LastName,roster[,-1])
roster<-roster[,-c(8,9)]
roster<-arrange(roster,FirstName)

?sapply
# awkward to use

# roster<-within(roster,{
#     grade<-NA
#     if(score>=y[1]) {
#         grade<-'A'
#     } else if(score>=y[2] & score<y[1]){
#         grade<-'B'
#     }else if(score>=y[3] & score<y[2]){
#         grade<-'C'
#     } else if (score>=y[4] & score<y[3]) {
#         grade <-'D'
#     } else {
#         grade<-'F'
#     }
# })








