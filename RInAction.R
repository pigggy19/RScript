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