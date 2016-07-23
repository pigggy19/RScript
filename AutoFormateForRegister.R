# read excel table from file
#install.packages("xlsx")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
library(xlsx)
library(dplyr)
library(tidyr)
library(stringr)
?read.xlsx2
originalTab2<-read.xlsx2("/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/reg_raw_input.xlsx", 1)

originalTab2<- tbl_df(originalTab2)
str(originalTab2)
#change column name
names(originalTab2)<-c("Module_Title","Department","First_Name","Last_Name","Staff_ID"
    ,"Faculty")
View(originalTab2)
#separate full name

#newTab<-originalTab %>% separate(Person_Full_Name, c("Last_Name", "First_Name"), sep = "\\,")
#trim the string
#newTab$Last_Name<-str_trim(newTab$Last_Name, side = "both")
#newTab$First_Name<-str_trim(newTab$First_Name, side = "both")

# group by faculty and department
newTab2 <- originalTab2 %>% arrange(Faculty,Department,First_Name);
#newTab$Faculty<-str_trim(newTab$Faculty, side = "both")
#newTab$Department<-str_trim(newTab$Department, side = "both")

# convert the factor of staff_id into numeric
#newTab<-transform(newTab,Staff_ID = as.numeric(Staff_ID))

Output2<-newTab2 %>% group_by(Faculty,Department,Staff_ID,First_Name,Last_Name) %>% select(Faculty,Department,Staff_ID,First_Name,Last_Name, Module_Title);


#add a column to indicate count of occurrences for staff_ID
temp2<-Output2%>%mutate(countOfModule=paste("Module",sequence(n()))) #####n() counts number of rows according to group_by 

#spread training module column into three modules rows
Output2<-temp2%>%spread(countOfModule,Module_Title)
Output2<-mutate(Output2,Indicator="")
Output2<-Output2%>%select(-c(9:13))
View(Output2)

test1<-as.data.frame(Output2[duplicated(Output2$Staff_ID),])
test1[,"Staff_ID"]
test1
duplicated(Output2$Staff_ID)





#write to file
#divide it up according to faculty
outputList2<-split(Output2,Output2$Faculty)
length(outputList2)

#outputList2<-lapply(outputList2,function(x) select(x,-Faculty))


?select
#a function that writes a list to the excel file
writeToXLS <- function(myList,myFile) {
    
    #Initialize output workbook
    #require(xlsx)
    wbook <- createWorkbook()
    #Write blank workbook to file
    saveWorkbook(wbook, myFile)
    
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], 'output.xlsx', sheetName=x, append=TRUE))
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], myFile, sheetName=x, append=TRUE))
    #Write output data to saved workbook
    for (name in names(myList)) {
      
        write.xlsx2(myList[[name]], myFile, sheetName=name,append=TRUE)
    }
}

writeToXLS(outputList2,"/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/raw_reg_output_all.xlsx")

?write.xlsx2


####################################Summary data###################################

################################################################
############1. Number of participants by no of modules##########
################################################################
# number of people complete more than 3 modules:
sum(!is.na(Output2$"Module 3")) # 80

# number of people complete 2 modules:
sum(!is.na(Output2$"Module 2")) #207-80=127

# number of people complete 1 module:
sum(!is.na(Output2$"Module 1")) #530-207=323

################################################################
############2. Number of participants##########################
################################################################

# number of participants in each faculty 
no_participants<- lapply(outputList,function(x) length(x[[1]]))


sumup<-function(list){
    sum<-0
    for(ele in no_participants){
        sum<-sum+ele[[1]]
    }
    return (sum)
}
# it sums up the number of participants in total 
#sumup(no_participants)















