# read excel table from file
install.packages("xlsx")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
library(xlsx)
library(dplyr)
library(tidyr)
library(stringr)
?read.xlsx2
originalTab<-read.xlsx2("/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/compl_raw_input.xlsx", 1);
originalTab<- tbl_df(originalTab)

#change column name
names(originalTab)<-c("Person_Full_Name","Staff_ID","Faculty","Department","Training_Module_Title")

#separate full name

newTab<-originalTab %>% separate(Person_Full_Name, c("Last_Name", "First_Name"), sep = "\\,")
#trim the string
newTab$Last_Name<-str_trim(newTab$Last_Name, side = "both")
newTab$First_Name<-str_trim(newTab$First_Name, side = "both")

# group by faculty and department
newTab <- newTab %>% arrange(Faculty,Department,First_Name);

Output<-newTab %>% group_by(Faculty,Department,Staff_ID,First_Name,Last_Name) %>% select(Faculty,Department,Staff_ID,First_Name,Last_Name, Training_Module_Title);


#add a column to indicate count of occurrences for staff_ID
temp1<-Output%>%mutate(countOfModule=paste("Module",sequence(n()))) #####n() counts number of rows according to group_by 

#spread training module column into three modules rows
Output<-temp1%>%spread(countOfModule,Training_Module_Title)
Output<-mutate(Output,Indicator="")





#write to file
#divide it up according to faculty
outputList<-split(Output,Output$Faculty)
#length(outputList)

outputList<-lapply(outputList,function(x)select(x,-Faculty))


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
        write.xlsx(myList[[name]], myFile, sheetName=name,append=TRUE)
    }
}

writeToXLS(outputList,"/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/raw_compl_output.xlsx")



############################### To get summary of data ############################

################################################################
############1. Number of participants by no of modules##########
################################################################

# number of people complete more than 3 modules:
sum(!is.na(Output$"Module 3")) # 18

# number of people complete 2 modules:
sum(!is.na(Output$"Module 2")) #66-18 = 48

# number of people complete 1 module:
sum(!is.na(Output$"Module 1")) #252-66 = 186

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







