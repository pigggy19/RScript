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

originalTab2<-read.xlsx2("/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/reg_raw_input.xlsx", 1,stringsAsFactors=F)

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

#newTab2 <- originalTab2 %>% arrange(Faculty,Department,First_Name);
#newTab$Faculty<-str_trim(newTab$Faculty, side = "both")
#newTab$Department<-str_trim(newTab$Department, side = "both")

# convert the factor of staff_id into numeric
#newTab<-transform(newTab,Staff_ID = as.numeric(Staff_ID))

Output2<-originalTab2 %>% arrange(Faculty,Department,First_Name)%>% 
    group_by(Faculty,Department,Staff_ID,First_Name,Last_Name)%>% 
    select(Faculty,Department,Staff_ID,First_Name,Last_Name, Module_Title) 
#add the title to levels
#factor(Output2, levels=c(levels(Output2), "Staff_ID","First_Name","Last_Name","Module_Title"))

#add a column to indicate count of occurrences for staff_ID
Output2<-Output2%>%mutate(countOfModule=paste("Module",sequence(n()))) #####n() counts number of rows according to group_by 

#spread training module column into three modules rows
Output2<-Output2%>%spread(countOfModule,Module_Title)
# add indicator column to the table & fill in indicator
Output2<-mutate(Output2,Indicator="")
#The column to remove depends on what is the max number of modules participants have taken. The geneneral
# is keep 3 modules only.
Output2<-Output2%>%select(-c(8:12))
Output2$Indicator <- apply(Output2,1, function(x) sum(!is.na(x[5:7])))

Output2<-Output2%>%select(-c(9:13))
?spread

View(Output2)


#write to file
#divide it up according to faculty
outputList2<-split(Output2,Output2$Faculty)
length(outputList2)
#outputList2<-lapply(outputList2,function(x) select(x,-Faculty))
#apply add row function to the data list
outputList2<- addRowAccordingToDepartment(outputList2)



#This function adds an additional row according to department
addRowAccordingToDepartment<-function(facultyList){
    for (i in (1:length(facultyList))){
        tempDFByFac<-facultyList[[i]]
        tempListByDep<-split(tempDFByFac,tempDFByFac$Department,drop=T)
        tempDFholder<-data.frame()
        ?data.frame
        for(j in (1:length(tempListByDep))){
            tempDFByDep<-tempListByDep[[j]]
            to_be_added<-c(tempDFByDep$Faculty[j],tempDFByDep$Department[j],
                           "Staff ID","First_Name","Last_Name",
                           "Module 1","Module 2","Module 3","Indicator")
            binded<-rbind(to_be_added,tempDFByDep)
            tempDFholder<- rbind(tempDFholder,binded)
            #tempListByDep[names(tempListByDep)[j]]<-list(binded)
        }
        facultyList[i]<-list(tempDFholder)
    }
    return (facultyList)
}



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

writeToXLS(outputList2,"/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/raw_reg_outputTest1.xlsx")



####################################Summary data###################################

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














