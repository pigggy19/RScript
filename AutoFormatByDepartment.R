# read excel table from file
#install.packages("xlsx")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
library(xlsx)
library(dplyr)
library(tidyr)
library(stringr)


###############################################################################
# Format registration
###############################################################################
# ?read.xlsx2
originalTab2<-read.xlsx2("/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/reg_raw_input.xlsx", 1,stringsAsFactors=F)
# ?tbl_df
originalTab2<- tbl_df(originalTab2)
# str(originalTab2)
#change column name
names(originalTab2)<-c("First_Name","Last_Name","Staff_ID"
                       ,"Faculty","Department","Module_Title")
# View(originalTab2)
#separate full name

#newTab<-originalTab %>% separate(Person_Full_Name, c("Last_Name", "First_Name"), sep = "\\,")
#trim the string
#newTab$Last_Name<-str_trim(newTab$Last_Name, side = "both")
#newTab$First_Name<-str_trim(newTab$First_Name, side = "both")

# group by faculty and department
# newTab2 <- originalTab2 %>% arrange(Faculty,Department,First_Name);
#newTab$Faculty<-str_trim(newTab$Faculty, side = "both")
#newTab$Department<-str_trim(newTab$Department, side = "both")

# convert the factor of staff_id into numeric
#newTab<-transform(newTab,Staff_ID = as.numeric(Staff_ID))

Output2<-originalTab2 %>%
    filter(Faculty %in% c("Faculty of Medicine Nursing & Health Sci",
                          "Faculty of Engineering",
                          "Faculty of Business & Economics",
                          "Faculty of Arts",
                          "Faculty of Science")) %>% 
    arrange(Faculty,Department,First_Name) %>% 
    group_by(Faculty,Department,Staff_ID,First_Name,Last_Name) %>% 
    select(Faculty,Department,Staff_ID,First_Name,Last_Name, Module_Title)


#add a column to indicate count of occurrences for staff_ID
temp2<-Output2%>%mutate(countOfModule=paste("Module",sequence(n()))) #####n() counts number of rows according to group_by 

#spread training module column into three modules rows
Output2<-temp2%>%spread(countOfModule,Module_Title)

# add indicator column to the table & fill in indicator
Output2<-mutate(Output2,Indicator="")
# Output2[Output2$Department=="N/A",1]<-"NA"   # get rid of / invalid value
Output2<-Output2%>%select(-c(9:13))
Output2$Indicator <- apply(Output2,1, function(x) sum(!is.na(x[6:8])))


View(Output2)
?split

#write to file
# change department into factors & divide it up according to faculty
Output2$Department <- factor(Output2$Department, levels=unique(Output2$Department))
outputList2<-split(Output2,Output2$Department)
as.character(outputList2[1]$Department)
?as.character

for(index in 1:length(outputList2)){
    names(outputList2[[index]][2])<-names(outputList2[index])
}



length(outputList2)
names(outputList2)
#outputList2<-lapply(outputList2,function(x) select(x,-Faculty))


# ?select
#a function that writes a list to the excel file
writeToXLS <- function(myList,myFile) {
    
    #Initialize output workbook
      # require(xlsx)
     # wbook <- createWorkbook()
    #Write blank workbook to file
     # saveWorkbook(wbook, myFile)
    
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], 'output.xlsx', sheetName=x, append=TRUE))
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], myFile, sheetName=x, append=TRUE))
    #Write output data to saved workbook
    for (name in names(myList)) {
        write.xlsx(as.data.frame(myList[[name]]), myFile, row.names = F , sheetName=name,append=TRUE, showNA = F)
    }
}


writeToXLS(outputList2,'/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/reports_by_department/reg_output_by_dep.xlsx')

?write.xlsx

###############################################################################
# Format completion
###############################################################################

originalTab<-read.xlsx2("/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/compl_raw_input.xlsx", 1,stringsAsFactors=F);
originalTab<- tbl_df(originalTab)

#change column name
names(originalTab)<-c("Person_Full_Name","Staff_ID","Faculty","Department","Module_Title")

#separate full name

newTabWithSeparateName<-originalTab %>% separate(Person_Full_Name, c("Last_Name", "First_Name"), sep = "\\,")
#trim the string
newTabWithSeparateName$Last_Name<-str_trim(newTabWithSeparateName$Last_Name, side = "both")
newTabWithSeparateName$First_Name<-str_trim(newTabWithSeparateName$First_Name, side = "both")

# group by faculty and department
# newTab <- newTab %>% arrange(Faculty,Department,First_Name);

# Output<-newTab %>% group_by(Faculty,Department,Staff_ID,First_Name,Last_Name) %>% select(Faculty,Department,Staff_ID,First_Name,Last_Name, Module_Title);
Output<-newTabWithSeparateName %>%
    filter(Faculty %in% c("Faculty of Medicine Nursing & Health Sci",
                          "Faculty of Engineering",
                          "Faculty of Business & Economics",
                          "Faculty of Arts",
                          "Faculty of Science")) %>% 
    arrange(Faculty,Department,First_Name) %>% 
    group_by(Faculty,Department,Staff_ID,First_Name,Last_Name) %>% 
    select(Faculty,Department,Staff_ID,First_Name,Last_Name, Module_Title)

#add a column to indicate count of occurrences for staff_ID
temp<-Output%>%mutate(countOfModule=paste("Module",sequence(n()))) #####n() counts number of rows according to group_by 

Output<-mutate(Output,Indicator="")
# Output2[Output2$Department=="N/A",1]<-"NA"   # get rid of / invalid value


#spread training module column into three modules rows
Output<-temp%>%spread(countOfModule,Module_Title)

# Add indicator and fill the column 
Output<-mutate(Output,Indicator="")

# Output<-Output%>%select(-c(9:13))
Output$Indicator <- apply(Output,1, function(x) sum(!is.na(x[6:8])))




#write to file
# change dep into factor to preserve the original order and divide it up according to department
Output$Department <- factor(Output$Department, levels=unique(Output$Department))

outputList<-split(Output,Output$Department)
length(outputList)




#a function that writes a list to the excel file
writeToXLS <- function(myList,myFile) {
    
    #Initialize output workbook
    #require(xlsx)
    # wbook <- createWorkbook()
    #Write blank workbook to file
    # saveWorkbook(wbook, myFile)
    
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], 'output.xlsx', sheetName=x, append=TRUE))
    # lapply(names(myList), function(x) write.xlsx(myList[[x]], myFile, sheetName=x, append=TRUE))
    #Write output data to saved workbook
    for (name in names(myList)) {
        write.xlsx(as.data.frame(myList[[name]]), myFile,row.names = F , showNA = F, sheetName=name,append=TRUE)
    }
}


writeToXLS(outputList,"/Users/TomLiu/Documents/Ceed/Excel_format/June30th_appended/reports_by_department/compl_output_by_dep.xlsx")


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















