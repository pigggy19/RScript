
library(dplyr)
library(tidyr)

#clear objects
#rm(list = ls(pattern="^[X]"))

#import file
job_click<-read.csv2("/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/V2/job_clicks_all_V2.csv",header=TRUE,sep="\t",fill=TRUE)
job_impression<-read.csv2("/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/V2/job_impressions_all_V2.csv",header=TRUE,sep="\t", fill=TRUE)
job_searches<-read.csv2("/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/V2/job_searches_all_V2.csv",header=TRUE,sep="\t", fill=TRUE)
job_all<-read.csv2("/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/jobs_all.csv",header=TRUE,sep="\t",fill=TRUE)
job_classification<-read.table("/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/job_classification.txt",header=TRUE,sep="\t",fill=TRUE) #job classification
str(job_all)
View(job_classification)


job_click1<- tbl_df(job_click);
job_impression1 <- tbl_df(job_impression);
job_searches1<-tbl_df(job_searches);
job_all1<-tbl_df(job_all);
job_classification1<-tbl_df(job_classification)

#view detail
job_click1%>%head(10)
job_impression1%>%head(10)
job_searches1%>%head(100)
job_all1%>%head(100)
str(job_all1)
str(job_click)
#separate files
job_click1<-job_click1%>%separate(job_id.user_id.session_id.search_id.created_at,
                      into=c("job_id","user_id","session_id","research_id","created_at"
                             ), convert="TRUE",sep=",",extra="drop")


write.csv(job_click1, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_click.csv",row.names=FALSE)
write.csv(job_impression1, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_impression.csv",row.names=FALSE)
write.csv(job_searches1, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_search.csv",row.names=FALSE)
write.table(job_all1, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_all.csv",row.names=FALSE, na="", sep=",")
#write.csv(job_all1, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_all.csv",row.names=FALSE)

#how many distinct job's are turned into impressions?
str(job_all1) #1439436 
count(distinct(select(job_impression1, job_id)))
count(job_impression1)
distinct(select(job_impression1,job_id)) #927874 jobs in the impression

joined_job_impression <-job_all1%>% inner_join(job_impression1,c("job_id"= "job_id"))
job_id_impression<-distinct(select(joined_job_impression,job_id)) #805378 jobs are showing up in the impression list
# write.csv(job_id_impression, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_id_in_impression.csv",row.names=FALSE)
(select(joined_job_impression,job_id))

#how many distinct job's are turned into clicks?
str(job_click1)
str(job_all1)
count(distinct(select (job_click1,job_id))) #287713
joined_job_click <-job_all1%>% inner_join(job_click1,c("job_id"= "job_id"))
job_id_click<-distinct(select (joined_job_click,job_id)) #256618 jobs are showing up in the click list
write.csv(job_id_click, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_id_in_click.csv",row.names=FALSE)


#click through rate--how many impressions are turned into clicks
str(job_impression1) #15963442
str(job_click1) # 688971
joined_click_impression <-job_impression1%>% inner_join(job_click1,c("job_id"= "job_id","session_id"="session_id","search_id"="search_id", "user_id"="user_id"))
distinct(select (joined_click_impression,job_id)) #287713
names(joined_click_impression)


#get the job list that appear in impression and has been searched by user
joined_job_search_impression <- joined_job_impression%>% inner_join(job_searches1, c("search_id"= "search_id","user_id"= "user_id")) %>% inner_join(job_classification1, c("subclasses"="sub_class_id"));
View(joined_job_search_impression)
names(joined_job_search_impression)
#change names giving alias
names(joined_job_search_impression)[c(3:4,17:20,24:25)]=
    c("raw_location_job","location_id_job", "mobile_user_impression","created_time_impression","raw_location_search",
      "location_id_search","mobile_user_search","created_time_search");

write.csv(joined_job_search_impression, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_search_impression.csv",row.names=FALSE)


#get the job list that appear in impression and click has been searched by user
View(job_click1)
joined_job_search_impression_click <-joined_job_search_impression%>% inner_join(job_click1, c("search_id"= "search_id","user_id"= "user_id","session_id"= "session_id","job_id"= "job_id"));
names(joined_job_search_impression_click)
count(select (joined_job_search_impression_click,job_id))
#get the joined list with impression coming with clicks or without clicks
joined_job_search_impression_click_lfjoin <-joined_job_search_impression%>% left_join(job_click1, c("search_id"= "search_id","user_id"= "user_id","session_id"= "session_id","job_id"= "job_id"));


#alias for the table
names(joined_job_search_impression_click_lfjoin)[29]<-"created_time_click"
names(joined_job_search_impression_click)[c(24,29)]<-c("mobile_user_search","created_time_click")
write.csv(joined_job_search_impression_click, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_search_impression_click.csv",row.names=FALSE)
write.csv(joined_job_search_impression_click_lfjoin, file = "/Users/TomLiu/GoogleDrive/Datathon/MelbourneDatathon2016/all/reformatted/job_search_impression_click_lftjoin.csv",row.names=FALSE)
View(joined_job_search_impression_click_lfjoin)

#change factor to numeric value
region[,1]<-as.numeric(levels(region[,1]))[region[,1]]
region[,2]<-as.numeric(levels(region[,2]))[region[,2]]

# 1) Average time lapse between impression and click.
names(joined_job_search_impression_click)
#convert factor to time
library(lubridate)

joined_job_search_impression_click$created_time_click <- ymd_hms(joined_job_search_impression_click$created_time_click)
joined_job_search_impression_click$created_time_impression<-ymd_hms(joined_job_search_impression_click$created_time_impression)

click_impression_diff<-difftime(joined_job_search_impression_click$created_time_click,joined_job_search_impression_click$created_time_impression,units="secs")

#2) Per user, average time (days/minutes) between 2 separate searches.
names(job_searches1)
group_by(job_searches1,user_id,)%>%summarize(count=n())

# 3)Per user, no of searches via mobile app vs desktop. Is there any changes in the click thrus?
    # of searches via mobile app 272743
    # of searches via nonmobile 200961
    mobile_user<- subset(job_searches1,job_searches1$mobile_user==1)
    non_mobile_user<-subset(job_searches1,job_searches1$mobile_user==0)
    count(mobile_user)
    count(non_mobile_user)
    
    names(joined_job_impression)
    names(joined_job_search_impression_click)
    #click thrus for mobile users 7%   909666/13036096
    select (joined_job_search_impression_click,job_id) #The total number of clicks generated from jobs are 909,666
    select (joined_job_search_impression,job_id) #The total number of impressions generated from jobs are 13,036,096

    



    length(unique(joined_job_search_impression_click$job_id))# 220947 unique jobs
    jobs_mobile_click<- length(unique(subset(joined_job_search_impression_click$job_id,joined_job_search_impression_click$mobile_user_impression==1))) #167500 unique jobs in mobile clicks
    jobs_non_mobile_click<- length(unique(subset(joined_job_search_impression_click$job_id,joined_job_search_impression_click$mobile_user_impression==0))) #113507 unique jobs in non-mobile clicks  
    jobs_mobile_impression<-length(unique(subset(joined_job_impression$job_id,joined_job_impression$mobile_user==1))) #700147 unique jobs in mobile impression
    jobs_non_mobile_impression<-length(unique(subset(joined_job_impression$job_id,joined_job_impression$mobile_user==0))) #635930 unique jobs in non mobile impression

    mobile_user_click_thrus<- 
    (count(subset(joined_job_search_impression_click,joined_job_search_impression_click$mobile_user_impression==1))/jobs_mobile_click)/
    (count(subset(joined_job_impression,joined_job_impression$mobile_user==1))/jobs_mobile_impression)
    #click thrus for nonmobile users  22.2%
    (count(subset(joined_job_search_impression_click,joined_job_search_impression_click$mobile_user_impression==0))/jobs_non_mobile_click)/
    (count(subset(joined_job_impression,joined_job_impression$mobile_user==0))/jobs_non_mobile_impression)
  

#4) Per user, duration when user was actively searching (time between first and last search).
 head(job_searches);
 search_duration_df<-select(job_searches1,user_id, search_id,created_at)
 search_duration_df$created_at<-ymd_hms(search_duration_df$created_at)
 View(search_duration_df)
 temp_1<-group_by(search_duration_df,user_id)%>%filter(created_at==max(created_at)) %>%arrange(user_id,created_at)
 temp_2<-group_by(search_duration_df,user_id)%>%filter(created_at==min(created_at)) %>%arrange(user_id,created_at)
   
   count(temp_1)
 temp<-temp_1%>%left_join(temp_2,c("user_id"= "user_id"))
    names(temp)[c(3,5)]<-c("max","min")
head(temp)
 temp$duration<-mutate(temp,duration=as.numeric(difftime(temp$max,temp$min,units="hours")))
count(as.numeric(difftime(temp$max,temp$min,units="hours")))

# how many records with same searchid, same time but different users?
temp<-group_by(search_duration_df,search_id)%>%arrange(created_at)
View(temp)

str(joined_job_search_impression_click)
table(joined_job_search_impression_click$salary_min)
table(joined_job_search_impression_click$salary_max)


par(mfrow=c(1,2))
with(joined_job_search_impression_click,{
     plot(job_id,salary_min,main="Clicks VS MinSalary")
     plot(job_id,salary_max,main="Clicks VS MaxSalary")
    mtext("Clicks & Salary")
})


# how many jobs have come with impressions and how many come without ones?
joined_job_impression_lftjoin <-job_all1%>% left_join(job_impression1,c("job_id"= "job_id"))
count(joined_job_impression_lftjoin)

temp_1<-subset(joined_job_impression_lftjoin,is.na(joined_job_impression_lftjoin['user_id']))
temp_2<-subset(joined_job_impression_lftjoin,!(is.na(joined_job_impression_lftjoin['user_id'])))
count(distinct(select(joined_job_impression_lftjoin,job_id)))
temp1<-count(distinct(select(temp_1,job_id)))
temp2<-count(distinct(select (temp_2,job_id)))

#temp1<-colSums(is.na(joined_job_impression_lftjoin['user_id']))
#temp2<-colSums(!is.na(joined_job_impression_lftjoin['user_id']))
names(joined_job_impression_lftjoin)
colSums(joined_job_impression_lftjoin['Segment']=='unkown')

temp<-data.frame(c(temp1,temp2))

names(temp)<-c('Without-Impressions','With-Impressions')
temp<-as.matrix(temp)
rownames(temp)

bplot<-barplot(temp, main="Jobs with Impressions VS Jobs Without Impressions",
        ylab="# of Jobs", col=c("blue"),ylim= c(0,890000),
        xlim=c(0,1), width=0.3, space=0.3)

text(bplot, temp, labels = temp, pos = 3, cex = 0.8)

range(temp)

# how many jobs have come with clicks and how many come without ones?

joined_job_click_lftjoin <-job_all1%>% left_join(job_click1,c("job_id"= "job_id"))
count(joined_job_click_lftjoin)

temp_1<-subset(joined_job_click_lftjoin,is.na(joined_job_click_lftjoin['user_id']))
temp_2<-subset(joined_job_click_lftjoin,!(is.na(joined_job_click_lftjoin['user_id'])))
count(distinct(select(joined_job_click_lftjoin,job_id)))
temp1<-count(distinct(select(temp_1,job_id)))
temp2<-count(distinct(select (temp_2,job_id)))

temp<-data.frame(c(temp1,temp2))

names(temp)<-c('Without-Clicks','With-Clicks')
temp<-as.matrix(temp)


bplot<-barplot(temp, main="Jobs with Clicks VS Jobs Without Clicks",
               ylab="# of Jobs", col=c("blue"),ylim= c(0,1300000),
               xlim=c(0,1), width=0.3, space=0.3)

text(bplot, temp, labels = temp, pos = 3, cex = 0.8)
attach(joined_job_click_lftjoin)
joined_job_search_impression_click[salary_min==300000,]
detach(joined_job_search_impression_click)
Segment
joined_job_click_lftjoin[is.na(created_at)==TRUE,job_id]

edit(joined_job_click_lftjoin)
