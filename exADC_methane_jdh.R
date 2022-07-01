#note had to apt-get install libblas-dev liblapack-dev gfortran cmake libssl-dev
# install.packages("openssl")
# install.packages("httr")
# install.packages("nloptr", dependencies = TRUE)
# install.packages("lme4", dependencies = TRUE)
# install.packages("pbkrtest", dependencies = TRUE)
# install.packages("car", dependencies = TRUE)
# install.packages("rstatix", dependencies = TRUE)
# 
# install.packages("ggpubr", repo = "https://cloud.r-project.org", dependencies = TRUE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringr)
library(data.table)

#rm(list = ls()) #clear R working memory

#note to self: ctrl+shift+c to comment toggle line(s)

#path to folder with main data folder in it
wd = "/Volumes/GoogleDrive/My Drive/WaterBear" 

ex2_path = paste(wd,sep="","/methanejars/ex2")
ex3_path = paste(wd,sep="","/methanejars/ex3")

#iterate folders in experiment directory, get list of all files in each folder and then append them
#NOTE: does not remove "debug" lines atm, didn't have any at the time
merge_ex_csvs <- function(exDir){
   data_dirs <- list.files(path=exDir, pattern = "jar*", all.files = FALSE) # pull all dirs starting with jar
   compiled_data<-data.frame() #prepare data frame to hold info from all csv files
   
   #iterate folders in working directory
   for(i in 1:length(data_dirs)){
      currentdir<-paste(exDir,"/",data_dirs[i],sep="")
      data_files<-list.files(path=paste(currentdir)) #find all files in sub directory
      #read each .csv
      for(j in 1:length(data_files)){
         data<-read.csv(paste(currentdir,"/",data_files[j],sep=""))
         if(nrow(data)>0){ #append all csv data into one file
            compiled_data<-bind_rows(compiled_data,data)
         }
      }
   }
   return(compiled_data)
}

#convert time.s to datetime column, split into hour and date, create a new hourly factor, change site and uuid to factors
process_time <- function(compiled_data){
   compiled_data$dtp<-lubridate::as_datetime(compiled_data$time.s)
   compiled_data$hour<-hour(compiled_data$dtp)
   compiled_data$date<-date(compiled_data$dtp)
   compiled_data$datehour<-as.factor(format.Date(compiled_data$dtp, format = "%Y-%m-%d %H"))
   compiled_data$site<-as.factor(compiled_data$site)
   compiled_data$uuid<-as.factor(compiled_data$uuid)
   
   return(compiled_data)
}

ex2_df <- merge_ex_csvs(ex2_path)
ex2_df_dt <- process_time(ex2_df)

ex3_df <- merge_ex_csvs(ex3_path)
ex3_df_dt <- process_time(ex3_df)

#basic plots of data
ex2_rf <- ggplot(data=ex2_df_dt)+geom_point(aes(x=dtp,y=ch4rf_raw,color=site))
ex2_data <- ggplot(data=ex2_df_dt)+geom_point(aes(x=dtp,y=ch4_raw,color=site))

ex3_rf <- ggplot(data=ex3_df_dt)+geom_point(aes(x=dtp,y=ch4rf_raw,color=site))
ex3_data <- ggplot(data=ex3_df_dt)+geom_point(aes(x=dtp,y=ch4_raw,color=site))

ex3_jar3 <- ex3_df_dt %>% filter(site == "jar3")
ex3_jar3_hourly <- ggplot(data=ex3_jar3)+geom_point(aes(x=dtp,y=ch4rf_raw), size=0)+facet_wrap(ex3_jar3$datehour)
ex3_jar3_hourly

#code for identifying groups based on 15 minute interval

ex3_df_dt_g<-data.frame()
for(i in unique(ex3_df_dt$site)){
	ex3_df_dt_0<-subset(ex3_df_dt,site==i)
ex3_df_dt_0$group <- cumsum(ifelse(difftime(ex3_df_dt_0$dtp,
                                  shift(ex3_df_dt_0$dtp, fill = ex3_df_dt_0$dtp[1]), 
                                  units = "mins") >= 25 
                         ,1, 0)) + 1
ex3_df_dt_0$seq <- ave(ex3_df_dt_0$ch4rf_raw, ex3_df_dt_0$group, FUN = seq_along)
ex3_df_dt_diff<-as.data.frame(ex3_df_dt_0 %>% group_by(group) %>% mutate(min_time = min(dtp, na.rm = TRUE)))
ex3_df_dt_diff$time_diff<-ex3_df_dt_diff$dtp-ex3_df_dt_diff$min_time

		ex3_df_dt_g<-bind_rows(ex3_df_dt_g,ex3_df_dt_diff)
}







	
ggplot(ex3_df_dt_g,aes(x=seq,y=ch4rf_raw))+
geom_point(aes(color=group))+
facet_wrap(.~site)

	
ggplot(ex3_df_dt_g,aes(x=time_diff,y=ch4rf_raw))+
geom_point(aes(color=group))+
facet_wrap(.~site)	

ex3_df_dt_jar1<-subset(ex3_df_dt_g,site=="jar0")

ggplot(ex3_df_dt_jar1,aes(x=seq,y=ch4rf_raw))+
geom_point(aes(color=group))

ggplot(ex3_df_dt_jar1,aes(x=time_diff,y=ch4rf_raw))+
geom_point(aes(color=group))


#goal here was to try to serially create plots with unique names, then use that to plot a series of graphs by hours
#this does not work, can't use variable for <-
#can return a plot though
# test <- function(dataSet,times,name){
#    for (i in 0:times) {
#       # print(paste("printing:", i))
#       # print(paste(name,i, sep = ""))
#       plotName = paste(sep="",name,i)
#       plotName <- ggplot(data=dataSet)+geom_point(aes(x=dtp,y=ch4rf_raw,color=site))
#    }
#    return(plotName)
# }
# plotTest <- test(ex3_df_dt,2,"test")
# plotTest

#experiment 2: 10 burst loops on 1 minute delay each half hour
#subset data by hour and date
ex2_hour_subset_rf <- subset(ex2_df_dt,hour==22&date==as.Date("2022-01-11"))
#graph ex2 hour
ex2_hour_rf <- ggplot(data=ex2_hour_subset_rf)+geom_point(aes(x=dtp,y=ch4rf_raw,color=site))
   #+facet_wrap(.~site) # option to separate graphs by site
ex2_hour_data <- ggplot(data=ex2_hour_subset_rf)+geom_point(aes(x=dtp,y=ch4_raw,color=site))

#experiment 3: 20 burst loops on 1 minute delay each hour
#subset by hour and date
ex3_hour_subset <- subset(ex3_df_dt,hour==18&date==as.Date("2022-01-21"))
#graph ex3 hour
ex3_hour_rf <- ggplot(data=ex3_hour_subset)+geom_point(aes(x=dtp,y=ch4rf_raw,color=site))
ex3_hour_data <- ggplot(data=ex3_hour_subset)+geom_point(aes(x=dtp,y=ch4_raw,color=site))

#arrange basic and single hour graphs
ex2Final <- ggarrange(ncol=2, nrow=2, labels=c("A","B","C","D"), ex2_rf, ex2_data, ex2_hour_rf, ex2_hour_data)
#ex2Final

ex3Final <- ggarrange(ncol=2, nrow=2, labels=c("A","B","C","D"), ex3_rf, ex3_data, ex3_hour_rf, ex3_hour_data)
#ex3Final

