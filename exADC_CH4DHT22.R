library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringr)
library(gridExtra)

rm(list = ls()) #clear R working memory

#note to self: ctrl+shift+c to comment toggle line(s)

#path to folder with main data folder in it
wd = "/home/pleocavee/Desktop" 

ex1_path = paste(wd,sep="","/4xCH4DHT22_20220401")

ex1_dest = paste(ex1_path,sep="","/4xCH4DHT22_20220401_plots.pdf")

#iterate folders in experiment directory, get list of all files in each folder and then append them
#NOTE: does not remove "debug" lines atm, didn't have any at the time
merge_ex_csvs <- function(exDir){
  data_dirs <- list.files(path=exDir, pattern = "ABElab*", all.files = FALSE) # pull all dirs starting with jar
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
  compiled_data$deployment<-as.factor(compiled_data$deployment)
  compiled_data$uuid<-as.factor(compiled_data$uuid)
  
  return(compiled_data)
}

process_basic_plots <-function(compiled_data){
  plot_list = list()
  
  return(plot_list)
}

#process experiment 1 into data frame
ex1_df <- merge_ex_csvs(ex1_path)
ex1_df_dt <- process_time(ex1_df)

#data frame of unique deployments, can be accessed by df[x,1]
WaterBear = unique(ex1_df_dt['deployment'])
  #note, might want to rename deployment names to be easier to read
WaterBear_L = length(WaterBear[,1])
commonName = list()
for(i in 1:WaterBear_L){
  commonName[i] = paste("WaterBear-",i,sep="")
}
commonName = unlist(commonName)

#hardcode columns to plot
col = c('battery.V','ch4rf_raw','ch4_raw','dht_C','dht_RH')
ylabs = c('Battery Voltage', 'Methane Reference Voltage', 'Methane Voltage', 'Temperature (C)', 'Relative Humidity (%)')
  # this will work once we have calibrated values to also plot
  # col = names(ex1_df_dt)[7:13]


#initialize list of lists to hold plots based on deployment count and col count
col_L = length(col)
ls = vector('list', col_L)

##double index method (hard for a user to interact with)
# for(i in 1:WaterBear_L){
#   ls[[i]] = vector('list', WaterBear_L)
# }
# 
# for(i in 1:col_L){
#   if(i == 1){
#     for(j in 1:WaterBear_L){
#       ls[[i]][[j]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+
#         xlab("Date")+
#         ggtitle(paste("WaterBear-",j,sep=""))
#       # ggtitle(WaterBear[j,1])
#     }
#   } else if (i == 2) {
#     for(j in 1:WaterBear_L){
#       ls[[i]][[j]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+
#         xlab("Date")+
#         ggtitle(paste("WaterBear-",j,sep=""))
#       # ggtitle(WaterBear[j,1])
#     }
#   } else if (i == 3){
#     for(j in 1:WaterBear_L){
#       ls[[i]][[j]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+
#         xlab("Date")+
#         ggtitle(paste("WaterBear-",j,sep=""))
#       # ggtitle(WaterBear[j,1])
#     }
#   } else {
#     for(j in 1:WaterBear_L){
#       ls[[i]][[j]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+
#         xlab("Date")+
#         ggtitle(paste("WaterBear-",j,sep=""))
#       # ggtitle(WaterBear[j,1])
#     }
#   }
# }

##list of lists, but the lists are the names of the columns and common names for the deployments
ex1_plots = vector('list', col_L)
names(ex1_plots) = col
#initialize empty double list to hold plots
for(i in 1:WaterBear_L+1){
  ex1_plots[[ col[i] ]] = vector('list', WaterBear_L)
  names( ex1_plots[[ col[i] ]] ) = commonName
}

for(i in 1:col_L){
  if(i == 1){
    for(j in 1:WaterBear_L){
      ex1_plots[[col[i]]][[commonName[j]]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(commonName[j])
        # ggtitle(paste("WaterBear-",j,sep=""))
        # ggtitle(WaterBear[j,1])
    }
  } else if (i == 2) {
    for(j in 1:WaterBear_L){
      ex1_plots[[col[i]]][[commonName[j]]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(commonName[j])
        # ggtitle(paste("WaterBear-",j,sep=""))
        # ggtitle(WaterBear[j,1])
    }
  } else if (i == 3){
    for(j in 1:WaterBear_L){
      ex1_plots[[col[i]]][[commonName[j]]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(commonName[j])
        # ggtitle(paste("WaterBear-",j,sep=""))
        # ggtitle(WaterBear[j,1])
    }
  } else {
    for(j in 1:WaterBear_L){
      ex1_plots[[col[i]]][[commonName[j]]] = ggplot(data=subset(ex1_df_dt, deployment==WaterBear[j,1]))+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(commonName[j])
        # ggtitle(paste("WaterBear-",j,sep=""))
        # ggtitle(WaterBear[j,1])
    }
  }
}

# view plots by groups:
# print(ex1_plots['battery.V'])
print(ex1_plots[1]) # 'battery.V'
print(ex1_plots[2]) # 'ch4rf_raw'
print(ex1_plots[3]) # 'ch4_raw'
print(ex1_plots[4]) # 'dht_C'
print(ex1_plots[5]) # 'dht_RH'

#output goal: save each deployment to its own page?
# save all combined plots to one page?

#open PDF
# pdf(file=ex1_dest, onefile=TRUE)
# 
# #specify to save plots in 2x2 grid
# par(mfrow = c(2,2))
# 
# #save plots to PDF
# for (i in seq(length(ls))) {
#   do.call("grid.arrange", ls[i])  
# }
# 
# #turn off PDF plotting
# dev.off()













