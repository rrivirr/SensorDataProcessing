library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringr)
library(gridExtra)

rm(list = ls()) #clear R working memory
# dev.off() # clear plots

# note to self: ctrl+shift+c to comment toggle line(s)

# path to folder with main data folder in it
wd = "/home/pleocavee/Desktop" 

# path fo folder with experiment data in it
ex1_path = paste(wd,sep="","/4xCH4DHT22_20220401")

# destination folder and name of file to create for plot outpus
ex1_dest = paste(ex1_path,sep="","/4xCH4DHT22_20220401_plots.pdf")

# hardcode columns to plot and corresponding y-axis labels
col = c('battery.V','ch4rf_raw','ch4_raw','dht_C','dht_RH')
col_L = length(col) # calculate length for use in for loops
  # this will work once we have calibrated values to also plot
  # col = names(ex1_df_dt)[7:13]
ylabs = c('Battery Voltage', 'Methane Reference Voltage', 'Methane Voltage', 'Temperature (C)', 'Relative Humidity (%)')

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

#generate individual vs time plots
plot_Y_v_Time <-function(df){
  ##list of lists, where list values are the names of the columns and common names for the deployments
  YvT = vector('list', col_L)
  names(YvT) = col
  
  commonName = as.character(unlist(unique(df['cName'])))
  unqDeploy_L = length(commonName)
  
  #initialize empty double list to hold plots
  for(i in 1:col_L){
    YvT[[ col[i] ]] = vector('list', unqDeploy_L)
    names( YvT[[ col[i] ]] ) = commonName
  }
  
  # grouped to make individual processing easier, but maybe not necessary depending on whether further customization is done or not
  # customization could also probably be done with a list as well?
  for(i in 1:col_L){
    if(i == 1){
      for(j in 1:unqDeploy_L){
        YvT[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(df, cName==commonName[j]))+
          geom_point(aes_string(x="dtp",y=col[i]))+
          ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
      }
    } else if (i == 2) {
      for(j in 1:unqDeploy_L){
        YvT[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(df, cName==commonName[j]))+
          geom_point(aes_string(x="dtp",y=col[i]))+
          ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
      }
    } else if (i == 3){
      for(j in 1:unqDeploy_L){
        YvT[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(df, cName==commonName[j]))+
          geom_point(aes_string(x="dtp",y=col[i]))+
          ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
      }
    } else {
      for(j in 1:unqDeploy_L){
        YvT[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(df, cName==commonName[j]))+
          geom_point(aes_string(x="dtp",y=col[i]))+
          ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
      }
    }
  }
  return(YvT)
}

#generate deployment vs time plots
plot_Deployment_v_Time <-function(df){
  # initialize list to hold plots
  DvT = vector('list', col_L)
  names(DvT) = col
  
  # plot each column vs time with all deployments into list
  for(i in 1:col_L){
    if(col[i] == 'battery.V'){
      DvT[[i]] = ggplot(data=df)+
        geom_point(aes_string(x="dtp",y=col[i],color="cName"))+
        ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
    } else if(col[i] == 'ch4rf_raw'){
      DvT[[i]] = ggplot(data=df)+
        geom_point(aes_string(x="dtp",y=col[i],color="cName"))+
        ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
    } else if(col[i] == 'ch4_raw'){
      DvT[[i]] = ggplot(data=df)+
        geom_point(aes_string(x="dtp",y=col[i],color="cName"))+
        ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
    } else if(col[i] == 'dht_C'){
      DvT[[i]] = ggplot(data=df)+
        geom_point(aes_string(x="dtp",y=col[i],color="cName"))+
        ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
    } else if(col[i] == 'dht_RH'){
      DvT[[i]] = ggplot(data=df)+
        geom_point(aes_string(x="dtp",y=col[i],color="cName"))+
        ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
    } else {} # just in case, or for expanding later
  }
  return(DvT)
}

#process experiment 1 into data frame
ex1_df <- merge_ex_csvs(ex1_path)
ex1_df_dt <- process_time(ex1_df)

# #data frame of unique deployments, can be accessed by df[x,1]
# WaterBear = unique(ex1_df_dt['deployment'])
#   #note, might want to rename deployment names to be easier to read or add a WB-datalogger setting for user assigned name -> metadata file rather than every row?
# WaterBear_L = length(WaterBear[,1])
# 
# #create custom names for deployments
# commonName = list()
# for(i in 1:WaterBear_L){
#   commonName[i] = paste("WaterBear-",i,sep="")
# }
# commonName = unlist(commonName) # list into character vector



createNamesTable <-function(df){
  #currently just serially creates names following the format of "WaterBear-X" with X ranging 1 to the number of unique deployments in the data
  unqDeploy = unique(df['deployment'])
  for(i in 1:length(unqDeploy[,1])){
    unqDeploy$cName[i] = paste("WaterBear-",i,sep="")
  }
  return(unqDeploy)
}

ex1_lookupTable <- createNamesTable(ex1_df_dt)
ex1_df_dt$cName <- ex1_lookupTable$cName[match(ex1_df_dt$deployment, ex1_lookupTable$deployment)]

# commonName = as.character(unlist(unqDeploy['cName']))
# unqDeploy_L = length(commonName)

# createLookupTable <- function(){
#   
# }

# #create lookup table to process df
# for(i in 1:WaterBear_L){
#   WaterBear$commonName[i] = paste("WaterBear-",i,sep="")
# }
# # add new column based on commonName
# ex1_df_dt$cName <- WaterBear$commonName[match(ex1_df_dt$deployment, WaterBear$deployment)]

ex1_individual_plots <- plot_Y_v_Time(ex1_df_dt)
ex1_individual_plots

ex1_deployment_plots <- plot_Deployment_v_Time(ex1_df_dt)
ex1_deployment_plots

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

# ##list of lists, where list values are the names of the columns and common names for the deployments
# ex1_subset_plots = vector('list', col_L)
# names(ex1_subset_plots) = col
# #initialize empty double list to hold plots
# for(i in 1:WaterBear_L+1){
#   ex1_subset_plots[[ col[i] ]] = vector('list', WaterBear_L)
#   names( ex1_subset_plots[[ col[i] ]] ) = commonName
# }
# 
# for(i in 1:col_L){
#   print(col[i])
#   if(i == 1){
#     for(j in 1:WaterBear_L){
#       print(commonName[j])
#       ex1_subset_plots[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(ex1_df_dt, cName==commonName[j]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
#     }
#   } else if (i == 2) {
#     for(j in 1:WaterBear_L){
#       print(commonName[j])
#       ex1_subset_plots[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(ex1_df_dt, cName==commonName[j]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
#     }
#   } else if (i == 3){
#     for(j in 1:WaterBear_L){
#       print(commonName[j])
#       ex1_subset_plots[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(ex1_df_dt, cName==commonName[j]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
#     }
#   } else {
#     for(j in 1:WaterBear_L){
#       print(commonName[j])
#       ex1_subset_plots[[ col[i] ]][[ commonName[j] ]] = ggplot(data=subset(ex1_df_dt, cName==commonName[j]))+
#         geom_point(aes_string(x="dtp",y=col[i]))+
#         ylab(ylabs[i])+xlab("Date")+ggtitle(commonName[j])
#     }
#   }
# }

# view plots by groups:
# ex1_plots['battery.V']
# ex1_subset_plots[1] # 'battery.V'
# ex1_subset_plots[2] # 'ch4rf_raw'
# ex1_subset_plots[3] # 'ch4_raw'
# ex1_subset_plots[4] # 'dht_C'
# ex1_subset_plots[5] # 'dht_RH'

#output goal: save each deployment to its own page?
# save all combined plots to one page?


# ##list to hold plots
# ex1_plots = vector('list', col_L)
# names(ex1_plots) = col
# 
# # plot each column vs time with all deployments
# for(i in 1:col_L){
#   if(col[i] == 'battery.V'){
#     ex1_plots[[i]] = ggplot(data=ex1_df_dt)+
#       geom_point(aes_string(x="dtp",y=col[i],color="commonName"))+
#       ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
#   } else if(col[i] == 'ch4rf_raw'){
#     ex1_plots[[i]] = ggplot(data=ex1_df_dt)+
#       geom_point(aes_string(x="dtp",y=col[i],color="commonName"))+
#       ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
#   } else if(col[i] == 'ch4_raw'){
#     ex1_plots[[i]] = ggplot(data=ex1_df_dt)+
#       geom_point(aes_string(x="dtp",y=col[i],color="commonName"))+
#       ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
#   } else if(col[i] == 'dht_C'){
#     ex1_plots[[i]] = ggplot(data=ex1_df_dt)+
#       geom_point(aes_string(x="dtp",y=col[i],color="commonName"))+
#       ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
#   } else if(col[i] == 'dht_RH'){
#     ex1_plots[[i]] = ggplot(data=ex1_df_dt)+
#       geom_point(aes_string(x="dtp",y=col[i],color="commonName"))+
#       ylab(ylabs[i])+xlab("Date")+scale_color_discrete(name="Deployment")
#   } else {} # just in case, or for expanding later
# }


#display plots
# ex1_plots[4]
# ex1_plots











