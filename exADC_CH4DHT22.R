# exADC_CH4DHT22.R created Apr4, 2022 by Ken Chong
# Code to process WaterBear sonde data containing one NGM2611-E13 methane sensor, and one DHT22 temperature and humidity sensor
# Produces individual plots for specified columns vs time per deployed WaterBear
# Produces conglomerate aggregate plots for specified columns vs time for the entire experiment
# 
# Additional goals:
# Aggregate and average by burst size, then create hourly plots
# Arrange and add plots to output pdf
# Create metadata file with summary statistics

### Required Packages ###
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringr)
library(gridExtra)

# note to self: ctrl+shift+c to comment toggle line(s)

### garbage collection lines ###
rm(list = ls()) # clear R working memory
graphics.off() # close any open plots

### SETTINGS ###
# path to folder with main data folder in it
wd = "/home/pleocavee/Desktop" 

# path of folder with experiment data in it
ex1_path = paste(wd,sep="","/4xCH4DHT22_20220401")

# destination folder and name of file to create for plot output
ex1_dest = paste(ex1_path,sep="","/4xCH4DHT22_20220401_plots.pdf")

# destination folder and name of file to create for metadata
ex1_metadata_path = paste(ex1_path,"/4xCH4DHT22_20220401_METADATA.txt",sep="")

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

createNamesTable <-function(df){
  #currently just serially creates names following the format of "WaterBear-X" with X ranging 1 to the number of unique deployments in the data
  unqDeploy = unique(df['deployment'])
  for(i in 1:length(unqDeploy[,1])){
    unqDeploy$cName[i] = paste("WaterBear-",i,sep="")
  }
  return(unqDeploy)
}

#process experiment 1 into data frame
ex1_df <- merge_ex_csvs(ex1_path)
ex1_df_dt <- process_time(ex1_df)

ex1_lookupTable <- createNamesTable(ex1_df_dt)
ex1_df_dt$cName <- ex1_lookupTable$cName[match(ex1_df_dt$deployment, ex1_lookupTable$deployment)]

ex1_individual_plots <- plot_Y_v_Time(ex1_df_dt)
# ex1_individual_plots # open plots in viewer

ex1_deployment_plots <- plot_Deployment_v_Time(ex1_df_dt)
# ex1_deployment_plots # open plots in viewer


### write plots to pdf ###
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



#### write metadata file ###
# fileConn<-file(ex1_metadata_path)
# writeLines(c("Hello","World"), fileConn)
# close(fileConn)













