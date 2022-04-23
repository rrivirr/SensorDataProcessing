# exADC_CH4DHT22.R created Apr 4, 2022 by Ken Chong
# Last edit: Apr 12, 2022 by Ken Chong
#
# Code to process WaterBear sonde data containing output from one Figaro NGM2611-E13 methane sensor, and one Adafruit DHT22 temperature and humidity sensor
# Produces individual plots for specified columns vs time per deployed WaterBear
# Produces aggregate plots for specified columns vs time for the entire experiment
# 
# Additional goals:
# Aggregate and average by burst size, then create hourly plots
# Arrange and add plots to output pdf
# Create metadata file with summary statistics
# Create system to flag potentially erroneous data for inspection
# Backup produced files or original data (or separate bash script)?
#
# Thoughts and considerations:
# Should this script take user input? For values that are hardcoded currently
#   Depends on how often script will have to be run
# install.packages('NCmisc')

# note to self: ctrl+shift+c to comment toggle line(s)
# install.packages('tidyverse')

### Required Packages ###
library(dplyr) # bind, relocate
library(lubridate) # as_datetime, hour, date
library(ggplot2) # plotting package
library(ggpubr) # ggarrange
# library(tidyverse) # 
# library(data.table) # melt
# library(stringr) #not used currently
# library(gridExtra) #not used currently

# list alll packages and what functions are being used in them:
library(NCmisc)
list.functions.in.file('exADC_CH4DHT22.R',alphabetic=TRUE)

### garbage collection lines ###
rm(list = ls()) # clear R working memory
graphics.off() # close any open plots

### R Options ###
options('digits'=15) # default is 7, increasing to show full precision of numbers (mainly time.s needs at least 13)

### R SCRIPT SETTINGS ###
# path to folder with main data folder in it
wd = "~/Desktop" #linux path

# USER INPUT - BASE EXPERIMENT FOLDER NAME #
ex1_folder = "4xCH4DHT22_20220401"
ex2_folder = "4xCH4DHT22_20220408"
ex3_folder = "4xCH4DHT22_20220411"

ex1_path = paste(sep="",wd,"/",ex1_folder)
ex2_path = paste(sep="",wd,"/",ex2_folder)
ex3_path = paste(sep="",wd,"/",ex3_folder)

# destination folder and name of file to create for plot and metadata output, based on experiment folder name
  # have each function build the output names?
ex1_pdf_dest = paste(sep="",ex1_path,"/",ex1_folder,"_plots.pdf")
ex2_pdf_dest = paste(sep="",ex2_path,"/",ex2_folder,"_plots.pdf")
ex3_pdf_dest = paste(sep="",ex3_path,"/",ex3_folder,"_plots.pdf")
ex1_metadata_dest = paste(sep="",ex1_path,"/",ex1_folder,"_METADATA.txt")
ex2_metadata_dest = paste(sep="",ex2_path,"/",ex2_folder,"_METADATA.txt")
ex3_metadata_dest = paste(sep="",ex3_path,"/",ex3_folder,"_METADATA.txt")
ex1_qc_dest = paste(sep="",ex1_path,"/",ex1_folder,"_QC.txt")
ex2_qc_dest = paste(sep="",ex2_path,"/",ex2_folder,"_QC.txt")
ex3_qc_dest = paste(sep="",ex3_path,"/",ex3_folder,"_QC.txt")


### USER INPUT - EXPERIMENT SETTINGS ###
# assuming same settings for all currently #
# ideally read from a header in the future #
wakeInterval <- 60 #min
burstCycles <- 20
startupDelay <- 0 #min
interBurstDelay <- 1 #min
readingCount <- 10
readingsPerWake <- burstCycles * readingCount

# hardcoded columns to plot and corresponding y-axis labels
column = c('battery.V','ch4rf_raw','ch4_raw','dht_C','dht_RH')
col_L = length(column) # calculate length for use in for loops
  # this will work once we have calibrated values to also plot
  # column = names(ex1_df_dt)[7:13]
ylabs = c('Battery', 'Methane Reference', 'Methane', 'Temperature (°C)', 'Relative Humidity (%)')

# hardcoded columns to calculate metrics for:
colMetrics = c('time.s','battery.V','ch4rf_raw','ch4_raw','dht_C','dht_RH')
# hardcoded decimal digits to round to for each column
colRnd = c(3, 0, 0, 0, 1, 1)
# hardcoded gross error limits per column (min, max)
timeMin <- as.numeric(as.POSIXct("2000-01-01"))
timeMax <- as.numeric(as.POSIXct("2100-01-01"))
grossLimits = c(timeMin,timeMax,0,4096,0,4096,0,4096,-30,50,0,100)

### R FUNCTIONS ###

# check if file at path exists, delete if it does
  # will only call in functions that generate new files
cleanFile <-function(path){
  if (file.exists(path)){
    sprintf("Deleting old file: %s",path)
    file.remove(path)
  }
}

#iterate folders in experiment directory, get list of all files in each folder and then append them
#NOTE: does not remove "debug" lines atm, didn't have any at the time
merge_ex_csvs <- function(exDir){
  data_dirs <- list.files(path=exDir, pattern = "ABElab*", all.files = FALSE) # pull all dirs starting with ABElab
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

#### generate individual vs time plots ####
## basic version, see commit 9644284 on Apr 12, 2022 for expanded version
#scatter plot, possible to customize and reuse function? or need to create separate functions for each style of plot? probably safer to have separate
plot_Y_v_Time <-function(df){
  ##list of lists, where list values are the names of the columns and common names for the deployments
  YvT = vector('list', col_L)
  names(YvT) = column

  commonName = as.character(unlist(unique(df['cName'])))
  unqDeploy_L = length(commonName)

  #initialize empty double list to hold plots
  for(i in 1:col_L){
    YvT[[ column[i] ]] = vector('list', unqDeploy_L)
    names( YvT[[ column[i] ]] ) = commonName
  }

  for(i in 1:col_L){
    for(j in 1:unqDeploy_L){
      YvT[[ column[i] ]][[ commonName[j] ]] = ggplot(data=subset(df, cName==commonName[j]))+
        geom_point(aes_string(x="dtp",y=column[i]),size=1)+theme_classic()+
        ylab(ylabs[i])+xlab("Date (M-D Hr)")+ggtitle(commonName[j])+scale_x_datetime(date_labels="%m-%d %H",breaks=scales::pretty_breaks(n=4))
    }
  }
  return(YvT)
}

#### generate deployment vs time plots ####
## basic version, see commit 9644284 on Apr 12, 2022 for expanded version
plot_Deployment_v_Time <-function(df){
  # initialize list to hold plots
  DvT = vector('list', col_L)
  names(DvT) = column

  # plot each column vs time with all deployments into list
  for ( i in 1:col_L ){
    DvT[[i]] = ggplot(data=df)+
      geom_point(aes_string(x="dtp",y=column[i],color="cName"),size=1)+theme_classic()+
      ylab(ylabs[i])+xlab("Date (M-D Hr)")+scale_x_datetime(date_labels="%m-%d %H",date_breaks="10 hours")+scale_color_discrete(name="Deployment")
  }
  return(DvT)
}

#currently just serially creates names following the format of "WaterBear-X" with X ranging 1 to the number of unique deployments in the data
createNamesTable <- function(df){
  unqDeploy = unique(df['deployment'])
  for(i in 1:length(unqDeploy[,1])){
    unqDeploy$cName[i] = paste("WaterBear-",i,sep="")
  }
  return(unqDeploy)
}

### flagging errors ###
# no data values, values that should be nan?
# gross min/max limits:
# battery.v, ch4rf_raw, ch4_raw [0, 4096] unitless
# dht_C [ -30, 50 ] celsius
# dht_R [0, 100] percent
# time.s [ 2000, 2024 ]
# overall range check: based on meta data, in and out of range?

# limits set by "grossLimits" in global conditions
grossErrors <- function(df){
  set = 1
  for(col in colMetrics){
    min = grossLimits[set]
    max = grossLimits[set+1]
    df[,col][ df[,col] < min | df[,col] > max] <- NaN
    set = set + 2
  }
  return(df)
}

# write the QC results for an experiment to the provided path .txt
# cumulative NaN counts
# returns processed df
writeQC <- function(df, path){
  cleanFile(path)
  sondes <- as.character(unlist(unique(df['cName'])))
  qcSummary <- data.frame(row.names=sondes)
  qcStep <- c('NaN','grossError')
  for(step in qcStep){
    for(wb in sondes){
      for(col in colMetrics){
        wbSubset <- df[ which(df$cName == wb),]
        name <- paste(sep=".",col,step)
        qcSummary[wb,name] <- sum(is.nan(wbSubset[,col]))
      }
    }
    if(step == qcStep[1]){
      df <- grossErrors(df) # process gross errors in dataframe #
    }
    else if(step == qcStep[2]){
      # perform next QC process, currently there is none. #
    }
  }
  qcSummary$sonde <- sondes
  qcSummary <- relocate(qcSummary, sonde)
  write.table(qcSummary, file=path, row.names=FALSE, sep="\t")
  return(df)
}

#calculate statistics for one column of data
calculateMetrics <- function(x){
  c(count=length(x), max=max(x), min=min(x), mean=mean(x), median=median(x), stdev=sd(x))
}

# write the metrics for an experiment to the provided path .txt
writeMetrics <- function(df, path){
  cleanFile(path)
  sondes <- as.character(unlist(unique(df['cName'])))
  for (i in 1:length(colMetrics)){
    x = as.formula(paste(colMetrics[i],"~cName"))
    rnd = colRnd[i]
    metrics = aggregate(x, data=df, FUN=calculateMetrics, na.action=na.omit)
    metricRnd = round(metrics[2],rnd)
    # y = colnames(metricRnd[])
    # z = colnames(metricRnd[,])
    metricRnd$sonde <- sondes
    metricRnd <- metricRnd[,c('sonde',colMetrics[i])]
    write.table(metricRnd, file=path, append=TRUE, row.names=FALSE, sep="\t")
    write("", file=path, append=TRUE)
  }
}
# writeMetrics(ex3_df_dt, ex3_metadata_dest)

### Process experiment data
#process experiment 1
ex1_df <- merge_ex_csvs(ex1_path)
ex1_df_dt <- process_time(ex1_df)
ex1_lookupTable <- createNamesTable(ex1_df_dt)
ex1_df_dt$cName <- ex1_lookupTable$cName[match(ex1_df_dt$deployment, ex1_lookupTable$deployment)]
ex1_df_dt$cName <- as.factor(ex1_df_dt$cName)

#process experiment 2
ex2_df <- merge_ex_csvs(ex2_path)
ex2_df_dt <- process_time(ex2_df)
ex2_lookupTable <- createNamesTable(ex2_df_dt)
ex2_df_dt$cName <- ex2_lookupTable$cName[match(ex2_df_dt$deployment, ex2_lookupTable$deployment)]
ex2_df_dt$cName <- as.factor(ex2_df_dt$cName)

#process experiment 3
ex3_df <- merge_ex_csvs(ex3_path)
ex3_df_dt <- process_time(ex3_df)
ex3_lookupTable <- createNamesTable(ex3_df_dt)
ex3_df_dt$cName <- ex3_lookupTable$cName[match(ex3_df_dt$deployment, ex3_lookupTable$deployment)]
ex3_df_dt$cName <- as.factor(ex3_df_dt$cName)

# QC data
ex1_df_dt_QC <- writeQC(ex1_df_dt, ex1_qc_dest)
ex2_df_dt_QC <- writeQC(ex2_df_dt, ex2_qc_dest)
ex3_df_dt_QC <- writeQC(ex3_df_dt, ex3_qc_dest)

### create and write meta data ###
writeMetrics(ex1_df_dt_QC, ex1_metadata_dest)
writeMetrics(ex2_df_dt_QC, ex2_metadata_dest)
writeMetrics(ex3_df_dt_QC, ex3_metadata_dest)

### create plots ###
#experiment 1 plot lists
ex1_individual_plots <- plot_Y_v_Time(ex1_df_dt_QC)
ex1_deployment_plots <- plot_Deployment_v_Time(ex1_df_dt_QC)

#experiment 2 plot lists
ex2_individual_plots <- plot_Y_v_Time(ex2_df_dt_QC)
ex2_deployment_plots <- plot_Deployment_v_Time(ex2_df_dt_QC)

#experiment 3 plot lists
ex3_individual_plots <- plot_Y_v_Time(ex3_df_dt_QC)
ex3_deployment_plots <- plot_Deployment_v_Time(ex3_df_dt_QC)


### write plots to pdf ###
#open PDF
writePDF <- function(ex_ip, ex_dp, path){
  cleanFile(path)

  pdf(file=path, onefile=TRUE)
  for (plots in ex_ip){
    individualGrid <- ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend=TRUE)
    print(individualGrid)
  }
  deploymentGrid <- ggarrange(plotlist=ex1_deployment_plots, ncol=2, nrow=3, common.legend=TRUE)
  print(deploymentGrid)

  dev.off()
}

writePDF(ex1_individual_plots,ex1_deployment_plots, ex1_pdf_dest)
writePDF(ex2_individual_plots,ex2_deployment_plots, ex2_pdf_dest)
writePDF(ex3_individual_plots,ex3_deployment_plots, ex3_pdf_dest)

### save ggarranged plots as individual images ###
savePlots <- function(ex_ip, ex_dp, path){
  i = 1
  for (plot in ex_ip){
    pngPath = paste(sep="",path,"/",column[i],".png")
    individualGrid <- ggarrange(plotlist=plot, ncol=2, nrow=2, common.legend=TRUE)
    png(file=pngPath,width=595,height=595)
    print(individualGrid)
    dev.off()
    i = i + 1
  }
  pngPath = paste(sep="",path,"/experiment.png")
  deploymentGrid <- ggarrange(plotlist=ex1_deployment_plots, ncol=2, nrow=3, common.legend=TRUE)
  png(file=pngPath,width=595,height=842) # A4 paper dimensions at 72 DPI
  # png(file=pngPath,width=2480,height=3508) # A4 paper dimensions at 300 DPI
  print(deploymentGrid)
  dev.off()
}

savePlots(ex1_individual_plots, ex1_deployment_plots, ex1_path)
savePlots(ex2_individual_plots, ex2_deployment_plots, ex2_path)
savePlots(ex3_individual_plots, ex3_deployment_plots, ex3_path)

##### I need a function that checks timestamp intervals based on settings #####
# interval 60 min
# burst number: 20
# start-up delay: 0 min
# inter-burst delay: 1 min
# burst size (readings): 10


#return a ~1hr clip from the middle of the dataset
clipData <- function(df){
  maxTime = max(df$time.s)
  minTime = min(df$time.s)
  middle = (maxTime - minTime) / 2
  halfHour = 30 * 60 # 30min * 60seconds/minute
  start = minTime + middle - halfHour
  end = minTime + middle + halfHour
  
  clip <- df[ which(df$time.s > start & df$time.s < end), ]
  return(clip)
}
ex3_clip <- clipData(ex3_df_dt_QC)

resampleData <- function(df){
  df$sec <- round(df$time.s,0)
  sondes <- as.character(unlist(unique(df['cName'])))
  df_unique <- unique(df[ , c("cName", "sec")])
  # print(str(df_unique))
  # print(length(df_unique$sec))
  for(i in 1:length(df_unique$sec)){
    sonde = as.character(unlist(df_unique[i,][1]))
    time = as.numeric(df_unique[i,][2])
    # print(toString(unlist(sonde)))
    # print(sonde)
    # print(class(sonde))
    # print(time)
    # print(class(time))
    subset <- df[ which(df$cName == sonde & df$sec == time) ]
    print(subset)
    # df_unique[i,resample] <-
    break
  }
  return(df_unique)
}

ex3_resample <- resampleData(ex3_clip)

ex3_clip$sec <- round(ex3_clip$time.s,0)
ex3_clip %>%
  group_by(cName,sec) %>%
  mean = mean(battery.V)



