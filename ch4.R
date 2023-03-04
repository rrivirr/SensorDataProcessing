# cloned from plastidipTests.R on Nov 7th, 2022 by Ken Chong
# Last edit: Nov 7th, 2022 by Ken Chong
#
# Code to process WaterBear data containing output from one Figaro NGM2611-E13 methane sensor,
# and one Adafruit DHT22 temperature and relative humidity sensor
#
# note: battery.V only works if a power pin has been wired into exADC sensor port 3 through 1 million ohm resistor currently
#
# note to self: ctrl+shift+c to comment toggle line(s)

### Required Packages ###
library(dplyr) # bind, relocate
library(lubridate) # as_datetime, hour, date
library(ggplot2) # plotting package
library(ggpubr) # ggarrange
library(caTools)
library(reshape2)
# library(grid) # textGrob

library(caTools)
library(reshape2)

### garbage collection lines ###
rm(list = ls()) # clear R working memory
graphics.off() # close any open plots

# custom color blind color palettes:
# https://davidmathlogic.com/colorblind/#%237000CC-%23E22C7B-%23FFAF2A-%2300CACA-%23046CC5-%23CB60E6-%23349634-%23E47D00
custom_colors=c('#7000CC','#E22C7B','#FFAF2A','#00CACA','#046CC5','#CB60E6','#349634','#E47D00') #00AAAA

### R Options ###
options('digits'=15) # default is 7, increasing to show full precision of numbers (mainly time.s needs at least 13)

### R SCRIPT SETTINGS ###
# path to folder with main data folders in it
# ! THIS IS THE ONLY THING THAT NEEDS TO BE CHANGED IF ALL DATAFOLDERS ARE THERE ! #
wd = "~/Desktop/data/mesoBinTests" #linux path #HARDCODED
# wd = "/Documents/GitHub/data/" #windows path #HARDCODED


# specific experiment folder to process
# pt2 = "plastidipTest2" #HARDCODED 
    #search "pt2" and replace with "experiment", script only process one experiment's data at a time
experiment = "20221104" #HARDCODED

### R FUNCTIONS ###

# check if file at path exists, delete if it does
# will only call in functions that generate new files
cleanFile <-function(path){
  if(file.exists(path)){
    cat(sprintf("Deleting old file: %s\n",path))
    file.remove(path)
  }
}

# check if directory exists, create if not
newDir <- function(dirPath){
  if(dir.exists(dirPath)){
    cat(sprintf("Output directory, %s, already exists\n", dirPath))
  }
  else{
    cat(sprintf("Output directory not found, creating: %s\n",dirPath))
    dir.create(dirPath)
  }
}

#iterate folders in experiment directory, get list of all files in each folder and then append them
#NOTE: does not remove "debug" lines atm, didn't have any at the time
merge_ex_csvs <- function(exDir){
  # data_dirs <- list.files(path=exDir, pattern="ABElab*", all.files=FALSE) # pull all dirs starting with ABElab
  data_dirs <- list.files(path=exDir, all.files=FALSE) # pull all dirs
  compiled_data<-data.frame() #prepare data frame to hold info from all csv files

  #iterate folders in working directory
  for(i in 1:length(data_dirs)){
    currentdir<-paste(exDir,"/",data_dirs[i],sep="")
    data_files<-list.files(path=paste(currentdir)) #find all files in sub directory

    # read each .csv
    for(j in 1:length(data_files)){
      currentfile <- paste(currentdir,"/",data_files[j],sep="")
      if(file.info(currentfile)[,1] != 0){ # skip files that are size 0
        data<-read.csv(currentfile)

        ### manually correcting df column type here, as it will fail to merge if mixed types
        # ie if site is given a numerical designation rather than a character string
        data$site <- as.character(data$site)

        if(nrow(data)>0){ #append all csv data into one file
          compiled_data<-bind_rows(compiled_data,data)
        }
      }
    }
  }
  return(compiled_data)
}

# #### jake's code ####
# cd <- merge_ex_csvs(paste(sep="", wd, pt2))
# 
# raw_bin_21008<-subset(cd,logger=="21008")
# 
# ## used to make hourly groups
# raw_bin_21008$dtp<-as.POSIXct(raw_bin_21008$time.s, origin="1970-01-01")
# raw_bin_21008$time.s_lag1<-lag(raw_bin_21008$time.s,1)
# raw_bin_21008$time_diff<-raw_bin_21008$time.s-raw_bin_21008$time.s_lag1
# raw_bin_21008$group<-1
# group_count<-1
# for(i in 2:nrow(raw_bin_21008)){
#   if(raw_bin_21008$time_diff[i]>100){
#     group_count<-group_count+1
#     raw_bin_21008$group[i:nrow(raw_bin_21008)]<-group_count
#   }
# }
# 
# #subset raw data type
# raw_bin_21008<-subset(raw_bin_21008,type=="raw")
# 
# #test plot
# ggplot(raw_bin_21008,aes(dtp,ch4_raw))+
#   geom_point(size=2,aes(color=group))
# 
# # generate a basic plot, and then a plot showing the running mean, standard deviation, and correlation of variance
# report<-data.frame()
# for(j in 1:max(raw_bin_21008$group)){
#   group_sub<-subset(raw_bin_21008,group==j)
#   gg<-ggplot(group_sub,aes(time.s,ch4_raw))+
#     geom_point(size=2)
#   # +scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
#   
#   print(gg)
#   
#   group_sub$runmean = runmean(group_sub$ch4_raw, 10, endrule="mean")
#   group_sub$runsd = runsd(group_sub$ch4_raw, 10, endrule="sd")
#   group_sub$runcv<-group_sub$runsd/group_sub$runmean
#   
#   invisible(readline(prompt="Press [enter] to continue"))
#   
#   gsmelt<-melt(group_sub[,c("time.s","runmean","runsd","runcv","ch4_raw")],id.vars=c("time.s"))
#   meltplot<-ggplot(gsmelt,aes(time.s,value))+
#     geom_point(size=2,aes(color=variable))+
#     facet_wrap(.~variable,scales="free_y",ncol=1)
#   
#   print(meltplot)
#   
#   
#   temp_report<-data.frame(group=j,min_cv=min(group_sub$runcv),min_sd=min(group_sub$runsd),first_cv_below_005=min(which(group_sub$runcv<0.005)))
#   report<-bind_rows(report,temp_report)
#   invisible(readline(prompt="Press [enter] to continue"))
#   
# }

#convert time.s to datetime column, split into hour and date, create a new hourly factor, change other columns to factors
process_columns <- function(compiled_data){
  compiled_data$datetime<-lubridate::as_datetime(compiled_data$time.s) # turn epoch to date time
  compiled_data$date<-date(compiled_data$datetime) # extract dates
  compiled_data$hour<-hour(compiled_data$datetime) # extract hours
  compiled_data$minute<-minute(compiled_data$datetime) # extract minutes
  compiled_data$second<-round(compiled_data$time.s, 0) # round off epoch to remove milliseconds (used for resampling later)
  
  # if using external ADC for battery value
  adcReference = 5.4 * 1000 #5.4V * 1000mV/V
  adcResolution = 4095 # 12bit adc
  compiled_data$battery.mV<-compiled_data$battery.V*(adcReference/adcResolution)

  #reinterpret columns as factors
  compiled_data$datehour<-as.factor(format.Date(compiled_data$datetime, format="%Y-%m-%d %H")) # measurement cycle
  compiled_data$datehourmin<-as.factor(format.Date(compiled_data$datetime, format="%Y-%m-%d %H:%M")) # reading cycle
  compiled_data$type<-as.factor(compiled_data$type)
  compiled_data$site<-as.factor(compiled_data$site)
  compiled_data$logger <- as.factor(compiled_data$logger)
  compiled_data$deployment<-as.factor(compiled_data$deployment)
  compiled_data$uuid<-as.factor(compiled_data$uuid)

  return(compiled_data)
}

# process dataframe gross erorrs into NaNs and return
grossErrors <- function(df, colMetrics){
  # TODO: non hardcoded limits using extra header info about sensor slot configuration?
  # hardcoded gross error limits per column (min, max)
  # time.s, battery.v, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal, dht_c, dht_rh
  timeMin <- as.numeric(as.POSIXct("2000-01-01")) #HARDCODED
  timeMax <- as.numeric(as.POSIXct("2100-01-01")) #HARDCODED
  grossLimits = c(timeMin,timeMax,0,4096,0,4096,0,4096,0,4096,0,4096,-30,50,0,100) #HARDCODED

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
# note: cumulative NaN counts
# returns processed df
qualityControl <- function(df, path, filePrefix, loggers, colMetrics){
  path <- paste(sep="", path, filePrefix, "_qc.txt")

  # delete old output QC file if necessary
  cleanFile(path)

  qcSummary <- data.frame(row.names=loggers)
  qcStep <- c('NaN', 'grossError')
  for(step in qcStep){
    for(wb in loggers){
      for(col in colMetrics){
        wbSubset <- df[ which(df$logger == wb),]
        name <- paste(sep=".",col,step)
        qcSummary[wb,name] <- sum(is.nan(wbSubset[,col]))
        qcSummary$logger <- wb #TODO, does this work?
      }
    }
    if(step == qcStep[1]){
      df <- grossErrors(df, colMetrics) # process gross errors in dataframe #
    }
    else if(step == qcStep[2]){
      # perform next QC process, currently there is none. #
    }
  }
  # qcSummary$logger <- loggers #TODO, this is not lining up correctly, consider populating this column within loop
  qcSummary <- relocate(qcSummary, logger)
  write.table(qcSummary, file=path, row.names=FALSE, sep="\t")
  return(df)
}

#calculate statistics for one column of data
calculateMetrics <- function(x){
  c(count=length(x), max=max(x), min=min(x), mean=mean(x), median=median(x), stdev=sd(x))
}

# write the metrics for an experiment to the provided path .txt
writeMetrics <- function(df, path, filePrefix, loggers, cols_metrics){
  path <- paste(sep="", path, filePrefix, "_metrics.txt")
  # delete old metric data file, if any
  cleanFile(path)

  # TODO: non hardcoded rouunding using extra header info about sensor slot configuration? Or identifying from raw values
  # hardcoded decimal digits to round to for each column
  # time.s, battery.v, dht_c, dht_rh, atlas_CO2_ppm, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal
  colRnd = c(3, 1, 1, 1, 0, 0, 0, 0, 0) #HARDCODED

  for (i in 1:length(cols_metrics)){
    x = as.formula(paste(cols_metrics[i],"~logger"))
    rnd = colRnd[i]
    metrics = aggregate(x, data=df, FUN=calculateMetrics, na.action=na.omit)
    metricRnd = round(metrics[2],rnd)
    metricRnd$logger <- loggers
    metricRnd <- metricRnd[,c('logger',cols_metrics[i])]
    write.table(metricRnd, file=path, append=TRUE, row.names=FALSE, sep="\t")
    write("", file=path, append=TRUE)
  }
}

writeMetrics(df_raw_qc, outputDir_raw, "raw", loggers, cols_metrics)

# create list of hourly plots of each metric from dataframe, save them as pngs and return list
hourlyPlotList <- function(df, cols_plots, ylabs){
  col_L = length(cols_plots)
  
  # initialize empty list of lists to hold plots
  hPL <- vector('list', col_L)
  names(hPL) = cols_plots
  hours <- sort(unique(df[['datehour']]))
  for(i in 1:col_L){
    ## in case a column does not have the same amount of unique hours of data as another
    # colData <- data.frame(df[cols_plots[i]], df['datehour'])
    # hours <- unique(colData[['datehour']])
    hPL[[ cols_plots[i] ]] <- vector('list', length(hours))
    names( hPL[[ cols_plots[i] ]] ) = hours
  }
  
  # loop through and plot each hour of data for each metric
  for(i in 1:col_L){
    ## in case a column does not have the same amount of unique hours of data as another
    # colData <- data.frame(df[cols_plots[i]], df['datehour'])
    # hours <- unique(colData[['datehour']])
    for(j in 1:length(hours)){
      #subset, create plot, and store in list
      hourSubset <- df[df$datehour == hours[j] ,]
      hPL[[ cols_plots[i] ]][[ hours[j] ]] = ggplot(data=hourSubset,aes_string(x="datetime",y=cols_plots[i],color="logger",shape="site"))+
        geom_point(size=2)+geom_line(size=1)+theme_classic(base_size=18)+
        labs(x="Time (hr:min)", y=ylabs[i], color="Logger")+
        scale_color_manual(values=custom_colors,na.translate=F)+
        scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
      # expand is adding 60 sec * 1.5 to each side of the scale [the base unit for datetime?]
    }
  }
  return(hPL)
}

# input a list of lists of hourly plots per metric
# output ggarrange and save a png containing each hourly plot for each metric
saveArrHourlyPlotsPerMetric <- function(hourlyPlots, cols_plots, outputDir, filePrefix=""){
  n = round(sqrt(length(hourlyPlots[[ cols_plots[1] ]])))
  sqPixels = n * 800

  # loop through and ggarrange each metric
  for(i in 1:length(cols_plots)){
    cat(sprintf("ggarrange and save: %s\n", cols_plots[i]))
    toSave <- ggarrange(ncol=n, nrow=n, plotlist=hourlyPlots[[ cols_plots[i] ]], common.legend=TRUE)
    pngPath = paste(sep="",outputDir,filePrefix,cols_plots[i],".png")

    png(file=pngPath,width=sqPixels,height=sqPixels)
    print(toSave)
    dev.off()
  }
}

#input list of plots, output directory, custom directory path or tag
#output save each plot to output directory
#800 is a little over 5" on my screen 1920x1080
savePlotList <-function(plotList, outputDir, tag="", width=800, height=800){
  plots <- names(plotList)
  for(i in 1:length(plots)){
    pngPath = paste(sep="",outputDir,tag,plots[i],".png")
    png(file=pngPath,width=width,height=height)
    print(plotList[[ plots[i] ]])
    dev.off()
  }
}

######### TESTING running mean, sd, cv report #########
# note, split into two functions, one to process data by the minute/reading cycle, for each logger, then one to create the melt plots

# process each reading cycle per logger for the entire data set, this is assuming a reading cycle falls within a minute
# TODO: change to group by every 10 raw readings instead or 11, including the summary reading [or, each unique summary reading represents a burst?]
# would not account for lack of overlap or each logger
# note: not resampled data
processReadingCycles <- function(df, loggers, colsReport){
  readingCycles <- unique(df['datehourmin'])
  
  outputCols <- data.frame()
  report <- data.frame()
  for(wb in loggers){
    wb_sub <- df[ which(df$logger == wb),]
    for(r in readingCycles){
      rc_sub <- wb_sub[which(df$datehourmin == rc)]
      
      #calculate running mean, standard deviation, and coefficient of variance into new columns
      k = length(rc_sub[1,])
      rc_sub$runmean = runmean(rc_sub$ch4_raw, k, endrule="mean")
      rc_sub$runsd = runsd(rc_sub$ch4_raw, k, endrule="sd")
      rc_sub$runcv <- rc_sub$runsd/rc_sub$runmean
      
      #add minimum values to output report dataframe
      temp_report<-data.frame(group=j,min_cv=min(group_sub$runcv),min_sd=min(group_sub$runsd),first_cv_below_005=min(which(group_sub$runcv<0.005)))
      report<-bind_rows(report,temp_report)
    }
  }
}

# for(step in qcStep){
#   for(wb in loggers){
#     for(col in colMetrics){
#       wbSubset <- df[ which(df$logger == wb),]
#       name <- paste(sep=".",col,step)
#       qcSummary[wb,name] <- sum(is.nan(wbSubset[,col]))
#       qcSummary$logger <- wb #TODO, does this work?
#     }
#   }

burstPlotList <- function(df, cols_report, outputDir, filePrefix=""){
  # #dataframe to hold final plots
  reportDF<-data.frame()

  for(j in 1:max(raw_bin_21008$group)){
  #subset dataframe by group
  group_sub<-subset(raw_bin_21008,group==j)

  #calculate running mean, standard deviation, and coefficient of variance into new columns
  group_sub$runmean = runmean(group_sub$ch4_raw, 10, endrule="mean")
  group_sub$runsd = runsd(group_sub$ch4_raw, 10, endrule="sd")
  group_sub$runcv<-group_sub$runsd/group_sub$runmean

  #reorganize data into long format, based on time.s as unique identities (time.s is raw time unix and should have milliseconds)
  gsmelt<-melt(group_sub[,c("time.s","runmean","runsd","runcv","ch4_raw")],id.vars=c("time.s"))

  # create plots vs time for running mean, standard deviation, and correlation of variance vs time.s, then display
  meltplot<-ggplot(gsmelt,aes(time.s,value))+	geom_point(size=2,aes(color=variable))+
    facet_wrap(.~variable,scales="free_y",ncol=1)
  print(meltplot)

  #add data to output dataframe
  temp_report<-data.frame(group=j,min_cv=min(group_sub$runcv),min_sd=min(group_sub$runsd),first_cv_below_005=min(which(group_sub$runcv<0.005)))
  reportDF<-bind_rows(report,temp_report)
  }

  returns <- list(meltPlotList, reportDF)
  
  return(returns)
}


# ## HARCODED ##
# # user settings based on data from header
# # user provided columns and rounding
# # resample data based on 10 readings per burst, 1 burst per minute (1 minute delay) [relies on burst taking less than a minute]
# resampleData <- function(df, colResample, colRnd){
#   #only works with the 10 reading burst taking under a second, maybe just average based on every 10 readings / burst size in the future
#   grouped <- group_by(df, logger, datehour, minute)
# 
#   #TODO: hardcoded names and columns, base off of colMetrics in the future
#   means <- summarise(grouped,
#                      datetime = mean(datetime),
#                      battery.V = round( mean(battery.V), colRnd[2] ),
#                      ch4rf_raw = round( mean(ch4rf_raw), colRnd[3] ),
#                      ch4rf_cal = round( mean(ch4rf_cal), colRnd[4] ),
#                      ch4_raw = round( mean(ch4_raw), colRnd[5] ),
#                      ch4_cal = round( mean(ch4_cal), colRnd[6] ),
#                      dht_C = round( mean(dht_C), colRnd[7] ),
#                      dht_RH = round( mean(dht_RH), colRnd[8] )
#                     )
#   # means <- summarise(grouped, mean(battery.V), mean(ch4rf_raw), mean(ch4_raw), mean(dht_C), mean(dht_RH))
#   means_df <- data.frame(means)
# 
#   #add site column back into data frame
#   output <- merge(x=means_df, y=unique(df[ , c("logger","site") ]), by="logger", sort=FALSE)
#   return(output)
# }

### Main function to execute full processing script
# include basic plotting?
processData <- function(workingDir, experimentFolderName){
  # generate paths for output directories
  exPath = paste(sep="",workingDir,"/",experimentFolderName)
  outputDir = paste(sep="",exPath,"_output/")
  outputDir_raw = paste(sep="",outputDir,"raw/")
  outputDir_summary = paste(sep="",outputDir,"summary/")

  outputDir_exName = paste(sep="",outputDir,experimentFolderName)

  # create output folders if necessary
  newDir(outputDir)
  newDir(outputDir_raw)
  newDir(outputDir_summary)

  print("Processing Data")
  # merge experiment data in csv format to dataframe
  exDf <- merge_ex_csvs(exPath)

  # process dataframe columns and split time to more columns
  exDf <- process_columns(exDf)

  # list of columns in dataframe
  cols <- names(exDf)
  # subset list of columns to calculate metrics for, time.s, and all columns between time.h and user_note
  cols_metrics <- append(cols[7], cols[(grep("time.h",cols)+1):(grep("user_note",cols)-1)])
  # list of columns to plot against time ('datetime'), all columns between time.h and user_note
  cols_plots <- cols[(grep("time.h",cols)+1):(grep("user_note",cols)-1)]

  # hardcoded column labels
  # TODO: use header settings to customize? or always manual?
  # battery.v, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal, dht_c, dht_rh
  ylabs = c('Battery Digital\nReading (12bit)', 'Methane Reference\nDigital Reading (12bit)', 
            'Methane Reference\n Calibrated\nDigital Reading (12bit)', 'Methane Digital\n Reading (12bit)',
            'Methane Digital\n Calibrated Reading (12bit)', '\nTemperature (°C)', '\nRelative Humidity (%)') #HARDCODED

  # alphabetized list of loggers involved in experiment
  loggers <- sort(as.character(unlist(unique(exDf['logger']))))

  # # ADC conversion values
  # maxBinCount = 4096 # 0 - 4095
  # inputVoltage = 5 #volts #TODO: find specs or measure, 5v not exact

  # subset dataframe for raw and summary types
  df_raw <- exDf[ exDf[['type']] == 'raw' ,]
  # df_summary <- exDf[ exDf[['type']] == 'summary' ,]

  print("Processing quality control")
  # write quality control results and process df
  df_raw_qc <- qualityControl(df_raw, outputDir_raw, "raw", loggers, cols_metrics)
  # df_summary_qc <- qualityControl(df_summary, outputDir_summary, "summary", loggers, cols_metrics)

  print("Writing metadata")
  # write preliminary metadata for the data
  writeMetrics(df_raw_qc, outputDir_raw, "raw", loggers, cols_metrics)
  # writeMetrics(df_summary_qc, outputDir_summary, "summary", loggers, cols_metrics)

  # create and save basic hourly plots
  print("Creating Hourly Plots")
  hourlyPlotList_raw <- hourlyPlotList(df_raw_qc, cols_plots, ylabs)
  # hourlyPlotList_summary <- hourlyPlotList(df_summary_qc, cols_plots, ylabs)
  print("Saving Individual Hourly Plots:")
  for(i in 1:length(cols_plots)){
    print(cols_plots[i])

    #create subdirectories for each metric
    od_raw = paste(sep="",outputDir_raw,cols_plots[i],"/")
    od_summary = paste(sep="",outputDir_summary,cols_plots[i],"/")
    newDir(od_raw)
    # newDir(od_summary)

    savePlotList(hourlyPlotList_raw[[ cols_plots[i] ]], od_raw)
    # savePlotList(hourlyPlotList_summary[[ cols_plots[i] ]], od_summary)
    
  }

  print("Save Arranged Hourly Plots:")
  print("Raw")
  saveArrHourlyPlotsPerMetric(hourlyPlotList_raw, cols_plots, outputDir_raw, "raw_grid_")
  # print("Summary")
  # saveArrHourlyPlotsPerMetric(hourlyPlotList_summary, cols_plots, outputDir_summary, "summary_grid_")

  # returns <- list(df_raw_qc, df_summary_qc, hourlyPlotList_raw, hourlyPlotList_summary)
  returns <- list(df_raw_qc, hourlyPlotList_raw)

  print("Done")
  return(returns)
}

###### TESTING processData #######
# generate paths for output directories
workingDir = wd
experimentFolderName = experiment
exPath = paste(sep="",workingDir,"/",experimentFolderName)
outputDir = paste(sep="",exPath,"_output/")
outputDir_raw = paste(sep="",outputDir,"raw/")
outputDir_summary = paste(sep="",outputDir,"summary/")

outputDir_exName = paste(sep="",outputDir,experimentFolderName)

# create output folders if necessary
newDir(outputDir)
newDir(outputDir_raw)
newDir(outputDir_summary)

print("Processing Data")
# merge experiment data in csv format to dataframe
exDf <- merge_ex_csvs(exPath)

# process dataframe columns and split time to more columns
exDf <- process_columns(exDf)

# list of columns in dataframe
cols <- names(exDf)
# subset list of columns to calculate metrics for, time.s, and all columns between time.h and user_note
cols_metrics <- append(cols[7], cols[(grep("time.h",cols)+1):(grep("user_note",cols)-1)])
# list of columns to plot against time ('datetime'), all columns between time.h and user_note
cols_plots <- cols[(grep("time.h",cols)+1):(grep("user_note",cols)-1)]

# hardcoded column labels
# TODO: use header settings to customize? or always manual?
# or create a mapping for embedded names to custom names? or allow user to make them on the fly
# battery.v, dht_c, dht_rh, atlas_CO2_ppm, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal
ylabs = c('Battery Digital\nReading (12bit)','\nTemperature (°C)', '\nRelative Humidity (%)','CO2 (ppm)', 
          'Methane Reference\nDigital Reading (12bit)', 'Methane Reference\n Calibrated\nDigital Reading (12bit)', 
          'Methane Digital\n Reading (12bit)', 'Methane Digital\n Calibrated Reading (12bit)') #HARDCODED
# alphabetized list of loggers involved in experiment
loggers <- sort(as.character(unlist(unique(exDf['logger']))))

# # ADC conversion values
# maxBinCount = 4096 # 0 - 4095
# inputVoltage = 5 #volts #TODO: find specs or measure, 5v not exact

# subset dataframe for raw and summary types
df_raw <- exDf[ exDf[['type']] == 'raw' ,]
df_summary <- exDf[ exDf[['type']] == 'summary' ,]

print("Processing quality control")
# write quality control results and process df
df_raw_qc <- qualityControl(df_raw, outputDir_raw, "raw", loggers, cols_metrics)
df_summary_qc <- qualityControl(df_summary, outputDir_summary, "summary", loggers, cols_metrics)

print("Writing metadata")
# write preliminary metadata for the data
writeMetrics(df_raw_qc, outputDir_raw, "raw", loggers, cols_metrics)
writeMetrics(df_summary_qc, outputDir_summary, "summary", loggers, cols_metrics)

# create and save basic hourly plots
print("Creating Hourly Plots")
hourlyPlotList_raw <- hourlyPlotList(df_raw_qc, cols_plots, ylabs)
# hourlyPlotList_summary <- hourlyPlotList(df_summary_qc, cols_plots, ylabs)
print("Saving Individual Hourly Plots:")
for(i in 1:length(cols_plots)){
  print(cols_plots[i])
  
  #create subdirectories for each metric
  od_raw = paste(sep="",outputDir_raw,cols_plots[i],"/")
  od_summary = paste(sep="",outputDir_summary,cols_plots[i],"/")
  newDir(od_raw)
  # newDir(od_summary)
  
  savePlotList(hourlyPlotList_raw[[ cols_plots[i] ]], od_raw)
  # savePlotList(hourlyPlotList_summary[[ cols_plots[i] ]], od_summary)
  
}

print("Save Arranged Hourly Plots:")
print("Raw")
saveArrHourlyPlotsPerMetric(hourlyPlotList_raw, cols_plots, outputDir_raw, "raw_grid_")
# print("Summary")
# saveArrHourlyPlotsPerMetric(hourlyPlotList_summary, cols_plots, outputDir_summary, "summary_grid_")

# returns <- list(df_raw_qc, df_summary_qc, hourlyPlotList_raw, hourlyPlotList_summary)
returns <- list(df_raw_qc, hourlyPlotList_raw)


##### MAIN #####
# do everything. return list of items:
system.time(experiment_returns <- processData(wd, experiment))

# unpack returns
#returns <- list(df_raw_qc, df_summary_qc, hourlyPlotList_raw, hourlyPlotList_summary)
experiment_df_raw_qc <- experiment_returns[[1]]
# pt2_df_summary_qc <- pt2_returns[[2]]
experiment_hourlyPlotList_raw <- experiment_returns[[2]]
# pt2_hourlyPlotList_summary <- pt2_returns[[4]]

## for each logger, for each minute, calculate mean, sd, and cv (sd/mean)
## note: group is each individual burst, this is for logger 21008
# report<-data.frame()
# for(j in 1:max(raw_bin_21008$group)){
#   group_sub<-subset(raw_bin_21008,group==j)
#   group_sub$runmean = runmean(group_sub$ch4_raw, 10, endrule="mean")
#   group_sub$runsd = runsd(group_sub$ch4_raw, 10, endrule="sd")
#   group_sub$runcv<-group_sub$runsd/group_sub$runmean
#   
#   gsmelt<-melt(group_sub[,c("time.s","runmean","runsd","runcv","ch4_raw")],id.vars=c("time.s"))
#   meltplot<-ggplot(gsmelt,aes(time.s,value))+
#     geom_point(size=2,aes(color=variable))+
#     facet_wrap(.~variable,scales="free_y",ncol=1)
#   
#   temp_report<-data.frame(group=j,min_cv=min(group_sub$runcv),min_sd=min(group_sub$runsd),first_cv_below_005=min(which(group_sub$runcv<0.005)))
#   report<-bind_rows(report,temp_report)
# }

##### POST PROCESSING ##### HARDCODED

# generate paths for output directories
exPath = paste(sep="",wd,"/",experiment)
outputDir = paste(sep="",exPath,"_output/")

# TODO: non hardcoded rouunding using extra header info about sensor slot configuration? Or identifying from raw values
# hardcoded decimal digits to round to for each column
# time.s, battery.v, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal, dht_c, dht_rh
colRnd = c(3, 0, 0, 0, 0, 0, 1, 1) #HARDCODED

# calculate difference between points for each hour
# x2-x1 = y2, y1 = 0, until !x2

### working on this data frame ###
df = experiment_df_raw_qc

# alphabetized list of loggers involved in experiment
loggers <- sort(as.character(unlist(unique(df['logger']))))

cols <- names(df)
# list of columns to plot against time ('datetime'), all columns between battery.V and user_note
cols_diff <- cols[(grep("battery.V",cols)+1):(grep("user_note",cols)-1)]

# resample each minute of data per logger per hour
resample_df <- resampleData(df, cols_diff, colRnd)

# Calculate diff for each hour, for each relevant column, and add to dataframe
loggers <- sort(unique(resample_df$logger))
hours <- sort(unique(resample_df[['datehour']]))

for (col in 1:length(cols_diff)){
  print(cols_diff[col])
  temp <- data.frame()
  for(log in 1:length(loggers)){
    # print(loggers[log])
    loggerSubset <- resample_df[ resample_df$logger == loggers[log] ,]
    for(hr in 1:length(hours)){
      # print(hours[hr])
      hourSubset<- loggerSubset[ loggerSubset$datehour == hours[hr] ,]
      if(length( hourSubset[[ cols_diff[col] ]]) >0)
      {
        temp <- append(temp, NA_integer_)
        temp <- append(temp, diff(hourSubset[[ cols_diff[col] ]]) )
      }
    }
  }
  newCol <- paste(sep="",cols_diff[col],"_diff")
  resample_df[[ newCol ]] <- unlist(temp)
}

# # single hardcoded plot, all hours
# test <- ggplot(data=df, aes(x=datetime, y=ch4_raw_diff, color=logger, shape=site))+geom_point()+theme_classic()+
#   labs(x="Time (hr:min)", y="CH4", color="Logger", shape="Site")+
#   scale_color_manual(values=custom_colors,na.translate=F)+
#   scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
# test

# hardcoded column labels
# TODO: use header settings to customize? or always manual?
# battery.v, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal, dht_c, dht_rh
ylabs = c('Methane Reference\nDigital Reading (12bit)', 
          'Methane Reference\n Calibrated\nDigital Reading (12bit)', 'Methane Digital\n Reading (12bit)',
          'Methane Digital\n Calibrated Reading (12bit)', '\nTemperature (°C)', '\nRelative Humidity (%)') #HARDCODED

# hourlyPlotList_diff_raw <- hourlyPlotList(df, paste(sep="",cols_diff, "_diff"), ylabs)

writeMetrics(resample_df,outputDir,"raw_resample_diff",loggers,paste(sep="",cols_diff, "_diff"))

ggplot(data=resample_df, aes(x=datetime, y=ch4_raw_diff, color=logger))+geom_point()+theme_classic()+
  labs(x="Time (hr:min)", y="CH4", color="Logger")+
  scale_color_manual(values=custom_colors,na.translate=F)+
  scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))

df_test <- resample_df

# remove 0 values
col = "ch4_raw_diff"
df_test[,col][ df_test[,col] == 0] <- NaN

#NOTE: stopping here to work on case supplies

writeMetrics(df,outputDir,"raw_resample_diff_0isNaN",loggers,paste(sep="",cols_diff, "_diff"))
# 
# ggplot(data=df, aes(x=datetime, y=ch4_raw_diff, color=logger))+geom_point()+theme_classic()+
#   labs(x="Time (hr:min)", y="CH4", color="Logger")+
#   scale_color_manual(values=custom_colors,na.translate=F)+
#   scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
# 
# hourlyPlotList_diff_raw_0isNaN <- hourlyPlotList(df, paste(sep="",cols_diff, "_diff"), ylabs)
# 
# # process df to remove values outside limits using 1 max standard deviation for ch4_raw diff, ~100, +/- 50
# col = "ch4_raw_diff"
# min = -50
# max = 50
# df[,col][ df[,col] < min | df[,col] > max] <- NaN
# 
# writeMetrics(df,outputDir,"raw_diff_100",loggers,paste(sep="",cols_diff, "_diff"))
# 
# ggplot(data=df, aes(x=datetime, y=ch4_raw_diff, color=logger))+geom_point()+theme_classic()+
#   labs(x="Time (hr:min)", y="CH4", color="Logger")+
#   scale_color_manual(values=custom_colors,na.translate=F)+
#   scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
# 
# hourlyPlotList_diff_raw_100 <- hourlyPlotList(df, paste(sep="",cols_diff, "_diff"), ylabs)
# 
# # # process dataframe gross erorrs into NaNs and return
# # grossErrors <- function(df, colMetrics){
# #   # TODO: non hardcoded limits using extra header info about sensor slot configuration?
# #   # hardcoded gross error limits per column (min, max)
# #   # time.s, battery.v, ch4rf_raw, ch4rf_cal, ch4_raw, cr4_cal, dht_c, dht_rh
# #   timeMin <- as.numeric(as.POSIXct("2000-01-01")) #HARDCODED
# #   timeMax <- as.numeric(as.POSIXct("2100-01-01")) #HARDCODED
# #   grossLimits = c(timeMin,timeMax,0,4096,0,4096,0,4096,0,4096,0,4096,-30,50,0,100) #HARDCODED
# #   
# #   set = 1
# #   for(col in colMetrics){
# #     min = grossLimits[set]
# #     max = grossLimits[set+1]
# #     df[,col][ df[,col] < min | df[,col] > max] <- NaN
# #     set = set + 2
# #   }
# #   return(df)
# }
# 
# ##### customize ggplot calls using variables research #####
# 
# # retroactively add geom_line to a ggplot
# test <- ggplot(data=df, aes(x=datetime, y=ch4_raw_diff, color=logger, shape=site))+geom_point()+theme_classic()+
#   labs(x="Time (hr:min)", y="CH4", color="Logger", shape="Site")+
#   scale_color_manual(values=custom_colors,na.translate=F)+
#   scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
# z <- quote(geom_line(size=1))
# test + as.formula(z)
# 
# # use a variable for ggplot call
# ggplot(data=df, aes(x=datetime, y=ch4_raw_diff, color=logger, shape=site))+geom_point()+as.formula(z)+theme_classic()+
#   labs(x="Time (hr:min)", y="CH4", color="Logger", shape="Site")+
#   scale_color_manual(values=custom_colors,na.translate=F)+
#   scale_x_datetime(date_labels="%H:%M",breaks=scales::pretty_breaks(n=4),expand=c(0,60*1.5))
