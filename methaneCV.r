### garbage collection lines ###
rm(list = ls()) # clear R working memory
graphics.off() # close any open plots

# Install and load libraries as necessary
# Package names
packages<- c("dplyr", "doBy", "lubridate", "ggplot2", "Hmisc", "zoo")

# Install packages if necessary
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# Define all the functions used

## Function to delete a file if it exists
# check if file at path exists, delete if it does
# will only call in functions that generate new files
cleanFile <-function(path){
  if(file.exists(path)){
    cat(sprintf("Deleting old file: %s\n",path))
    file.remove(path)
  }
}

## Function to create a new directory if it does not exist
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

## Function to save a list of plots
#input list of plots, output directory, custom directory path or tag
#output save each plot to output directory
#800 is a little over 5" on my screen at 1920x1080
savePlotList <-function(plotList, outputDir, tag="", width=800, height=800){
  plots <- names(plotList)
  for(i in 1:length(plots)){
    pngPath = paste(sep="",outputDir,tag,plots[i],".png")
    png(file=pngPath,width=width,height=height)
    print(plotList[[ plots[i] ]])
    dev.off()
  }
}

## Function for concatenating .CSV files larger than 500 bytes within one project folder and subfolders and removing debug lines
concat_run<-function(directory){
    #define custom call to read.csv and remove debug lines & where logger is not populated
    readCSV<-function(filePath){
        fileData<-read.csv(filePath,header=TRUE)
        fileData<-subset(fileData, type!="debug" & !is.na(logger))
    }
    
    #list all .CSV in folders and subfolders
    wb_files<-list.files(path=directory, recursive=TRUE, pattern="*.CSV$", full.names=TRUE)
    
    #subset files larger than 500 bytes
    wb_files<-subset(wb_files, file.info(wb_files)[,1]>300)
    
    #read each file and output a single dataframe
    data<-do.call(rbind, lapply(wb_files,readCSV))
    
    ### Change relevant columns to integers, numeric or factors
    data$type<-as.factor(data$type)
    data$site<-as.factor(data$site)
    data$logger<-as.factor(data$logger)
    data$deployment<-as.factor(data$deployment)
    data$deployed_at<-as.integer(data$deployed_at)
    data$uuid<-as.factor(data$uuid)
    data$time.s<-as.numeric(data$time.s)
    # measurementCycle
    # burstCycle
    #hardcoded, but could be all columns between time.h and user_note? or measurementCycle when included
    data$battery.V<-as.numeric(data$battery.V)
    data$dht_C<-as.numeric(data$dht_C)
    data$dht_RH<-as.numeric(data$dht_RH)
    data$atlas_CO2_ppm<-as.numeric(data$atlas_CO2_ppm)
    data$ch4rf_raw<-as.numeric(data$ch4rf_raw)
    data$ch4rf_cal<-as.numeric(data$ch4rf_cal)
    data$ch4_raw<-as.numeric(data$ch4_raw)
    data$ch4_cal<-as.numeric(data$ch4_cal)
    
    ## Adding time formats for human readable time and plotting.
    comp_data$time_epoch<-as.character(comp_data$time.s)
    comp_data$dtp<-lubridate::as_datetime(comp_data$time.s)
    
    return(data)
}

## Function that parses through the data and assigns a burst number and then counts each measurement in a burst.
parse_data<-function(comp_data,burst_interval_threshold,measure_cycle_size){
    comp_data$interval<-comp_data$time.s-Lag(comp_data$time.s,shift=1)

    measurement_cycle<-1
    burst<-1
    reading<-1
    comp_data$measurement_cycle<-1
    comp_data$burst<-1
    comp_data$reading<-1
    comp_rows<-nrow(comp_data)
    
    for(i in 1:comp_rows){
        if(!is.na(comp_data$interval[i])){
            if(comp_data$interval[i]>burst_interval_threshold){
                burst<-burst+1
                reading<-1
                comp_data$burst[i:comp_rows]<-burst
                comp_data$reading[i]<-reading
            }else{
                comp_data$reading[i]<-reading
                reading<-reading+1
            }
            if(burst == measure_cycle_size){
                measurement_cycle<-measurement_cycle+1
                comp_data$measurement_cycle[i:comp_rows]<-measurement_cycle
                burst<-0
            }            
        }
    }
    comp_data$burst<-as.factor(comp_data$burst)
#     comp_data$measurement_cycle<-as.factor(comp_data$measurement_cycle)
    return(comp_data)
}


## Function to process each logger individually and output single dataframe
processIndividualLoggers<-function(comp_data,loggerList,loggerLength){
    loggerDataList<-vector("list",loggerCount) #empty list to hold temporary data frames
    
    #subset each logger's data and parse
    for(i in 1:length(loggerList)){
        loggerDataList[[i]]<-parse_data(subset(comp_data,logger==loggerList[i]) ,30,30)
    }
    return(bind_rows(loggerDataList))
}

## Function to create basic plots of burst vs raw methane reading colored by measurement cycle
cycleVsCH4_plots<-function(processed_data, loggerCount, dataType){
    plots<-vector("list", loggerCount)
    names(plots)<-loggerList
    for(i in 1:loggerCount){
        data<-subset(processed_data,logger==loggerList[i] & type==dataType)
        plots[[i]]<-ggplot(data,aes(burst,ch4_raw))+
        geom_point(aes(color=measurement_cycle))+
        ggtitle(paste("Logger: ",loggerList[i],"\nData type: ", dataType))
    }
    return(plots)
}


## Function to calculate rolling CV for warm-up detection
### Starting with using the summary data for calculations
calculateCV<-function(processed_data, loggerCount, dataType){
    temp_cv<-data.frame()
    calculated_cv_list<-vector("list",loggerCount)
    for(i in 1:loggerCount){
        df<-subset(processed_data, logger==loggerList[i] & type==dataType)

        measurement_cycles<-unique(df$measurement_cycle)
        for(i in 1:length(measurement_cycles)){
            temp<-subset(df,measurement_cycle==measurement_cycles[i])
            temp$roll_mean<-rollapply(data=temp$ch4_raw,width=4,align=c("right"),FUN=mean,fill=NA)
            temp$roll_sd<-rollapply(data=temp$ch4_raw,width=4,align=c("right"),FUN=sd,fill=NA)
            temp$roll_cv<-temp$roll_sd/temp$roll_mean #optionally multiply by 100 for %
            temp_cv<-bind_rows(temp_cv,temp)
        }
        calculated_cv_list[[i]]<-temp_cv
    }
    return(bind_rows(calculated_cv_list))
}

## Function to save a dataframe as an rds
saveDFrds<-function(inputDF, outputDir){
    dfName<-substitute(inputDF)
    outputPath<-paste(outputDir,dfName,".rds",sep="")
    print(outputPath)
    saveRDS(inputDF, outputPath)
}

## Function to plot rolling cv vs burst cycle and colored by measurement cycle
rollCV_plots<-function(parseDataCV, loggerCount){
    plots<-vector("list", loggerCount)
    names(plots)<-loggerList
    for(i in 1:loggerCount){
        data<-subset(parseDataCV,logger==loggerList[i])
        plots[[i]]<-ggplot(data,aes(burst,roll_cv))+
        geom_point(aes(color=as.integer(measurement_cycle)))+
        scale_y_log10()+
        ggtitle(paste("Rolling CV for Logger: ",loggerList[i]))
    }
    return(plots)
}

## Function to calculate the mean of rolling cv above provided burstfor each measurement cycle to determine target cv for methane driver
mean_cv_burst<-function(parseDataCV, loggerList, burstCycleStart){
  mean_na<-function(x){mean(x,na.rm=TRUE)}
  
  loggerCount<-length(loggerList)
  
  temp_mean<-vector("list", loggerCount)
  for(i in 1:loggerCount){
    df<-subset(parseDataCV, logger==loggerList[i] & as.integer(burst)>=burstCycleStart)
    temp_mean[[i]]<-summaryBy(roll_cv~measurement_cycle,df,FUN=c(mean_na))
    temp_mean[[i]]$logger<-unique(df$logger)
    temp_mean[[i]]$deployed_at<-lubridate::as_datetime(unique(df$deployed_at))
  }
  return(bind_rows(temp_mean))
}

# MAIN
## Define location of data
#directory<-"/Users/jhosen/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/WaterBear/CH4DHT22/20221110_pull/bin"
# directory<-"~/Desktop/data/mesoBinTests/20221110_pull"
directory<-"~/Desktop/data/mesoBinTests/20221118_pull"

### Create directory for outputs based on data directory
outputDir<-paste(directory,"_output/",sep="")
newDir(outputDir)

### Importing the data from a single test run using the function.
comp_data<-concat_run(directory)

### Process data and create variables for use later
loggerList<-unique(comp_data$logger) #save each individual logger
loggerCount<-length(loggerList) #how many loggers
processed_data<-processIndividualLoggers(comp_data, loggerList, loggerCount)

#### Save processed dataframe as .rds to output folder
saveDFrds(processed_data, outputDir)

### Create plots of raw data and summary data
raw_cycleVsCH4_plots<-cycleVsCH4_plots(processed_data, loggerCount, "raw")
summary_cycleVsCH4_plots<-cycleVsCH4_plots(processed_data, loggerCount, "summary")

raw_cycleVsCH4_plots

summary_cycleVsCH4_plots

#### Note: you can modify plots after the fact:
#plots[["21003"]]+ggtitle("Overwrite")

#### TODO: save the final processed dataframe to RDS file
parseSummaryDataCV<-calculateCV(processed_data, loggerCount, "summary")

saveDFrds(parseSummaryDataCV, outputDir)

### Create rolling cv plots
summaryDataCV_plots2<-rollCV_plots(parseSummaryDataCV, loggerCount)

summaryDataCV_plots2

## calculate the mean cv of each burst
meanCVburst<-mean_cv_burst(parseSummaryDataCV, loggerList, 20)

### Print results  TODO: save these results
for(i in 1:length(loggerList)){
    print(loggerList[i])
    print(subset(meanCVburst, logger==loggerList[i]))
}
### TODO: use this ^ data to decide a CV limit indicated sensor has been sufficiently warmed, then go back to dataset and subset it to graph individual points when warmed up

# cv_burst_20<-subset(parseDataCV,logger==21004 & as.integer(burst)>20)

# mean_na<-function(x){mean(x,na.rm=TRUE)}

# test<-summaryBy(roll_cv~measurement_cycle,cv_burst_20,FUN=c(mean_na))
# test$logger<-unique(cv_burst_20$logger)
# test$deployed_at<-lubridate::as_datetime(unique(cv_burst_20$deployed_at))

# comp_cv_burst_20<-subset(comp_cv_50FF74068678545256252467,burst>20)

# mean_na<-function(x){mean(x,na.rm=TRUE)}

# comp_sum_50FF74068678545256252467<-summaryBy(roll_cv~measurement_cycle,comp_cv_burst_20,FUN=c(mean_na))
# comp_sum_50FF74068678545256252467$uuid<-unique(comp_cv_50FF74068678545256252467$uuid)
# comp_sum_50FF74068678545256252467$deployed_at<-lubridate::as_datetime(unique(comp_cv_50FF74068678545256252467$deployed_at))

# head(comp_sum_50FF74068678545256252467)
# saveRDS(comp_sum_50FF74068678545256252467,"comp_sum_50FF74068678545256252467.rds")

# comp_parse<-parse_data(subset(comp_data,uuid=="50FF74068678545256252467"),30,30)
# ggplot(comp_parse,aes(as.factor(burst),atlas_CO2_ppm))+
# geom_point()

# head(comp_parse)

# comp_data_sum<-summaryBy(ch4_raw+ch4rf_raw~dtp+uuid,comp_parse,FUN=c(sd,mean))

# head(comp_data_sum)
#comp_parse$dtp

# comp_data_sum$dtp<-as_datetime(comp_data_sum$dtp)

# head(comp_data_sum)

# ggplot(comp_data_sum,aes(dtp,ch4_raw.mean))+
# geom_point()+
# facet_wrap(.~uuid)


# ggplot(comp_data_sum,aes(dtp,ch4rf_raw.mean))+
# geom_point()+
# facet_wrap(.~uuid)

# comp_data_sum$ch4_raw_norm<-comp_data_sum$ch4_raw.mean/comp_data_sum$ch4rf_raw.mean

# ggplot(comp_data_sum,aes(dtp,ch4_raw_norm))+
# geom_point()+
# facet_wrap(.~uuid)

# ggplot(subset(comp_data),aes(dtp,battery.V))+
#     geom_point(aes(color=as.factor(group)))+
# ylim(1500,1650)

# names(comp_data)

# ggplot(comp_data,aes(dtp,atlas_CO2_ppm))+
#        geom_point()

# head(comp_data,n=100)

### Saving plots that have been produced so far
savePlotList(raw_cycleVsCH4_plots, outputDir, "raw_cycleVsCH4_")
savePlotList(summary_cycleVsCH4_plots, outputDir, "summary_cycleVsCH4_")
savePlotList(summaryDataCV_plots, outputDir, "summaryDataCV_")

# dir<-'~/Desktop/data/mesoBinTests/20221110_pull/'

# list.files(path=directory2, recursive=TRUE)

# files<-list.files(path=directory2, recursive=TRUE, pattern=".CSV$")

# files

# exclude<-list("notes.txt","debug.csv","metadata.rtf")

# files<-list.files(path=dir, recursive=TRUE)

# files[!(files %in% exclude)]


