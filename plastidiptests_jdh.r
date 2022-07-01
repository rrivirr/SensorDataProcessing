# exADC_CH4DHT22.R created Jun 27, 2022 by Ken Chong
# Last edit: Jul 1, 2022 by Ken Chong
#
# Code to process WaterBear data containing output from one Figaro NGM2611-E13 methane sensor,
# and one Adafruit DHT22 temperature and relative humidity sensor
#
# note: battery.V may be irrelevant due to some components on rail being removed on board
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
# wd = "~/Desktop/data/productionTests" #linux path
wd = "/Volumes/GoogleDrive-116069313024281609105/My Drive/USDA Wetlands/Data/Raw_Sensor_Data/WaterBear_MethaneTemperatureRelativeHumidity/" #linux path #HARDCODED

# specific experiment folder to process
pt2 = "plastidipTest2" #HARDCODED

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
exDir<-"/Volumes/GoogleDrive-116069313024281609105/My Drive/USDA Wetlands/Data/Raw_Sensor_Data/WaterBear_MethaneTemperatureRelativeHumidity/plastidipTest2/"
merge_ex_csvs <- function(exDir){
  # data_dirs <- list.files(path=exDir, pattern="ABElab*", all.files=FALSE) # pull all dirs starting with ABElab
  data_dirs <- list.files(path=exDir, all.files=FALSE) # pull all dirs starting with ABElab
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

#convert time.s to datetime column, split into hour and date, create a new hourly factor, change other columns to factors
process_columns <- function(compiled_data){
  compiled_data$datetime<-lubridate::as_datetime(compiled_data$time.s) # turn epoch to date time
  compiled_data$date<-date(compiled_data$datetime) # extract dates
  compiled_data$hour<-hour(compiled_data$datetime) # extract hours
  compiled_data$minute<-minute(compiled_data$datetime) # extract minutes
  compiled_data$second<-round(compiled_data$time.s, 0) # round off epoch to remove milliseconds (used for resampling later)

  #reinterpret columns as factors
  compiled_data$datehour<-as.factor(format.Date(compiled_data$datetime, format = "%Y-%m-%d %H"))
  compiled_data$type<-as.factor(compiled_data$type)
  compiled_data$site<-as.factor(compiled_data$site)
  compiled_data$logger <- as.factor(compiled_data$logger)
  compiled_data$deployment<-as.factor(compiled_data$deployment)
  compiled_data$uuid<-as.factor(compiled_data$uuid)

  return(compiled_data)
}


cd<-merge_ex_csvs("/Volumes/GoogleDrive-116069313024281609105/My Drive/USDA Wetlands/Data/Raw_Sensor_Data/WaterBear_MethaneTemperatureRelativeHumidity/plastidipTest2/")


raw_bin_21008<-subset(cd,deployed_at=="1656006172")

raw_bin_21008$dtp<-as.POSIXct(raw_bin_21008$time.s, origin="1970-01-01")
raw_bin_21008$time.s_lag1<-lag(raw_bin_21008$time.s,1)
raw_bin_21008$time_diff<-raw_bin_21008$time.s-raw_bin_21008$time.s_lag1
raw_bin_21008$group<-1
group_count<-1
for(i in 2:nrow(raw_bin_21008)){
	if(raw_bin_21008$time_diff[i]>100){
		group_count<-group_count+1
		raw_bin_21008$group[i:nrow(raw_bin_21008)]<-group_count
	}
}

ggplot(raw_bin_21008,aes(dtp,ch4_raw))+
geom_point(size=2,aes(color=group))

raw_bin_21008<-subset(raw_bin_21008,type=="raw")

report<-data.frame()
for(j in 1:max(raw_bin_21008$group)){
	group_sub<-subset(raw_bin_21008,group==j)
	gg<-ggplot(group_sub,aes(time.s,ch4_raw))+
	geom_point(size=2)
	print(gg)
	group_sub$runmean = runmean(group_sub$ch4_raw, 10, endrule="mean")
	group_sub$runsd = runsd(group_sub$ch4_raw, 10, endrule="sd")
	group_sub$runcv<-group_sub$runsd/group_sub$runmean
	
	invisible(readline(prompt="Press [enter] to continue"))
	
	gsmelt<-melt(group_sub[,c("time.s","runmean","runsd","runcv","ch4_raw")],id.vars=c("time.s"))
	meltplot<-ggplot(gsmelt,aes(time.s,value))+
	geom_point(size=2,aes(color=variable))+
	facet_wrap(.~variable,scales="free_y",ncol=1)
	
	print(meltplot)
		
		
	temp_report<-data.frame(group=j,min_cv=min(group_sub$runcv),min_sd=min(group_sub$runsd),first_cv_below_005=min(which(group_sub$runcv<0.005)))
	report<-bind_rows(report,temp_report)
	invisible(readline(prompt="Press [enter] to continue"))
	
}