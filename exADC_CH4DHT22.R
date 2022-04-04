library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(stringr)
library(gridExtra)

# rm(list = ls()) #clear R working memory

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
    break
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

# #process experiment 1 into data frame
# ex1_df <- merge_ex_csvs(ex1_path)
# ex1_df_dt <- process_time(ex1_df)
# 
# #data frame of unique deployments, can be accessed by df[x,1]
# deployments = unique(ex1_df_dt['deployment'])
# 
# #manual subsets [4 deployed WaterBears]
# ex1_df_dt_1 <- subset(ex1_df_dt, deployment==deployments[1,1])
# ex1_df_dt_2 <- subset(ex1_df_dt, deployment==deployments[2,1])
# ex1_df_dt_3 <- subset(ex1_df_dt, deployment==deployments[3,1])
# ex1_df_dt_4 <- subset(ex1_df_dt, deployment==deployments[4,1])
# 
# #basic plots of all data [methane reference, methane, temperature, relative humidity]
# ex1_ch4rf <- ggplot(data=ex1_df_dt)+geom_point(aes(x=dtp,y=ch4rf_raw,color=deployment))
# ex1_ch4 <- ggplot(data=ex1_df_dt)+geom_point(aes(x=dtp,y=ch4_raw,color=deployment))
# ex1_t <- ggplot(data=ex1_df_dt)+geom_point(aes(x=dtp,y=dht_C,color=deployment))
# ex1_rh <- ggplot(data=ex1_df_dt)+geom_point(aes(x=dtp,y=dht_RH,color=deployment))
# 
# #basic plots by deployment
# ex1_ch4rf_1 <-ggplot(data=ex1_df_dt_1)+geom_point(aes(x=dtp,y=ch4rf_raw))
# ex1_ch4_1 <-ggplot(data=ex1_df_dt_1)+geom_point(aes(x=dtp,y=ch4_raw))
# ex1_t_1 <-ggplot(data=ex1_df_dt_1)+geom_point(aes(x=dtp,y=dht_C))
# ex1_rh_1 <-ggplot(data=ex1_df_dt_1)+geom_point(aes(x=dtp,y=dht_RH))
# 
# ex1_ch4rf_2 <-ggplot(data=ex1_df_dt_2)+geom_point(aes(x=dtp,y=ch4rf_raw))
# ex1_ch4_2 <-ggplot(data=ex1_df_dt_2)+geom_point(aes(x=dtp,y=ch4_raw))
# ex1_t_2 <-ggplot(data=ex1_df_dt_2)+geom_point(aes(x=dtp,y=dht_C))
# ex1_rh_2 <-ggplot(data=ex1_df_dt_2)+geom_point(aes(x=dtp,y=dht_RH))
# 
# ex1_ch4rf_3 <-ggplot(data=ex1_df_dt_3)+geom_point(aes(x=dtp,y=ch4rf_raw))
# ex1_ch4_3 <-ggplot(data=ex1_df_dt_3)+geom_point(aes(x=dtp,y=ch4_raw))
# ex1_t_3 <-ggplot(data=ex1_df_dt_3)+geom_point(aes(x=dtp,y=dht_C))
# ex1_rh_3 <-ggplot(data=ex1_df_dt_3)+geom_point(aes(x=dtp,y=dht_RH))
# 
# ex1_ch4rf_4 <-ggplot(data=ex1_df_dt_4)+geom_point(aes(x=dtp,y=ch4rf_raw))
# ex1_ch4_4 <-ggplot(data=ex1_df_dt_4)+geom_point(aes(x=dtp,y=ch4_raw))
# ex1_t_4 <-ggplot(data=ex1_df_dt_4)+geom_point(aes(x=dtp,y=dht_C))
# ex1_rh_4 <-ggplot(data=ex1_df_dt_4)+geom_point(aes(x=dtp,y=dht_RH))

# method to plot ch4rf_raw for each deployment as individual elements in a list
ls = list()
for (x in 1:length(deployments[,1])){
  ls[[x]] = ggplot(data=subset(ex1_df_dt, deployment==deployments[x,1]))+geom_point(aes(x=dtp,y=ch4rf_raw))
}
print(ls[[4]])

#hardcode columns to plot
col = c('battery.V','ch4rf_raw','ch4_raw','dht_C','dht_RH')
ylabs = c('Battery Voltage', 'Methane Reference Voltage', 'Methane Voltage', 'Temperature (C)', 'Relative Humidity (%)')
  # this will work once we have calibrated values to also plot
  # col = names(ex1_df_dt)[7:13]
a = list()
b = list()
ls = list(list())
for(i in 1:length(col)){
  if(i == 1){
    for(j in 1:length(deployments[,1])){
      # print(j)
      test =subset(ex1_df_dt, deployment==deployments[j,1])
      ls[[i]][[j]] = ggplot(data=test)+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(deployments[j,1])
    }
  } else if (i == 2) {
    print(i)
    for(j in 1:length(deployments[,1])){
      print(j)
      test =subset(ex1_df_dt, deployment==deployments[j,1])
      ls[[i]][[j]] = ggplot(data=test)+
        geom_point(aes_string(x="dtp",y=col[i]))+
        ylab(ylabs[i])+
        xlab("Date")+
        ggtitle(deployments[j,1])
    }
  } else if (i == 3){
    
  } else {
    
  }
}

# as.formula
print(ls[[1]][[2]])
#ideally create basic plots into table with deployment as index, columns are values being plotted
#so [1,'ch4rf_raw'] would store the plot for deployment 1's raw methane values

#output goal: save each deployment to its own page?
# save all combined plots to one page?

# # Plot all relationships
# ggplotRegression <- function (fit) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5))) +
#     theme_classic(base_size = 16)
# }
# 
# for (i in colnames(combined_dataset_log[,c(11:21)])) {
#   formula <- as.formula(paste(i, " ~ invaded_percentCover"))
#   fit1 <- lm(formula, data = combined_dataset_log)
#   print(ggplotRegression(fit1))
# }

# list1 and list2 are uni-dimensional lists
list1 <- list (c(1:5), "hi", 0 + 5i)
list2 <- list(c(6:8))

# create a list_data with two lists as its elements
list_data <- list(list1, list2)

# runs uptil the length of outer list
for (i in c(1 : length(list_data)))
{
  # runs uptil the length of inner lists at ith indices
  for (j in c(1: length(list_data[[i]])))
  { 
    cat ("List", i, "element", j, ": ")
    print (list_data[[i]][[j]])
  }
}


#open PDF
pdf(file=ex1_dest, onefile=TRUE)

#specify to save plots in 2x2 grid
par(mfrow = c(2,2))

#save plots to PDF
for (i in seq(length(ls))) {
  do.call("grid.arrange", ls[i])  
}

#turn off PDF plotting
dev.off()
