# Functions used in multiple files to be run with source()
# Note: source() can only use .r files, not .ipynb or .rmd
## TODO: these functions currently lack basic error checking and error messages
## TODO: parameter names need to be more self explanatory

# 0. Session management

## Clear session memory
clearMemory<-function(){
    rm(list = ls()) # clear R working memory
    graphics.off() # close any open plots
}

## Take a list of packages, install them to the local library, if necessary, and then load them all
packageHandler<-function(packageList=packages){
    # Install packages if necessary
#     installed_packages <- packages %in% rownames(installed.packages())
#     if (any(installed_packages == FALSE)) {
#       install.packages(packages[!installed_packages])
#     }
    # will it crash if there's no packages to install?
    install.packages( packages[ !( packages %in% rownames( installed.packages() ) ) ] )

    # Load packages
    invisible(lapply(packages, library, character.only = TRUE))
}

# 1. File management

# check if file at path exists, delete if it does
# will only call in functions that generate new files
cleanFile<-function(path){
  if(file.exists(path)){
    cat(sprintf("Deleting old file: %s\n",path))
    file.remove(path)
  }
}

# check if directory exists, create if not
newDir<-function(dirPath){
  if(dir.exists(dirPath)){
    cat(sprintf("Output directory, %s, already exists\n", dirPath))
  }
  else{
    cat(sprintf("Output directory not found, creating: %s\n",dirPath))
    dir.create(dirPath)
  }
}

# write lines to a complete file path
writeFile<-function(lines, filePath){
    fd<-file(filePath)
    writeLines(lines, fd)
    close(fd)
}


#800 is a little over 5" on my screen at 1920x1080
#todo: add units, default to pixels, check other options for png()
savePlot<-function(plot, tag="", width=800, height=800, od=outputDir){
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    pngPath=paste0(od,tag,".png")
    png(file=pngPath, width=width, height=height)
    print(plot)
    dev.off()
}


#input: list of plots , output directory, custom directory path or tag
    # class(plotList) should return 'list'
    # class(plotList[[1]]) should return 'gg plot' or something equivalent
#output: save each plot to output directory with individual names
#800 is a little over 5" on my screen at 1920x1080
savePlotList<-function(plotList, tag="", width=800, height=800, od=outputDir){
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    plots<-names(plotList)
    # check if plots have names or not, incorporate into file name or not
    if(is.null(plots)){
        for(i in 1:length(plotList)){
            tag=paste0(tag, "_", i)
            savePlot(plotList[[ i ]], tag, width, height, od)
#             pngPath = paste0(od, tag, "_", i, ".png")
#             png(file=pngPath, width=width, height=height)
#             print(plotList[[ i ]])
#             dev.off()
        }
    } else {
        for(i in 1:length(plots)){
            tag=paste0(tag, "_", plots[i])
            savePlot(plostlist[[ plots[i] ]], tag, width, height, od)
#             pngPath = paste0(od, tag, "_", plots[i], ".png")
#             png(file=pngPath, width=width, height=height)
#             print(plotList[[ plots[i] ]])
#             dev.off()
        }
    }
}

# Save a list of list of plots to an output directory
savePlotListList<-function(pll, tag="", width=800, height=800, od=outputDir){
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    for(i in 1:length(pll)){
        savePlotList(pll[[i]], od, tag=paste0(tag,"_",names(pll[i])), width=width, height=height)
    }
}

## Save a dataframe as an rds file to a specified directory
# filename is : dataframeName_{user input string}.rds or dataframeName.rds if no tag provided
saveDFrds<-function(inputDF, tag=NULL, od=outputDir){
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    dfName<-substitute(inputDF)
    if(is.null(tag)){
        outputPath<-paste0(od,dfName,".rds")
    } else {
        outputPath<-paste0(od,dfName,"_",tag,".rds")
    }
    print(outputPath)
    saveRDS(inputDF, outputPath)
}

## Save a dataframe as a csv file to a specified directory
# filename is : dataframeName_{user input string}.csv or dataframeName.csv if no tag provided
saveDFcsv<-function(inputDF, tag=NULL, od=outputDir){
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    dfName<-substitute(inputDF)
    if(is.null(tag)){
        outputPath<-paste0(od,dfName,".csv")
    } else {
        outputPath<-paste0(od,dfName,"_",tag,".csv")
    }
    print(outputPath)
    write.csv(inputDF, outputPath)
}

## Custom readCSV function for RRIV data
# define custom call to read.csv
# RRIV data has a "type" column which contains "debug" lines which can be removed for processing data
# some files have lines with issues that offest the data,
# which can be seen by checking if there is a logger value present, and removing if not
read_rriv_CSV<-function(filepath){
    fileData<-read_csv(filepath,col_names=TRUE,col_types=cols(.default="c"))
 
#     for(name in names(dileData){
#         fileData[[name]]<-as.character(fileData[[name]])
#     }
    
    fileData<-fileData[fileData[1] != "debug" & fileData[1] != "",] # changing for old conductivity data, might need to change back
#     fileData<-subset(fileData, type!="debug" & !is.na(logger))
    return(as.data.frame(fileData))
}

## Custom readZIP function for Gas Analzyer data
read_GA_ZIP<-function(filepath){
    filepath<-unzip(filepath, exdir="./temp")
    fileData<-read_GA_TXT(filepath)
}

read_GA_TXT<-function(filepath){
    fileData<-read.csv(filepath, skip=1, header=TRUE)
}

## Custom function to clean .txt files generated by unzipping Gas Analyzer data
clean_temp<-function(){
    dirs<-list.dirs(getwd(),recursive=FALSE)
    dirs<-dirs[ grepl(dirs, pattern="temp") ]
    unlink(dirs, recursive=TRUE)
}

## Concatenate all files in a directory including sub directories into one dataframe and return it using a supplied read function (such as read.csv, read.csv2, or a custom reading function )
# input: "directory" containing all files to be concatenated (will check sub directories),
# "readFn" function to use for reading files,
# "filePattern" regex? string to search for specific file names/types/etc,  ex "*.csv$"
# "minFileSize" omit files under specified size in bytes, 0 bytes by default
# output: dataframe of concatenated files
concat_dirs<-function(directory=dataDirectory, readFn, filePattern=NULL, minFileSize=0){
    # list all files following specific pattern in folders and subfolders
    Files<-list.files(path=directory, recursive=TRUE, pattern=filePattern, full.names=TRUE)
    
    # subset files larger than minFileSize bytes
    Files<-subset(Files, file.info(Files)[,1]>minFileSize)
    
    #read each file and output a single dataframe
    ###TODO, rbind rows even if their columns don't align, names() returns different results, custom rbind?
    data<-do.call(bind_rows, lapply(Files, readFn))
    
    print("Dataframe generated, manually process column types if necessary")
    return(data)
}

# 2. RRIV data specific functions

## Custom column data type processing for RRIV csv data
process_rriv_columns<-function(df){
    cols<-names(df)
    
    df$type<-as.factor(df$type)
    df$site<-as.factor(df$site)
    if("logger" %in% cols){
        df$logger<-as.factor(df$logger)
    }
    df$deployment<-as.factor(df$deployment)
    df$deployed_at<-as.integer(df$deployed_at)
    df$uuid<-as.factor(df$uuid)
    df$time.s<-as.numeric(df$time.s)
    df$time.h<-lubridate::as_datetime(df$time.h)
    if("measurementCycle" %in% cols & "burstCycle" %in% cols){
        df$measurementCycle<-as.numeric(df$measurementCycle)
        df$burstCycle<-as.factor(df$burstCycle)
    }
    ##hardcoded, but could be all columns between time.h and user_note? or measurementCycle when included
    df$battery.V<-as.numeric(df$battery.V)
    if("dht_C" %in% cols){
        df$dht_C<-as.numeric(df$dht_C)
        df$dht_RH<-as.numeric(df$dht_RH)
    }
    if("aht_C" %in% cols){
        df$aht_C<-as.numeric(df$aht_C)
        df$aht_RH<-as.numeric(df$aht_RH)
    }
    if("atlas_CO2_ppm" %in% cols){
        df$atlas_CO2_ppm<-as.numeric(df$atlas_CO2_ppm)
    }
    if("ch4rf_raw" %in% cols){
        df$ch4rf_raw<-as.numeric(df$ch4rf_raw)
        df$ch4rf_cal<-as.numeric(df$ch4rf_cal)
    }
    df$ch4_raw<-as.numeric(df$ch4_raw)
    df$ch4_cal<-as.numeric(df$ch4_cal)
    
    return(df)
}

## Custom RRIV data parsing that created measurementCycle and burstCycle columns, before the columns were included in the RRIV firmware

## Function that parses through the data and assigns a burst number and then counts each measurement in a burst. (Data will eventually have measurementCycle and burstCycle natively)
parse_data<-function(df,burst_interval_threshold,measure_cycle_size){
    #check if measurementCycle and burstCycle already present in data:
    cols<-names(df)
    if("measurementCycle" %in% cols & "burstCycle" %in% cols){
        return(df)
    }
    
    df$interval<-df$time.s-Lag(df$time.s,shift=1)

    measurementCycle<-1
    burstCycle<-1
    reading<-1
    df$measurementCycle<-1
    df$burstCycle<-1
    df$reading<-1
    comp_rows<-nrow(df)
    
    for(i in 1:comp_rows){
        if(!is.na(df$interval[i])){
            if(df$interval[i]>burst_interval_threshold){
                burstCycle<-burstCycle+1
                reading<-1
                df$burstCycle[i:comp_rows]<-burstCycle
                df$reading[i]<-reading
            }else{
                df$reading[i]<-reading
                reading<-reading+1
            }
            if(burstCycle == measure_cycle_size){
                measurementCycle<-measurementCycle+1
                df$measurementCycle[i:comp_rows]<-measurementCycle
                burstCycle<-0
            }            
        }
    }
    df$burstCycle<-as.factor(df$burstCycle)
#     df$measurementCycle<-as.factor(df$measurementCycle)
    return(df)
}

## Function to process each logger individually using above function and output single dataframe
parseIndividualLoggers<-function(df, ll=loggerList, lc=loggerCount){
    loggerDataList<-vector("list", lc) #empty list to hold temporary data frames
    
    #subset each logger's data and parse
    for(i in 1:lc){
        loggerDataList[[i]]<-parse_data(subset(df, logger==ll[i]), 30, 30)
    }
    return(bind_rows(loggerDataList))
}

## Function to calculate absolute humidity given a dataframe with a temperature (celsius) and relative humidity (%) column
calcAbsoluteHumidity<-function(df, celsius="dht_C", relativeHumidity="dht_RH"){
    #determine water vapor saturation point (https://www.orslabs.fr/pdf/Humidity%20Equations.pdf)
    P<-1013.25 #millibar, standard pressure ~1 atmosphere
#     df$ews_mbar<-(1.0007+3.46*10^-6*P)*6.1121^(17.502*df$dht_C/(240.9+df$dht_C))
    df$ews_mbar<-(1.0007+3.46*10^-6*P)*6.1121^(17.502*df[[celsius]]/(240.9+df[[celsius]]))
    df$ews_kPa<-df$ews_mbar/10

    #vaisala absolute humidity equation (https://www.hatchability.com/Vaisala.pdf)
    C<-2.16679 #gK/J
#     df$aH_gm3<-C*df$ews_kPa*(df$dht_RH/100)*1000/(273.15+df$dht_C)
    df$aH_gm3<-C*df$ews_kPa*(df[[relativeHumidity]]/100)*1000/(273.15+df[[celsius]])

    return(df)
}


## Function to add a Sensor column based on a dictionary/character vector of named pairs of id = sensor name
createSensorColFromIDs<-function(df, idCol, dict){
    ids<-unique(df[[idCol]])
    for(id in ids){
        df$Sensor[ df[[idCol]]==id ]<-dict[id]
    }
    return(df)
}

## Function to add a New column based on a dictionary/character vector of named pairs of id = sensor name
createNewColFromIDs<-function(df, idCol, newCol, dict){
    ids<-unique(df[[idCol]])
    for(id in ids){
        df[[ newCol ]][ df[[idCol]]==id ]<-dict[id]
    }
    return(df)
}

## Function to calculate v0 values for data
calcV0<-function(df, v0lmFilePath){
    v0_lm<-readRDS(v0lmFilePath)
    
    sensors<-unique(df$Sensor)
    for(sensor in sensors){
        print(sensor)
        lm<-v0_lm[[sensor]][["interactive"]]
        print(lm)
        df$v0[df$Sensor == sensor]<-predict(lm,newdata= df[df$Sensor==sensor,])
    }
    return(df)
}

## Function to calculate methane sensor resistance according to Figaro TGS 2611 product information
calcSensorResistance<-function(df){
#     df$ch4_mV<-5000/4096*df$ch4_raw
#     df$v0_mV<-5000/4096*df$v0
#     df$Rs_mΩ = (5000/df$ch4_mV-1)/(5000/df$v0_mV-1)*1000
    
    df$ch4_V<-5/4096*df$ch4_raw
    df$v0_V<-5/4096*df$v0
    df$Rs_ohm = (5/df$ch4_V-1)/(5/df$v0_V-1)
    return(df)
}

## Function to calculate calibrated CH4 from linear models
calcCH4_cal<-function(df, lmFilePath){
    lms<-readRDS(lmFilePath)
    
    sensors<-unique(df$Sensor)
    for(sensor in sensors){
        print(sensor)
        lm<-lms[[sensor]]
        print(lm)
        df$ch4_cal[df$Sensor==sensor]<-predict(lm,newdata=df[df$Sensor==sensor,])
    }
    return(df)
}

## Get a custom dictionary of columns to plot and labels for them
# hardcoded labels for specific columns
getDict<-function(df){
    dict<-c(
        "battery.V"="Battery Digital\nReading (12bit)",
        "dht_C"="\nTemperature (°C)",
        "dht_RH"="\nRelative Humidity (%)",
        "aht_C"="\nTemperature (°C)",
        "aht_RH"="\nRelative Humidity (%)",
        "ch4rf_raw"="Raw Methane Reference\nDigital Reading (12bit)",
        "ch4rf_cal"="Calibrated Methane Reference\nDigital Reading (12bit)",
        "ch4_raw"="Raw Methane Digital\nReading (12bit)",
        "ch4_cal"="Calibrated Methane Digital\nReading (12bit)",
        "atlas_CO2_ppm"="\nCO2 (ppm)",
        "roll_cv"="Rolling Coefficient\nof Variation",
        "aH_gm3"="Absolute Humidity (g/m^3)"
    )
    return( dict[ names(df)[names(df) %in% names(dict) == TRUE] ] )
}

# 3. Plotting functions

## function that goes through each item in a variable dictionary and creates a basic plot of variable vs a time column, or just x vs y colored by logger/site
plot_Data_v_Time <-function(df, color="logger", timeCol="time.h"){
    variableDict<-getDict(df)
    
    variableKeys<-names(variableDict)
    variableCount<-length(variableDict)
    
    # initialize list to hold plots
    DvT = vector('list', variableCount)
    names(DvT) = variableKeys
    
    # plot each column vs time with all deployments into list
    for ( i in 1:variableCount ){
        DvT[[i]] = ggplot(data=df,aes_string(x=timeCol,y=variableKeys[i],color=color),size=1)+
        geom_point()+geom_line()+theme_classic(base_size=12)+
        labs(x="Date", y=variableDict[i], color=NULL)#+
#         scale_color_manual(values=custom_colors2,na.translate=F)+
#         scale_x_datetime(date_labels="%m/%d %H",breaks=scales::pretty_breaks(n=4),expand=c(0,60*5))
    }
    return(DvT)
}

## function that goes through and does each series of basic plots of variable vs time.h for each individual logger
### TODO, allow for uuid/site instead of logger
# 1. function can count unique uuid/site occurrences in df and iterate over that
# 2. title can be column name? or added by user later, or generic "RRIV:"
plot_individual_logger_data_v_time <-function(df, timeCol="time.h", idCol="uuid"){
    variableDict<-getDict(df)
    variableKeys<-names(variableDict)
    variableCount<-length(variableDict)
    
    ids<-unique(df[[idCol]])
    idCount<-length(ids)
    
    ## list of lists, where list values are the names of columns and loggers for the deployment
    output <- vector("list", variableCount)
    names(output) <- variableKeys
    
    # initialize empty double list to hold plots
    for(i in 1:variableCount){
        output[[ variableKeys[i] ]] <- vector("list", idCount)
        names( output[[ variableKeys[i] ]] ) <- ids
    }
    
    # create plots at respective locations
    for(i in 1:variableCount){
        for(j in 1:idCount){
          output[[ variableKeys[i] ]][[ ids[j] ]] = ggplot(data=df[df[[idCol]]==ids[j],])+
            geom_point(aes_string(x=timeCol,y=variableKeys[i]),size=1)+theme_classic(base_size=12)+
            ylab(variableDict[i])+xlab("Date")+ggtitle(paste0("ID: ",ids[j]))
          # +
          #   scale_color_manual(values=custom_colors2,na.translate=F)+
          #   scale_x_datetime(date_labels="%m/%d %H",breaks=scales::pretty_breaks(n=4),expand=c(0,60*120))
        }
    }
    return(output)
}


## function that subsets a dataset for each hour of a chosen posix time column, then creates plots and saves them, colored by a chosen id column
hourlyPlotsvTime<-function(df, timeCol="time.h", id="site", od=outputDir){
    oneHour=1*60*60
    rangeStart<-round( range(df[[timeCol]])[1], units="hours" )
    rangeEnd<-round( range(df[[timeCol]])[2], units ="hours")
    
    # check if od ends with /, add if not
    if(substr(od, nchar(od), nchar(od)) != "/"){ od<-paste0(od,"/") }
    
    # create output directory for hourly plots
    h_od = paste0(od, "hourlyPlots/")
    newDir(h_od)

    hour = 1
    while(rangeStart < rangeEnd){
        subsetEnd<-rangeStart+oneHour

        subset<-df[ df[[timeCol]]>rangeStart & df[[timeCol]]<subsetEnd ,]
        sitePlots<-plot_Data_v_Time(subset, id)

        savePlotList(sitePlots, tag=hour, od=h_od)

        rangeStart=rangeStart+oneHour
        hour=hour+1
    }
}


