exploreDataPlot1 <- function() {
    # working directory should include a "data" folder that contains the
    # household_power_consumption.txt file
    if(!file.exists("data")) {
        stop("Data folder ('data') not found.")
    }
    setwd("./data")
    
    # date format in data file = dd/mm/yyyy
    # time format in data file = hh:mm:ss (24-h time)
    
    # read in data as data.frame
    housePower <- read.table("household_power_consumption.txt", 
                             header=TRUE, sep=";", na.strings="?", stringsAsFactors=FALSE, 
                             colClasses=c(rep("character",2),rep("numeric",7)))
    # subset housePower to only include data from 2007-02-01 and 2007-02-02
    housePower <- housePower[housePower$Date=="1/2/2007" | housePower$Date=="2/2/2007",]
    # convert date and time values to a single dateTime as POSIXct
    dateTime <- as.POSIXct(strptime(paste(housePower$Date,housePower$Time),"%d/%m/%Y %H:%M:%S"))
    
    # append dateTime column to housePower dataframe
    housePower <- data.frame(housePower, dateTime)
    
    # Create plot1, a histogram of Global Active Power
    # plot is 480x480 pixel, PNG type
    png("plot1.png",width=480,height=480,units="px", bg="transparent", pointsize=12)
    hist(housePower$Global_active_power,xlab="Global Active Power (kilowatts)",
         ylab="Frequency",col="red", main="Global Active Power")
    dev.off()
}