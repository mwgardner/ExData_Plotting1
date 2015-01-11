exploreDataPlot4 <- function() {
    # reads in variable descriptions and activity descriptions
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
    
    # Create plot4, which contains 4 plots in a 2 x 2 grid
    # plot is 480x480 pixel, PNG type
    png("plot4.png",width=480,height=480,units="px", bg="transparent", pointsize=11)
    par(mfrow=c(2,2))
    with(housePower, {
        # top-right is a scatterplot (line type) of Global Active Power vs. Time
        plot(dateTime,Global_active_power,type="l",ann=FALSE)
        title(ylab="Global Active Power")
        # top-left is a scatterplot (line type) of Voltage vs. Time    
        plot(dateTime,Voltage,type="l",xlab="datetime",ylab="Voltage")
        # bottom-left is a scatterplot (line type) of all three Energy sub metering values
        # vs. Time
        plot(dateTime,Sub_metering_1,ann=FALSE,type="l",col="black",lwd=1)
        lines(dateTime,Sub_metering_2,col="red",lwd=1)
        lines(dateTime,Sub_metering_3,col="blue",lwd=1)
        title(ylab="Energy sub metering")
        legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               col=c("black","red","blue"),lty=1,lwd=1,bty="n")
        # bottom-right is a scatterplot (line type) of Global Reaactive Power vs. Time
        plot(dateTime,Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
    })
    dev.off()
}