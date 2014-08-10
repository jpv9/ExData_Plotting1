#---------------- Plot 1--------------------------------------------
#     readData()  --> Downloads zip and reads initial data frame
#
#     plot2()  --> to create plot from data
#------------------------------------------------------------------

readData <- function(){
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
  con <- unz(temp, "household_power_consumption.txt")
  data <- as.data.frame(read.table(con, sep=";", na.strings="?", header=T, stringsAsFactors = FALSE))
  unlink(temp)
  
  data$Time <- strptime(paste(data$Date,data$Time), format="%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  
  data$Global_active_power   <- as.numeric(data$Global_active_power)
  data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
  data$Global_intensity      <- as.numeric(data$Global_intensity, format="%H:%M:%S")
  
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  
  data$Voltage <- as.numeric(data$Voltage)
  
  data <- data[data$Date <= "2007-02-02", ]
  data <- data[data$Date >= "2007-02-01", ]
  
  data    
}

plot2 <- function(){
  
  dat <- readData() 
  png("plot2.png", width=480, height=480, bg="transparent")
  plot(dat$Time, dat$Global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type='l')
  dev.off()
}