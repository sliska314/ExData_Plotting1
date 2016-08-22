###############################################################################
# Coursera: Exploratory Data Analysis (Aug 15 - Sept 18, 2016)                #
# Week 1: Course Project 1: Generate Plot 4                                   #
###############################################################################


library(data.table)


# Download and Extract Data File
#-----------------------------------------------------------------------------#

# url of zipfile contaning data
DATA_ZIPFILE_URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

# create pathname for temporary copy of zipfile
data_zipfile_path <- tempfile()

# download zipfile
download.file(
  url = DATA_ZIPFILE_URL,
  destfile = data_zipfile_path,
  method = "curl" )

# get the list of files inside the zipfile
zipfile_list <- unzip(
  zipfile = data_zipfile_path,
  list = TRUE )

# check that there is only one file
if ( nrow(zipfile_list) != 1 ) {
  stop( "zipfile must contain exactly one file" )
}

# create pathname for temporary directory
data_dir_path <- tempfile( )

# unzip data file
unzip(
  zipfile = data_zipfile_path,
  files = zipfile_list$Name[1],
  exdir = data_dir_path )

# build data file path
data_file_path <- file.path( data_dir_path, zipfile_list$Name[1] )

# Read, Convert, and Filter Data
#-----------------------------------------------------------------------------#

DATE_INCL_RANGE <- as.Date(
  x = c("2007-02-01","2007-02-02"),
  format = "%Y-%m-%d" )

# read relevant columns to data table
DT <- fread(
    input = data_file_path,
    header = TRUE,
    sep = ";",
    na.strings = "?",
    select = c("Date", "Time", "Global_active_power", "Global_reactive_power",
      "Voltage", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ),
    stringsAsFactors = FALSE )

# convert "Date" from charcters to Date Class
DT[ , Date := as.Date( x = DT$Date, format = "%d/%m/%Y" ) ]

# filter according to date range
DT <- DT[ DATE_INCL_RANGE[1] <= Date & Date <= DATE_INCL_RANGE[2], ]

# build DateTime column
# NOTE: "data.table" does not support POSIXlt
DT[ , DateTime := as.POSIXct(
  x = strptime(
    x = paste( format( x = Date, "%Y-%m-%d" ), Time ),
    format = "%Y-%m-%d %H:%M:%S" ) ) ]

# Plot Data
#-----------------------------------------------------------------------------#

# initialize png device
png( "plot4.png", width = 480, height = 480 )

# set graphical parameters
par( mfcol = c( 2, 2 ) )

# Plot (1,1)
#.............................................................................#

# build line plot
plot(
  x = DT$DateTime,
  y = DT$Global_active_power,
  type = "l",
  xlab = "",
  ylab = "Global Active Power" )

# Plot (2,1)
#.............................................................................#

# build base canvas
plot(
  x = DT$DateTime,
  y = DT$Sub_metering_1,
  type = "n",
  xlab = "",
  ylab = "Energy sub metering" )

# add Sub_metering_1
points(
  x = DT$DateTime,
  y = DT$Sub_metering_1,
  type = "l",
  col = "black" )

# add Sub_metering_2
points(
  x = DT$DateTime,
  y = DT$Sub_metering_2,
  type = "l",
  col = "red" )

# add Sub_metering_3
points(
  x = DT$DateTime,
  y = DT$Sub_metering_3,
  type = "l",
  col = "blue" )

# add legend
legend(
  "topright",
  bty = "n",
  lty = c( 1, 1, 1 ),
  col = c( "black", "red", "blue"),
  legend = c( "Sub_metering_1", "Sub_metering_2", "Sub_metering_3") )

# Plot (1,2)
#.............................................................................#

# build line plot
plot(
  x = DT$DateTime,
  y = DT$Voltage,
  type = "l",
  xlab = "datetime",
  ylab = "Voltage" )

# Plot (2,2)
#.............................................................................#

# build line plot
plot(
  x = DT$DateTime,
  y = DT$Global_reactive_power,
  type = "l",
  xlab = "datetime",
  ylab = "Global_reactive_power" )

# close device
dev.off()