###############################################################################
# Coursera: Exploratory Data Analysis (Aug 15 - Sept 18, 2016)                #
# Week 1: Course Project 1: Generate Plot 2                                   #
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
    select = c("Date", "Time", "Global_active_power"),
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
png( "plot2.png", width = 480, height = 480 )

# build line plot
plot(
  x = DT$DateTime,
  y = DT$Global_active_power,
  type = "l",
  xlab = "",
  ylab = "Global Active Power (kilowatts)" )

# close device
dev.off()