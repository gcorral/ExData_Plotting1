

setwd("C:/Users/user/Documents/ExploratoryDataAnalysis/ExData_Plotting1")

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

localfile <- "HouseHoldPowerConsumption.zip"

if ( ! file.exists(paste( "./", localfile, sep = "" ) ) ) {
  
  download.file(url, "./HouseHoldPowerConsumption.zip")
  
}

if ( ! file.exists( "household_power_consumption.txt" )) {

  unzip("./HouseHoldPowerConsumption.zip")
  
}

