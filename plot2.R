
grepByDateIn <- function(fin, fout) {
  
  fdes_in <- file(fin, "r")
  fdes_out <- file(fout, "w")
  
  header <- readLines(fdes_in, n = 1)
  writeLines(header, fdes_out)
  
  while ( TRUE ) {
    
    line = readLines(fdes_in, n = 1)
    
    if ( length(line) == 0 ) {
      
      break
      
    }
    
    
    if (  length( i <- grep( "^[12]/2/2007", line ))   ) {
    
      writeLines(line, fdes_out)
      
    }
    
  }
  
  close(fdes_out)
  close(fdes_in)
  
}

readByDate <- function( filename, d1, d2, newheaders) {
  
  date1 <- as.Date( strptime(d1, "%D"))
  date2 <- as.Date( strptime(d2, "%D"))
  
  fdes <- file(filename, "r")
  
  headers <- readLines(fdes, n = 1)
  #print( headers )
  
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(df) <- newheaders
  
  while ( TRUE ) {
    
    line = readLines(fdes, n = 1)
    
    if ( length(line) == 0 ) {
      
      break
      
    }
    
    #(d, t, gap, grp, v, gi, sb1, sb2, sb3) <- strsplit(line, ";")
    d <- strsplit(line, ";")[[1]][1]
    t <- strsplit(line, ";")[[1]][2]
    gap <- strsplit(line, ";")[[1]][3]
    grp <- strsplit(line, ";")[[1]][4]
    v <- strsplit(line, ";")[[1]][5]
    gi <- strsplit(line, ";")[[1]][6]
    sb1 <- strsplit(line, ";")[[1]][7]
    sb2 <- strsplit(line, ";")[[1]][8]
    sb3 <- strsplit(line, ";")[[1]][9]
    
    date <- as.Date( strptime(d, "%D"))
    
    if (  date <= date2 && date >= date1  ) {
      
      newrow <- c(date, t, gap, grp, v, gi, sb1, sb2, sb3 )
      #df <- rbind(df[1:r,],newrow,df[-(1:r),])
      newrow
      
    }
    
  }
  
  close(fdes)
  
  return(df)
}
  

cnames <- c("Date", "Time", "GActivePower", "GReactivePower", 
            "Voltage", "GIntensity", "SubMetering1", "SubMetering2",
            "SubMetering3")

fileName <- "./household_power_consumption.txt"
fileOutName <- "./household_power_consumption_filtered.txt"

if( ! file.exists(fileOutName) ) { 

  grepByDateIn(fileName, fileOutName)

}

dat <- read.table(fileOutName, header =TRUE, sep =';', col.names=cnames)

dat$DateTime <- strptime(paste(dat$Date,dat$Time),"%d/%m/%Y %H:%M:%S")

plot( dat$DateTime, dat$GActivePower, type = "l", xlab=NA,
     ylab="Global Active Power (in kilowatt)")

 
png(file = "plot2.png", width = 480, height = 480, units = "px")

plot( dat$DateTime, dat$GActivePower, type = "l", xlab=NA,
      ylab="Global Active Power (in kilowatt)")


dev.off()
