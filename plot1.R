plot1 <- function() {

    # Read in the data from the text file ..
    # 9 data fields with names separated by semi colon
    # 9 data fields separated by semi colon
    # assume file is in this directory
    df <- read.table('household_power_consumption.txt', header = TRUE, sep = ';')

    # filter for the days 2007-02-01 and 2007-02-02
    df <- subset(df, (Date=='1/2/2007' | Date== '2/2/2007'))
    gap <- as.numeric(as.character(df$Global_active_power))

    # output to plot1.png
    png('plot1.png')
    
    # using the base graphing library in r
    # create a bar chart Frequency ~ Global Active Power
    # color of bars is Red
    # xlab = "Global Active Power (kilowatts)
    # ylabel = "Frequency"
    # main = "Global Active Power"
    hist(gap, col='red', xlab ='Global Active Power (kilowatts)', main ='Global Active Power')
    dev.off()
}

plot1()




