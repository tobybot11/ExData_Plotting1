plot2 <- function() {

    # Read in the data from the text file ..
    # 9 data fields with names separated by semi colon
    # 9 data fields separated by semi colon
    # assume file is in this directory
    df <- read.table('household_power_consumption.txt', header = TRUE, sep = ';')

    # filter for the days 2007-02-01 and 2007-02-02
    df <- subset(df, (Date=='1/2/2007' | Date== '2/2/2007'))

    # add a 'gab' column that is numeric
    df$gab <- as.numeric(as.character(df$Global_active_power))

    # add a unixDateTime column 
    df$unixDateTime <- as.numeric(as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S"))

    # output to plot1.png
    png('plot2.png')

    # date range for the x axis scale
    date_range <- c('1/2/2007', '2/2/2007', '3/2/2007')
    date_range <- as.numeric(as.POSIXct(date_range, format="%d/%m/%Y"))

    # plot Global Active Power vs. Unix Date Time
    # for the plot don't put the date in yet .. type ='n'
    # remove x axis ticks and label and provide a label for the y axis
    plot(gab ~ unixDateTime, data = df,
         type="n", xaxt ='n', xlab='', ylab='Global Active Power (kilowatts)')

    # plot the line graph
    lines(gab ~ unixDateTime, data = df)

    # make  the x axis with the date range provided above
    axis(side = 1,
         at = date_range,
         labels = c('Thu', 'Fri', 'Sat'))
    dev.off()
}

plot2()
