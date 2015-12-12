plot4 <- function() {
    # Read in the data from the text file ..
    # 9 data fields with names separated by semi colon
    # 9 data fields separated by semi colon
    # assume file is in this directory
    df <- read.table('household_power_consumption.txt', header = TRUE, sep = ';')

    # filter for the days 2007-02-01 and 2007-02-02
    df <- subset(df, (Date=='1/2/2007' | Date== '2/2/2007'))

    # add a 'gap' column that is numeric
    df$gap <- as.numeric(as.character(df$Global_active_power))
    
    # add a 'grp' column that is numeric
    df$grp <- as.numeric(as.character(df$Global_reactive_power))

    # add a 'v' column that is numeric based on Voltage
    df$v <- as.numeric(as.character(df$Voltage))

    # add a 'sm1', 'sm2', and 'sm3' column that is numeric
    df$sm1 <- as.numeric(as.character(df$Sub_metering_1))
    df$sm2 <- as.numeric(as.character(df$Sub_metering_2))
    df$sm3 <- as.numeric(as.character(df$Sub_metering_3))

    # add a unixDateTime column 
    df$unixDateTime <- as.numeric(as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S"))

    # date range for the x axis scale
    date_range <- c('1/2/2007', '2/2/2007', '3/2/2007')
    date_range <- as.numeric(as.POSIXct(date_range, format="%d/%m/%Y"))

    # Output to plot4.png
    png('plot4.png')

    # need to create the par grid first
    par(mfrow=c(2,2))

    ############## Plot 1
    # plot Global Active Power vs. Unix Date Time
    # for the plot don't put the date in yet .. type ='n'
    # remove x axis ticks and label and provide a label for the y axis
    plot(gap ~ unixDateTime, data = df,
         type="n", xaxt ='n', xlab='', ylab='Global Active Power')

    # plot the line graph
    lines(gap ~ unixDateTime, data = df)

    # make  the x axis with the date range provided above
    axis(side = 1,
         at = date_range,
         labels = c('Thu', 'Fri', 'Sat'))
    
    ############## Plot 2
    # plot Voltage vs. Unix Date Time
    # for the plot don't put the date in yet .. type ='n'
    # remove x axis ticks and label and provide a label for the y axis
    plot(v ~ unixDateTime, data = df,
         type="n", xaxt ='n', xlab='datetime', ylab='Voltage')

    # plot the line graph
    lines(v ~ unixDateTime, data = df)

    # make  the x axis with the date range provided above
    axis(side = 1,
         at = date_range,
         labels = c('Thu', 'Fri', 'Sat'))

    ############### Plot 3
    # plot Sub_metering_1,2,3 vs. Unix Date Time
    # for the plot don't put the date in yet .. type ='n'
    # remove x axis ticks and label and provide a label for the y axis
    plot(sm1 ~ unixDateTime, data = df,
         type="n", xaxt ='n', xlab='', ylab='Energy sub metering')

    # plot the line graphs for each Sub metering data set
    lines(sm1 ~ unixDateTime, data = df, col="black")
    lines(sm2 ~ unixDateTime, data = df, col="red")
    lines(sm3 ~ unixDateTime, data = df, col="blue")

    # make the x axis with the date range provided above
    axis(side = 1,
         at = date_range,
         labels = c('Thu', 'Fri', 'Sat'))

    # add legend ..  added bty='n' to get rid of the border
    legend("topright",
           c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),
           col=c('black','red', 'blue'),
           lty=c(1,1,1), bty='n')

    ############### Plot 4
    # plot Global Reactive Power vs. Unix Date Time
    # for the plot don't put the date in yet .. type ='n'
    # remove x axis ticks and label and provide a label for the y axis
    plot(grp ~ unixDateTime, data = df,
         type="n", xaxt ='n', xlab='datetime', ylab='Global_Reactive_Power')

    # plot the line graphs for grb
    lines(grp ~ unixDateTime, data = df)

    # make the x axis with the date range provided above
    axis(side = 1,
         at = date_range,
         labels = c('Thu', 'Fri', 'Sat'))
    
    # turn graphics device off
    dev.off()

}

plot4()




