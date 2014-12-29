library(lubridate)
library(ggplot2)

# Load data from 1997 to 2013 for CA, Santa Clara
# http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm
# State and county codes
# https://aqs.epa.gov/aqsweb/codes/data/StateCountyCodes.html
# CA: 06, Santa Clara: 085, San Mateo: 081
load_data <- function() {
    data <- data.frame()
    years <- c(1997:2013)
    file_prefix <- "RD_501_88101_"
    file_suffix <- "-0.txt"
    sapply(years, FUN = function(year) {
        file_name <- paste0(file_prefix, year, file_suffix)
        pm <- read.table(file_name, sep="|", na.strings="")
        cnames <- readLines(file_name, 1)
        cnames <- strsplit(cnames, "|", fixed=TRUE)
        names(pm) <- make.names(cnames[[1]])
        pm <- pm[pm$State.Code == 6 & (pm$County.Code == 85 | pm$County.Code == 81),
                 c("Date", "Sample.Value", "County.Code")]
        data <<- rbind(data, pm)
    })
    data$Date <- ymd(data$Date)
    data$Year <- year(data$Date)
    data$Month <- month(data$Date)
    data$County.Code <- factor(data$County.Code, 
                                  levels = c(81, 85), 
                                  labels = c("San Mateo", "Santa Clara"))
    data <- data[!is.na(data$Sample.Value), ]
    
    data
}

plot_data <- function(data) {
    png("sm_sc_pm25.png", width = 640, height = 480)
    dataByYear <- summarise(group_by(data, Year, County.Code), mean(Sample.Value))
    names(dataByYear) <- c("Year", "County.Code", "MeanPM2.5")
    plot <-
        ggplot(dataByYear) +
        aes(Year, MeanPM2.5, fill = County.Code) + 
        facet_wrap(~ County.Code) + 
        geom_bar(stat = "identity") +
        xlab("Years") +
        ylab("Mean PM2.5 Levels (μg/m3)") +
        ggtitle("Mean PM2.5 Levels in Santa Clara and San Mateo counties, California") +
        stat_smooth(method = "loess", se = FALSE)
    print(plot)
    dev.off()
}
