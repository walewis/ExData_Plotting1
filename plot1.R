## setwd(readClipboard())

## install.packages("stringr","ggplot2","lattice","dplyr","png")

## library(stringr,ggplot2,dplyr,png)

z <- readLines("household_power_consumption.txt")

w <- z[grep("^[1-2]{1}/2/2007",y)]

e <- gsub(";"," ",w)

l <- length(w)

d <- vector("character", length=l)
t <- vector("character", length=l)
x1 <- vector("character", length=l)
x2 <- vector("character", length=l)
x3 <- vector("character", length=l)
x4 <- vector("character", length=l)
x5 <- vector("character", length=l)
x6 <- vector("character", length=l)
x7 <- vector("character", length=l)

for (i in 1:l) {
    d[i] <- substr(e[i], 1, str_locate(e[i],"([^ ]+ ){1}")[2]-1)
    t[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){1}")[2]+1, str_locate(e[i],"([^ ]+ ){2}")[2]-1)
    x1[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){2}")[2]+1, str_locate(e[i],"([^ ]+ ){3}")[2]-1)
    x2[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){3}")[2]+1, str_locate(e[i],"([^ ]+ ){4}")[2]-1)
    x3[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){4}")[2]+1, str_locate(e[i],"([^ ]+ ){5}")[2]-1)
    x4[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){5}")[2]+1, str_locate(e[i],"([^ ]+ ){6}")[2]-1)
    x5[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){6}")[2]+1, str_locate(e[i],"([^ ]+ ){7}")[2]-1)
    x6[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){7}")[2]+1, str_locate(e[i],"([^ ]+ ){8}")[2]-1)
    x7[i] <- substr(e[i], str_locate(e[i],"([^ ]+ ){8}")[2]+1, nchar(e[i]))
}

df1 <- data.frame(d,t,x1,x2,x3,x4,x5,x6,x7)

names(df1) <- c('Date', 'Time', 'Global_active_power', 'Global_reactive_power', 'Voltage', 'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')

df1 <- transform(df1,Date=as.Date(Date,"%d/%m/%Y"), Time=as.character(Time), Global_active_power=as.numeric(x1),Global_reactive_power=as.numeric(x2),Voltage=as.numeric(x3),Global_intensity=as.numeric(x4),Sub_metering_1=as.numeric(x5),Sub_metering_2=as.numeric(x6),Sub_metering_3=as.numeric(x7))

df2 <- select(df1,Date,Time)

v <- paste(df2[[1]],df2[[2]])

Date_Time <- strptime(v, "%Y-%m-%d %H:%M:%S")

df3 <- data.frame(Date_Time)

df4 <- select(df1,-(Date:Time))

df5 <- data.frame(df3,df4)

head(df5)


png("plot1.png", width=480, height=480, bg="transparent")

hist(df5$Global_active_power, col='red', main="Global Active Power",xlab="Global Active Power (kilowatts)")

dev.off()

hist(df5$Global_active_power, col='red', main="Global Active Power",xlab="Global Active Power (kilowatts)")

