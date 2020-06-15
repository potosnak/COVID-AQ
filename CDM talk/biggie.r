# mark potosnak
# 2020-06-05

# talk for CDM
# considering EPA data for 2020 COVID-19 shutdown

# downloaded from:
# https://www.epa.gov/outdoor-air-quality-data/download-daily-data
# Pollutnat: varies
# Year 2020
# Geographic Area IL - Cook
# Monitor Site: 170310076 (ComEd) or 170314201 (Northbrook)

My.Setwd("AoT/COVID-19/CDM talk/")

# store everything in a nested list
dat <- list()
dat[["PM2.5"]] <- list()
dat[["Ozone"]] <- list()

sites <- c("ComEd", "Northbrook")

for(foo in names(dat)) {
   for(bar in sites) {
      x <- read.csv(paste(bar, "-", foo, ".csv", sep=""))
      if(foo == "PM2.5") {x <- x[x$POC ==3,]}
      x$date.formatted <- strptime(x[,'Date'], format="%m/%d/%Y")
      png(paste(bar, "-", foo, ".png", sep=""),
         res=150, width=750, height=750)
      plot(x[,'date.formatted'], x[,5], type='o',
         xlab="Date", ylab=paste(foo, x[1,6]), main=bar)
      dev.off()
      dat[[foo]][[bar]] <- x
   }
}

# look at 2019 data

for(foo in c("Ozone")) {
   for(bar in sites) {
      x <- read.csv(paste(bar, "-", foo, "-2019.csv", sep=""))
      x$date.formatted <- strptime(x[,'Date'], format="%m/%d/%Y")
      png(paste(bar, "-", foo, "2019.png", sep=""),
         res=150, width=750, height=750)
      x1 <- dat[[foo]][[bar]] 
      plot(x1[,'date.formatted'], x1[,5], type='l',
         xlab="Date", ylab=paste(foo, x[1,6]), main=bar)
      lines(365*86400 + x[,'date.formatted'], x[,5], type='l', col=2)
      legend('topleft', c("2019", "2020"), lty=1, col=c(2,1))
      dev.off()
      dat[[foo]][[bar]] <- x
      # do weekly average
      x.2019 <- as.factor(floor(x[,'date.formatted']$yday/7))
      y.2019 <- tapply(x[,5], x.2019, mean)
      x.2020 <- as.factor(floor(x1[,'date.formatted']$yday/7))
      y.2020 <- tapply(x1[,5], x.2020, mean)
      png(paste(bar, "by year.png"),
         res=150, width=750, height=750)
      plot(as.numeric(levels(x.2020)), y.2020, type='o', 
         xlab="Week of year", ylab=paste(foo, x[1,6]), main=bar)
      lines(as.numeric(levels(x.2019)), y.2019, type='o', col=2, pch=1)
      legend('topleft', c("2019", "2020"), lty=1, col=c(2,1))
      dev.off()
   }
}
