# # To keep winter holt strictly positive may be able to use R's forecast package 
#   that includes a lambda argument - when true, a Box-Cox transformation is used 
#   that will keep the forecast strictly positive.
# # "To address the part of your question related to R, the ets function from the 
#   forecast package includes a lambda argument -- when true, a Box-Cox 
#   transformation is used that will keep the forecasts strictly positive. You may 
#   be able to use the same general approach in Java."
# # answered Jul 12, 2014 at 21:38
# # https://stats.stackexchange.com/a/107735
# # --https://stats.stackexchange.com/questions/107467/avoid-negative-results-in-holt-winters-forecasting

#Bizforecaster has a few posts related to Winter Holt Forecasting and using the 
#Box Cox lambda to avoid negative values (essentially it is tranforming the data with the lambda value

#This link gives a Tutorial on Winter Holt forecasting in R:
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#holt-winters-exponential-smoothing


library(forecast)


#ex. 1  Basic level timeseries
    kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
    kings
    kingstimeseries <- ts(kings)
    #Documentation for ts() = time series can be found by typing help(function you need documentation for)
    help(ts)
      # ts(data = NA, start = 1, end = numeric(), frequency = 1,
      #    deltat = 1, ts.eps = getOption("ts.eps"), class = , names = )
      # as.ts(x, .)
      # is.ts(x)
    kingstimeseries
    plot.ts(kingstimeseries)
    
    #decomposing time series = estimating the the trend component and the irregular component
    library("TTR") 
    
    #for SMA()=simple moving average  for smoothing
    kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
    plot.ts(kingstimeseriesSMA3)
    
    #more smoothing n=8
    kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
    plot.ts(kingstimeseriesSMA8)
    





#ex. 2 specify start slightly more insightful
    births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
    birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
    plot.ts(birthstimeseries)
    
    birthstimeseriescomponents <- decompose(birthstimeseries)  
    ##I really like the decompose function that pulls apart the different impacts on the data
    plot(birthstimeseriescomponents)
    
    birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
    plot(birthstimeseriesseasonallyadjusted)


#ex. 3 getting into Winter Holt for real
    rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
    rainseries <- ts(rain,start=c(1813))
    plot.ts(rainseries)
    
    rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
    #Here we start using winter-holt, we also have a alpha parameter to make adjustments to the model
    rainseriesforecasts
    rainseriesforecasts$fitted
    plot(rainseriesforecasts)
    rainseriesforecasts$SSE
    HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
    
    #the install.packages() is another method for loading a package instead of using the menu
    install.packages("forecast")
    library("forecast")
      # this library had trouble loading
      # moe info at https://rdrr.io/cran/forecast/man/forecast.HoltWinters.html
    rainseriesforecasts2 <- forecast:::forecast.HoltWinters(rainseriesforecasts, h=8)
      #note the colons this is a modification from the tutorial
      #the colons are needed with certain functions at times(this is not typical if 
      #the library and package are installed)
    forecast:::plot.forecast(rainseriesforecasts2)
    
    
    #this is where we get into model selection
    acf_plot <- Acf(rainseriesforecasts2$residuals, lag.max=20)
      #a lot of these functions (maybe all) are case sensitive;
      #the above was listed in the tutorial
    
    
    Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
    plot.ts(rainseriesforecasts2$residuals)
    
    #This is setting up a function called plotForecastErrors() that will be used to make a histogram
      plotForecastErrors <- function(forecasterrors)
      {
        # make a histogram of the forecast errors:
        mybinsize <- IQR(forecasterrors)/4
        mysd   <- sd(forecasterrors)
        mymin  <- min(forecasterrors) - mysd*5
        mymax  <- max(forecasterrors) + mysd*3
        # generate normally distributed data with mean 0 and standard deviation mysd
        mynorm <- rnorm(10000, mean=0, sd=mysd)
        mymin2 <- min(mynorm)
        mymax2 <- max(mynorm)
        if (mymin2 < mymin) { mymin <- mymin2 }
        if (mymax2 > mymax) { mymax <- mymax2 }
        # make a red histogram of the forecast errors, with the normally distributed data overlaid:
        mybins <- seq(mymin, mymax, mybinsize)
        hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
          # freq=FALSE ensures the area under the histogram = 1
          # generate normally distributed data with mean 0 and standard deviation mysd
        myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
        # plot the normal curve as a blue line on top of the histogram of forecast errors:
        points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
      }
    
    #this was a matrix/data.frame/table with quite a few NA values so I needed to add the replace() 
    #and the is.na() to turn NA values into zero so the algorithm could operate without error
    plotForecastErrors(replace(rainseriesforecasts2$residuals, is.na(rainseriesforecasts2$residuals), 0))



###-----------Lets apply this to stuff that we do----------------------------###
    
#Try to apply this to the IIC forecast
rm()
gc()
install.packages("tidyverse")
library(readr)
library(lubridate)
#make sure you are connected through VPN before changing directories to a shared drive
setwd("W:/Wendy/R Folder")
IIC_18_22 <- read_csv(
  "2018-2022 IIC Completed by Hour Historical from Online Claim IC SA no Auto.csv")


#Inspect the data
#View(IIC_18_22)
#looking at the console pane will tell you the data type of each variable
# -- Column specification ------------------------------------------------------
#   Delimiter: ","
# chr (3): All_Done_Code, Applicant_Complete_Date, MAX(Applicant_Complete_Time_Stamp)
# dbl (2): EXTRACT(_HOUR_FROM_Applicant_Complete_Time_Stamp), COUNT(DISTINCT_Confirmation_Number)

#--------------------Convert data classes from input file-----------------------

#set up variables that we will use in multiple ways
#If you want to assign non-rounded values to applicant complete time use this:
Applicant_Complete_Time_Stamp <- trunc(
  as.POSIXct(
    as.character.Date(IIC_18_22[[5]]), 
             format="%m/%d/%Y %H:%M", tz = "America/Los_Angeles"), 
  units = "hours")


DISTINCT_Confirmation_Number <- as.numeric(IIC_18_22[[3]])
#CHECK TIME NOW and confirm it is what you think it is - some POSIXct() functionality 
#relies on confirming that the system date matches what you think; for this next 
#section we don't need to worry, but it is good practice

#Sys.time()
#CONFIRM that system time matches with current local time

  #make sure delimiter in the format = part of the function as.POSIXct matches the input file
#Use columns from file for forecast IIC_18_22; as.POSIXct gives a date time class = POSIXct
IIC_Time<-data.frame(
          Applicant_Complete_Time_Stamp = Applicant_Complete_Time_Stamp,
          DISTINCT_Confirmation_Number = DISTINCT_Confirmation_Number)

View(IIC_Time)
# We are using column 5 = date time and column 3 = the incoming IIC volume from file import 
# We will merge on rounded hourly values where Midnight to 1 am are time = 00:00
# The data frame grabbed the two columns we indicated from the import file and renamed them to:


# Applicant_Complete_Time_Stamp
# class(DISTINCT_Confirmation_Number)


######USE THIS to Create Full Interval List to Merge Input File Onto ###########
#open library to use vec_rep() which
library(readr)

install.packages("tidyverse")
install.packages("validate")
library(lubridate)


#---User Prompted or Use Max and Min from File - Choose Here -------------------
#get start and end date from user
# End.Date <- readline(prompt="Enter End Date in YYYY-MM-DD format:")
# Start.Date <- readline(prompt="Enter Start Date in YYYY-MM-DD format:")

#Get Min [1] and Max [2] for the Applicant Complete Dates
App_dt_Range <- range(Applicant_Complete_Time_Stamp)

# #create 24 hour vector to repeat for each day of date range
Int_1hr <- data.frame(Applicant_Complete_Time_Stamp = as.POSIXct(format(
  seq.POSIXt(App_dt_Range[1], App_dt_Range[2], 
             by = "60 min"), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")))

View(Int_1hr)
dim.data.frame(Int_1hr)
dim.data.frame(IIC_Time)

class(Int_1hr$Applicant_Complete_Time_Stamp)
class(IIC_Time$Applicant_Complete_Time_Stamp)
Int1HR_IICVol <- merge(Int_1hr, IIC_Time, by="Applicant_Complete_Time_Stamp", all=TRUE)
dim.data.frame(Int1HR_IICVol)
View(Int1HR_IICVol)


#need to fix this for POSIX since it might be looking for some other class------
class(Int1HR_IICVol$Applicant_Complete_Time_Stamp)
Int1HR_IICVol <- replace(
  Int1HR_IICVol$Applicant_Complete_Time_Stamp, 
  is.na(
    Int1HR_IICVol$Applicant_Complete_Time_Stamp), 0)
#--To Do:-----------------------------------------------------------------------
#merge Int_1hr onto IIC_Time
#-------------------------------------------------------------------------------




#--Notes:-----------------------------------------------------------------------
  ##These were all the things I tried#
      #Applicant_Complete_Time_Stamp <-as.POSIXct(IIC_18_22[[5]],"%m/%d/%Y %H:%M")
      # Applicant_Complete_Time_Stamp <- as.Date(IIC_18_22[[5]],"%m/%d/%Y %H:%M")
      # Applicant_Complete_Time_Stamp <- as.character.Date(IIC_18_22[[5]])
      # Applicant_Complete_Time_Stamp <- as.character.Date(IIC_18_22[[5]])
      # Applicant_Complete_Time_Stamp <- as.POSIXct(Applicant_Complete_Time_Stamp,format="%m/%d/%Y %H:%M")
      
      # Applicant_Complete_Time_Stamp<- as.POSIXct(as.numeric(as.character(IIC_18_22[[4]])), origin="01/01/2018")
      # Applicant_Complete_Time_Stamp
          #might want to use this later https://tidyr.tidyverse.org/reference/separate.html
          # separate(
          #   IIC_18_22,
          #   4,
          #   ,
          #   sep = "[^[:alnum:]]+",
          #   remove = TRUE,
          #   convert = FALSE,
          #   extra = "warn",
          #   fill = "warn",
          #   ...
          # ); library(tidyselect) ^^
          
          ## OR maybe something like
          
          #as.Date(Applicant_Complete_Time_Stamp, format = '%Y-%m-%d %H:%M:%S')

 
# library(vctrs)
# #create 24 hour vector to repeat for each day of date range
# Int_1hr <- format( 
#   seq.POSIXt(as.POSIXct(Sys.Date()), 
#              as.POSIXct(Sys.Date()+1), by = "60 min"), 
#                       "%H:%M", tz="PST") 


#create 24 hour datetime list for date range
# timehhmm <- data.frame(vec_rep(Int_1hr[1:24], as.integer(difftime(End.Date, Start.Date, units = "days"))))
# View(timehhmm)
#-------------------------------------------------------------------------------

