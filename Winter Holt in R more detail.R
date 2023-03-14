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
ts()?
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
#Here we start using winter holt, we also have a alpha parameter to make adjustments to the model
rainseriesforecasts
rainseriesforecasts$fitted
plot(rainseriesforecasts)
rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)



library("forecast")
#this library had trouble loading
#moe info at https://rdrr.io/cran/forecast/man/forecast.HoltWinters.html
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

#this was a matrix with quite a few NA values so I needed to add the replace() 
#and the is.na() to turn NA values into zero so the algorithm could operate without error
plotForecastErrors(replace(rainseriesforecasts2$residuals, is.na(rainseriesforecasts2$residuals), 0))



# function (x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, 
#           type = 7, digits = 7, ...) 
# {
#   if (is.factor(x)) {
#     if (is.ordered(x)) {
#       if (!any(type == c(1L, 3L))) 
#         stop("'type' must be 1 or 3 for ordered factors")
#     }
#     else stop("(unordered) factors are not allowed")
#     lx <- levels(x)
#     x <- as.integer(x)
#   }
#   else {
#     if (is.null(x)) 
#       x <- numeric()
#     lx <- NULL
#   }
#   if (na.rm) 
#     x <- x[!is.na(x)]
#   else if (anyNA(x)) 
#     stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
#   eps <- 100 * .Machine$double.eps
#   if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 
#                                      1 + eps))) 
#     stop("'probs' outside [0,1]")
#   n <- length(x)
#   probs <- pmax(0, pmin(1, probs))
#   np <- length(probs)
#   {
#     if (type == 7) {
#       index <- 1 + max(n - 1, 0) * probs
#       lo <- floor(index)
#       hi <- ceiling(index)
#       x <- sort(x, partial = if (n == 0) 
#         numeric()
#         else unique(c(lo, hi)[p.ok]))
#       qs <- x[lo]
#       i <- which(!p.ok | (index > lo & x[hi] != qs))
#       h <- (index - lo)[i]
#       qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
#     }
#     else {
#       if (type <= 3) {
#         nppm <- if (type == 3) 
#           n * probs - 0.5
#         else n * probs
#         j <- floor(nppm)
#         h <- switch(type, !p.ok | (nppm > j), ((nppm > 
#                                                   j) + 1)/2, !p.ok | (nppm != j) | ((j%%2L) == 
#                                                                                       1L))
#       }
#       else {
#         switch(type - 3, {
#           a <- 0
#           b <- 1
#         }, a <- b <- 0.5, a <- b <- 0, a <- b <- 1, 
#         a <- b <- 1/3, a <- b <- 3/8)
#         fuzz <- 4 * .Machine$double.eps
#         nppm <- a + probs * (n + 1 - a - b)
#         j <- floor(nppm + fuzz)
#         h <- nppm - j
#         if (any(sml <- abs(h) < fuzz, na.rm = TRUE)) 
#           h[sml] <- 0
#       }
#       x <- sort(x, partial = if (n == 0) 
#         numeric()
#         else unique(c(1, j[p.ok & j > 0L & j <= n], (j + 
#                                                        1)[p.ok & j > 0L & j < n], n)))
#       x <- c(x[1L], x[1L], x, x[n], x[n])
#       qs <- x[j + 2L]
#       qs[!is.na(h) & h == 1] <- x[j + 3L][!is.na(h) & 
#                                             h == 1]
#       other <- (0 < h) & (h < 1) & (x[j + 2L] != x[j + 
#                                                      3L])
#       other[is.na(other)] <- TRUE
#       if (any(other)) 
#         qs[other] <- ((1 - h) * x[j + 2L] + h * x[j + 
#                                                     3L])[other]
#     }
#   }
#   if (is.character(lx)) 
#     qs <- factor(qs, levels = seq_along(lx), labels = lx, 
#                  ordered = TRUE)
#   if (names && np > 0L) {
#     stopifnot(is.numeric(digits), digits >= 1)
#     names(qs) <- format_perc(probs, digits = digits)
#   }
#   qs
# }  