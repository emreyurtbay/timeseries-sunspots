library(astsa)
library(quantmod)
library(timeSeries)
library(knitr)
library(broom)
library(forecast)
library(depmixS4)

dat <- read.csv("sunspotdata.csv")
y <- ts(dat[,2], frequency = 1, start = 1700)
forecast::autoplot(y, main = "Yearly Sunspot Counts", xlab = "Year", ylab = "Sunspot Counts")

par(mfrow = c(1, 2))
hist(y)
hist(log(y))

invisible(astsa::acf2(y))

#########################
# HMM
#########################
set.seed(69)
y <- sunspot.year
mod3 <-depmix(y~1, nstates=3, family=gaussian(), data=data.frame(y))
fm2 <-fit(mod3)
summary(fm2 <-fit(mod3))


matrix(c("pr1", "pr2", "pr3",
         0,0,1), nrow = 2, byrow = T) %>% knitr::kable()

matrix(c("", "toS1",  "toS2",  "toS3", 
         "fromS1", 0.799,  0.221,  0.000,
         "fromS2", 0.283, 0.425, 0.292,
         "fromS3", 0.000, 0.303, 0.697
), nrow = 4, byrow = T) %>% knitr::kable()

# data and states
plot(y, main="", ylab='Sunspot Count', type='h', col=gray(.7))
text(y, col=6*posterior(fm2)[,1]-2, labels=posterior(fm2)[,1], cex=.9)

# prob of state 2
plot(ts(posterior(fm2)[,3], start=1900), ylab = expression(hat(pi)[~2]*'(t|n)'));
abline(h=.5, lty=2)
##

#######
## SARIMA
####
model <- auto.arima(y)
broom::tidy(model) %>% knitr::kable()

# Plotting
sarima.for(y, n.ahead = 11,
           p = 3, d = 1, q = 2)

broom::tidy(sarima.for(y, n.ahead = 11,
                       p = 3, d = 1, q = 2)$pred) %>% knitr::kable()