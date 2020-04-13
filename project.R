setwd("E:/hu510/project")

library(readr)
virus <- read_csv("total-deaths-and-cases-covid-19.csv")


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# USA coronavirus data

usa_data <- subset(virus, Code=="USA")
usa_data$mr <- usa_data$`Total confirmed deaths (deaths)` / usa_data$`Total confirmed cases (cases)`

usa_data <- na.omit(usa_data)

usa_data$weekday <- rep(c(2,3,4,5,6,7,1), times = 30, length = 83)

usa_data <- usa_data[which(usa_data$weekday != 6 & usa_data$weekday != 7),]
usa_data<-usa_data[!(usa_data$Date=="Apr 10, 2020" | usa_data$Date=="Feb 17, 2020"),]

usa_data$confirmed <- as.numeric(usa_data$`Total confirmed cases (cases)`)
usa_data$death <- as.numeric(usa_data$`Total confirmed cases (cases)`)

usa_data$delta_cc <- c(0, tail(usa_data$confirmed, -1) - head(usa_data$confirmed, -1))
usa_data$delta_cc <- normalize(usa_data$delta_cc)

usa_data$beta_cc <- c(0, tail(usa_data$delta_cc, -1) - head(usa_data$delta_cc, -1))
usa_data$beta_cc <- normalize(usa_data$beta_cc)

usa_data$delta_mr <- c(0, tail(usa_data$mr, -1) - head(usa_data$mr, -1))
usa_data$delta_mr <- normalize(usa_data$delta_mr)

usa_data$beta_mr <- c(0, tail(usa_data$delta_cc, -1) - head(usa_data$delta_cc, -1))
usa_data$beta_mr <- normalize(usa_data$beta_mr)


# stock market data

library(BatchGetSymbols)
library(timeSeries)

first.date <- Sys.Date() - 82
last.date <- Sys.Date()
freq.data <- 'daily'

tickers <- c('SPY')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
sub<-na.omit(l.out$df.tickers[,c("ref.date","ticker", "ret.adjusted.prices")])
SPY<-subset(sub, ticker == "SPY")


SPY$absret <- abs(SPY$ret.adjusted.prices)
SPY$absret <- SPY$absret


# combine virus data and stock data

Anova_dataset <- cbind(SPY$absret, usa_data$mr, usa_data$delta_cc, usa_data$delta_mr, usa_data$beta_mr, usa_data$beta_cc)
Anova_dataset <- data.frame(Anova_dataset)

names(Anova_dataset) <- c("vol", "mr", "delta_cc", "delta_mr", "beta_mr", "beta_cc")

# analyze

library(readr)
library(reshape2)
library(moments)
mod<-lm(vol ~ mr*delta_cc*delta_mr*beta_mr*beta_cc, Anova_dataset)

#mod<-lm(vol ~ ., Anova_dataset)

anova(mod)
summary(mod)
resids <- residuals(mod)
preds <- predict(mod)
sq_preds <- preds^2

preds_resids <- data.frame(cbind(resids,preds,sq_preds))

plot(resids ~ preds, data = preds_resids,
     xlab = "Predicted Values",
     ylab = "Residuals")
abline(0,0)

nodeath <- subset(Anova_dataset, mr == 0)
withdeath <- subset(Anova_dataset, mr > 0)

mr_cutoff <- Anova_dataset$mr != 0

Anova_dataset$group <- 0
Anova_dataset$group[mr_cutoff] <- 1

mod<-lm(vol ~ mr*delta_cc*delta_mr*beta_mr*beta_cc + group, Anova_dataset)
anova(mod)
summary(mod)

preds_resids <- data.frame(cbind(resids,preds,sq_preds))

plot(resids ~ preds, data = preds_resids,
     xlab = "Predicted Values",
     ylab = "Residuals")
abline(0,0)
