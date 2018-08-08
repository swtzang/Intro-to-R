rm(list=ls())
library(devtools)
devtools::install_github("Displayr/flipAPI")

#install_github("Displayr/flipAPI")
#cola1 = DownloadXLSX("https://wiki.q-researchsoftware.com/images/b/b9/Cola_Discriminant_Functions.xlsx", want.col.names = TRUE, range = "A2:G9")
#cola2 = DownloadXLSX("https://wiki.q-researchsoftware.com/images/b/b9/Cola_Discriminant_Functions.xlsx", want.col.names = TRUE, want.row.names = FALSE, sheet = 2, range = "AB2:AC330"

data(mtcars)
head(mtcars)
 

head(iris)                    
write.table(iris, file='iris.csv',sep=',')

#=================================================================
library(XML)
url <- "http://en.wikipedia.org/wiki/FIFA_World_Cup"
tab <- readHTMLTable(url, stringsAsFactors=FALSE)
names(tab)
#===============================================================
library(quantmod)
from.dat <- as.Date("2018/01/01", format="%Y/%m/%d")
aapl <-getSymbols("AAPL", auto.assign = F, from  = from.dat)
saveRDS(aapl, 'aapl_18.rds')
aapl3<-readRDS('aapl_18.rds')
head(aapl)
aapl.close<-aapl[,6]
aapl.ret<-dailyReturn(aapl.close)
plot(aapl.ret)
hist(aapl.ret)
chartSeries(aapl)
addMACD()
addRSI()
#-------------------------------------------------------
from.dat <- as.Date("2010/01/01", format="%Y/%m/%d")
tsmc <-getSymbols("2330.TW", auto.assign = F, from  = '2010/01/01')
dim(tsmc)
twii<-getSymbols("^TWII", auto.assign = F, from  = '2010/01/01')
dim(twii)
#=======================================================
symbols <- stockSymbols()
head(symbols)
dim(symbols)
nasdaq<-symbols[symbols$Exchange == 'NASDAQ',]
head(nasdaq)
dim(nasdaq)
class(nasdaq)
#=========================================================
library(rvest)
html <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(html, "span.itemprop")
html_text(cast)

cast1 <- html_nodes(html, "#titleCast .itemprop")
length(cast1)
dim(cast1)
cast1[1:2]
cast1[[1]]
html_text(cast1)
#----------------------------------------------------------
cast2 <- html_nodes(html, "#titleCast span.itemprop")
cast2
length(cast2)
html_text(cast2)

#-----------------------------------------------------------
# try to scrap tables from web
#-----------------------------------------------------------


#=========================================\
x=0
for (i in 1:50){
  x=x+i
}
print(x)
#==========================================
library(magrittr)
xd<- data.frame(quant=1:10,ch=sample(letters[1:4],10,replace = T),
                price=sample(c(10000,20000,30000),10,replace = T)) 
xd
xd$Sale<-with(xd,price*quant) 
xd$Sale<-xd$quant*xd$price 
xd

xd1<-xd[,-2]  # ignore character vector
xd1
stack(xd)
xd_long<-stack(xd1)
xd_long
xd_short<-unstack(xd_long)
xd_short

#========================================
# aggregate()
#=========================================

aggregate(price~ch,xd, FUN = mean)
aggregate(xd$price, by = list(xd$ch), FUN = mean)
library(plyr)
aggregate(xd$price, by = list(xd$ch), each(mean, sd))

aggregate(price~ch,xd, each(mean, sd))

aggregate(price~ch,xd,each(mean,sd, moments))


#------------------------------------------------
d.f <- data.frame(rating = c("AAA", "A", "A", "AAA", 
                             "BB", "BB", "AAA", "A"))
d.f
i <- 1
by <- d.f$rating
sub.data.frame <- d.f[by == unique(by)[i], ]
sub.data.frame
#
dates <- data.frame(date = as.Date("2001-01-01", format = "%Y-%m-%d") + 0:729)
dates
last.day <- aggregate(x = dates["date"], 
                      by = list(month = substr(dates$date, 1, 7)), 
                      FUN = max)
last.day

#---------------------------------------------------------------
assets <- data.frame(asset.class = c("equity", "equity","equity",
                                     "option","option","option",
                                     "bond", "bond"),
                     rating = c("AAA", "A", "A", "AAA", 
                                "BB", "BB", "AAA", "A"),
                     counterparty.a = c(runif(3), rnorm(5)),
                     counterparty.b = c(runif(3), rnorm(5)),
                     counterparty.c = c(runif(3), rnorm(5)))
assets

library(MASS)
categories <- data.frame(category = c("a", "a", "a", "a", "a", 
                                      "b", "b", "b", "b", "b",
                                      "c", "c", "c", "c"))


observations <- data.frame(observation = c(rnorm(5, mean = 3, sd = 0.2),
                                           rnorm(5, mean = -2, sd = 0.4),
                                           rnorm(4, mean = 0, sd = 1)))

distr.estimate <- aggregate(x = observations, 
                            by = categories,
                            FUN = function(observations){
                              fitdistr(observations, 
                                       densfun = "normal")$estimate
                            })

distr.estimate

distr.estimate$observation[1,][["mean"]]

#
attach(mtcars)
mtcars
aggregate(mtcars, by=list(cyl), FUN=mean)

aggregate(mtcars, by=list(cyl, gear), FUN=mean)

aggregate(cbind(mpg,hp) ~ cyl+gear, FUN=mean)

# apply
dataset1 <- cbind(observationA = 16:8, observationB = c(20:19, 6:12)) 
dataset1

apply(dataset1, 1, mean)
apply(dataset1, 2, sort)
apply(dataset1, 1, prod)

DerivativeFunction <- function(x) { log10(x) + 1 } 
apply(dataset1, 2, DerivativeFunction)

# 
library(XML)
library(RCurl)
curlVersion()$features
curlVersion()$protocol
##These should show ssl and https. I can see these on windows 8.1 at least. 
##It may differ on other OSes.
url = "https://en.wikipedia.org/wiki/World_population"
#tables <- readHTMLTable(url)
xData <- getURL(url, ssl.verifyPeer=FALSE)
DFX <- xmlTreeParse(temp,useInternal = TRUE)
doc <- xmlParse(xData, useInternal = TRUE)
str(doc)
doc
