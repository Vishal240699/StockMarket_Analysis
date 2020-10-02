#Loading the required packages
library(XML)
library(ggplot2)
library(dplyr)
library(reshape2)
library(zoo)

####The dataset was imported and stored by variable name finviz
finviz <- read.csv("Dataset/unique_30092020.csv")

#####Summarising the data
head(finviz)
summary(finviz)

#####Cleaning the data
clean_numeric <- function(s){
    s <- gsub("%|\\$|,|\\)|\\(", "", s)
    s <- as.numeric(s)
}

finviz <- cbind(finviz[,1:6],apply(finviz[,7:69], 2, clean_numeric))
warnings()

head(finviz)

hist(finviz$Price, breaks=100, main="Price Distribution", xlab="Price")
# Due to higher prices of outlier stocks graph seems usesless, So
# we would have to use a cap on the x axis

hist(finviz$Price[finviz$Price<150], breaks=100, main="Price Distribution", xlab="Price")
# This graph is better than previous one.

sector_avg_prices <- aggregate(Price~Sector,data=finviz,FUN="mean")
colnames(sector_avg_prices)[2] <- "Sector_Avg_Price"

ggplot(sector_avg_prices, aes(x=Sector, y=Sector_Avg_Price,fill=Sector)) +
    geom_bar(stat="identity") + ggtitle("Sector Avg Prices") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# First, we create a summary of the average prices by industry:
industry_avg_prices <- aggregate(Price~Sector+Industry,data=finviz,FUN="mean")
industry_avg_prices <- industry_avg_prices[order(industry_avg_prices$Sector,industry_avg_prices$Industry),]
colnames(industry_avg_prices)[3] <- "Industry_Avg_Price"

# We isolate the industries in the financial sector:
industry_chart <- subset(industry_avg_prices,Sector=="Financial")

# We create a chart showing avg prices of industry in financial sector
ggplot(industry_chart, aes(x=Industry, y=Industry_Avg_Price, fill=Industry)) +
    geom_bar(stat="identity") + theme(legend.position="none") +
    ggtitle("Industry Avg Prices") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Going deeper into the Insurance Diversified Industry
company_chart <- subset(finviz,Industry=="Insurance - Diversified")

ggplot(company_chart, aes(x=Company, y=Price, fill=Company)) +
    geom_bar(stat="identity") + theme(legend.position="none") +
    ggtitle("Company Avg Prices") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Currently the stock price for Berkshire Hathaway Inc. is  greter than 3e+05 per share.

#Lets remove them from our list and see what happens
# Ticker(coloumn 2) for Berkshire is "BRK-A"

finviz <- subset(finviz, Ticker!="BRK-A")

sector_avg_prices <-aggregate(Price~Sector,data=finviz,FUN="mean")
colnames(sector_avg_prices)[2] <- "Sector_Avg_Price"

#Plotting the graph again
ggplot(sector_avg_prices, aes(x=Sector, y=Sector_Avg_Price, fill=Sector)) +
    geom_bar(stat="identity") + ggtitle("Sector Avg Prices") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

