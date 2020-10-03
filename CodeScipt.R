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


###########################################################################################

# In order to calculate averages in multiple columns in R, we first need to melt the data.
# This will make every column after Sector a row and then display its value, essentially
# making the data long instead of wide.

sector_avg <- melt(finviz, id="Sector")
library("reshape")
library("reshape2")
sector_avg <- melt(finviz, id="Sector")
library("XML")
library("ggplot2")
library("zoo")
###########################################3

sector_avg <- subset(sector_avg,variable%in%c("Price","P.E","PEG","P.S","P.B"))
sector_avg

# Removina NA values
sector_avg <- (na.omit(sector_avg))
sector_avg$value <- as.numeric(sector_avg$value)

sector_avg <- dcast(sector_avg, Sector~variable, mean)
colnames(sector_avg)[2:6] <- c("SAvgPE","SAvgPEG","SAvgPS","SAvgPB","SAvgPrice")

#Doing it for industry
industry_avg <- melt(finviz, id=c("Sector","Industry"))
industry_avg <- subset(industry_avg,variable %in%c("Price","P.E","PEG","P.S","P.B"))
industry_avg <- (na.omit(industry_avg))
industry_avg$value <- as.numeric(industry_avg$value)
industry_avg <- dcast(industry_avg, Sector+Industry~variable,mean)
industry_avg <- (na.omit(industry_avg))
colnames(industry_avg)[3:7] <- c("IAvgPE","IAvgPEG","IAvgPS","IAvgPB","IAvgPrice")

# Now we add sector and industry average columns to our original finviz dataset
finviz <- merge(finviz, sector_avg, by.x="Sector", by.y="Sector")
finviz <- merge(finviz, industry_avg, by.x=c("Sector","Industry"), by.y=c("Sector","Industry"))

# The below fields will be used to track whether a stock is undervalued, based on being lower than the sector or industry average
finviz$SPEUnder <- 0
finviz$SPEGUnder <- 0
finviz$SPSUnder <- 0
finviz$SPBUnder <- 0
finviz$SPriceUnder <- 0
finviz$IPEUnder <- 0
finviz$IPEGUnder <- 0
finviz$IPSUnder <- 0
finviz$IPBUnder <- 0
finviz$IPriceUnder <- 0

# we replace the 0s with 1s wherever the respective value for the stock is
# less than the average to indicate that these stocks might be undervalued based
# on that metric
finviz$SPEUnder[finviz$P.E<finviz$SAvgPE] <- 1
finviz$SPEGUnder[finviz$PEG<finviz$SAvgPEG] <- 1
finviz$SPSUnder[finviz$P.S<finviz$SAvgPS] <- 1
finviz$SPBUnder[finviz$P.B<finviz$SAvgPB] <- 1
finviz$SPriceUnder[finviz$Price<finviz$SAvgPrice] <- 1
finviz$IPEUnder[finviz$P.E<finviz$IAvgPE] <- 1
finviz$IPEGUnder[finviz$PEG<finviz$IAvgPEG] <- 1
finviz$IPSUnder[finviz$P.S<finviz$IAvgPS] <- 1
finviz$IPBUnder[finviz$P.B<finviz$IAvgPB] <- 1
finviz$IPriceUnder[finviz$Price<finviz$IAvgPrice] <- 1

# We sum the 10 columns to create new column with index value telling us, on a scale 
# of 1 to 10, how undervalued the stock is based on the different dimensions
finviz$RelValIndex <- apply(finviz[80:89],1,sum)



# Screening stocks and analyzing historical prices

# choose some stock screening criteria, that is, a way to select the stocks within
# the finviz dataset that we feel have the potential to be good investments.

# Only Indian companies
# Price per share between $20 and $100
# Volume greater than 10,000
# Positive earnings per share currently and projected for the future
# Total debt to equity ratio less than 1
# Beta less than 1.5
# Institutional ownership less than 30 percent
# Relative valuation index value greater than 8

target_stocks_india <- subset(finviz, Price>20 & Price<100 &
                            Volume>10000 &
                            Country=="India" &
                            EPS.this.Y>0 &
                            EPS.next.Y>0 &
                            EPS.next.5Y>0 &
                            LTDebt.Eq<1 & Beta<1.5 &
                            Inst.Own<30)
target_stocks_india

# As we see only one Indian companies follow our restrictions we change our country to USA to 
# have more things to calculate.

target_stocks <- subset(finviz, Price>20 & Price<100 &
                                  Volume>10000 &
                                  Country=="USA" &
                                  #EPS.this.Y>0 &
                                  EPS.next.Y>0 &
                                  EPS.next.5Y>0 &
                                  LTDebt.Eq<1 & Beta<1.5 &
                                  Inst.Own<30)
target_stocks

###############################################################333
counter <- 0
for (symbol in target_stocks$Ticker){
    #url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/SCCO?period1=1443830400&period2=1601683200&interval=1d&events=history")
    stock <- read.csv("./Dataset/Companies.csv")
    stock <- na.omit(stock)
    colnames(stock)[7] <- "AdjClose"
    stock[,1] <- as.Date(stock[,1])
        #stock <- cbind(Symbol=symbol,stock)
        maxrow <- nrow(stock)-49
        ma50 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,50,align="right"))
        maxrow <- nrow(stock)-199
        ma200 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,200,align="right"))
        stock <- merge(stock,ma50,by.x=c("Symbol","Date"),by.y=c("Symbol", "Date"),all.x=TRUE)
        colnames(stock)[9] <- "MovAvg50"
        stock <- merge(stock,ma200,by.x=c("Symbol","Date"),by.y=c("Symbol", "Date"),all.x=TRUE)
        colnames(stock)[10] <- "MovAvg200"
        price_chart <- melt(stock[,c(1,2,7,9,10)],id=c("Symbol","Date"))
        qplot(Date, value, data=price_chart, geom="auto", color=variable, ylim = c(0,50), main=paste(symbol,"Daily Stock Prices"),ylab="Price")
        ggsave(filename=paste0("stock_price_",counter,".png"))
        price_summary <- ddply(stock, "Symbol", summarise, open=Open[nrow(stock)], high=max(High), low=min(Low),close=AdjClose[1])
        #price_sum <- group_by(stock, Symbol)
        #price_summary <- summarise( price_sum, open=Open[nrow(stock)], high=max(High), low=min(Low),close=AdjClose[1])
        
        if(counter==0){
            stocks <- rbind(stock)
            price_summaries <- rbind(price_summary)
        }else{
            stocks <- rbind(stocks, stock)
            price_summaries <- rbind(price_summaries, price_summary)
        }
    
    counter <- counter+1
}

qplot(Date, AdjClose, data=stocks, geom="line", color=Symbol, main="Daily Stock Prices")

summary <- melt(price_summaries,id="Symbol")

ggplot(summary, aes(x=variable, y=value, fill=Symbol)) +
    geom_bar(stat="identity") + facet_wrap(~Symbol)


    