data<-read.csv('train.csv', header = TRUE, sep=',', stringsAsFactors=FALSE)
View(data)

co_li<-c(data['Store'], data['DayOfWeek'], data['Date'], data['Sales'], data['Customers'], data['Open'], data['Promo'], data['StateHoliday'], data['SchoolHoliday'] )
checkna<-function(col) { 
  i=1   
  for (co in col){        
    cat('Column: ', names(col[i]),', NA Values: ',sum(is.na(co)), '\n')
    i=i+1
  }
}
func<-checkna(co_li)
summary(data)
#Data cleaning 1: Split the column 'Date'
install.packages("tidyr")
library(tidyr)
cl1_df<-separate(data,Date, c("Year", "Month", "Day"), sep = "-")
View(cl1_df)
#How are the sales when store is open or not?
just_open<-subset(cl1_df, Open == 1)
cat("Sum sales for open stores", sum(as.numeric(just_open$Sales)))
just_closed<-subset(cl1_df, Open == 0)
cat("Sum sales for closed stores", sum(just_closed$Sales))
#Data cleaning 2: The sum of sales for closed stores is zero, so wie drop these rows (means we keep the just_open dataframe as our dataframe to proceed)
#Data cleaning 3: Replace the string values in the column 'state holiday' by numeric values to calculate faster
df<-just_open
df[df$StateHoliday == "a",]$StateHoliday = 1
df[df$StateHoliday == "b",]$StateHoliday = 2
df[df$StateHoliday == "c",]$StateHoliday = 3
df[df$StateHoliday == "0",]$StateHoliday = 0
View(df)
#VISUALIZATION
#1. What is the distribution of customers coming into the stores?
hist(df$Customers, main='Distribution of Customers coming into stores on a daily basis', xlab='Customers', col='seagreen', xlim=c(0, 3500))
#2. How many sales are there in sum with and without promo?
install.packages('dplyr')
library(dplyr)
df2<-df %>%
  group_by(Promo) %>%
  summarise(count = sum(as.numeric(Sales)))
barplot(df2$count, main='Sales based on Promotion or no promotion', names.arg = c('No Promo', 'Promo'))
counts <- table(df$Promo, sum(df$Sales))
barplot(df2, main="Sales based on Promotion or no promotion",
        xlab="Number of Fears", col=c("darkblue","green"),
        legend = rownames(df2), beside=TRUE)
install.packages('ggplot2')
library(ggplot2)
d <- aggregate(df$Sales, by=list(Promo=df$Promo), FUN = function(x){sum(as.numeric(x))})
barplot(d$x, main='Sales based on Promotion', names.arg = c('No Promo', 'Promo'), horiz = TRUE, col='red')
#3 Sales based on weekdays
we <- aggregate(df$Sales, by=list(DayOfWeek=df$DayOfWeek), FUN = function(y){sum(as.numeric(y))})
cols <- c("aquamarine3","blue4","brown2","cadetblue3","chocolate3","coral3","firebrick2")
percentlabels<- round(100*we$x/sum(we$x), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(we$x, main="Sales grouped by weekdays", col=cols, labels=pielabels, cex=0.8, clockwise=TRUE)
legend("topright", c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex=0.8, fill=cols)
#4 Show sales over the years depending on promotion
pr <-aggregate(Sales~Year+Promo, df, FUN = function(y){sum(as.numeric(y))})
ggplot(data=pr, aes(x=Year, y=Sales, colour=Promo, group=Promo))+
  geom_line() +
  geom_point()
#5 Show Sales over the year based on 