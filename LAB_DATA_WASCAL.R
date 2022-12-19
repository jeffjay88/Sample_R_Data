#WASCAL_EDA_TSERIES

#Load data
library(readxl)
WascalTS <- read_excel("C:/Users/user/Desktop/DATA ANALYSIS_CLIMATE/WascalTS.xlsx")
View(WascalTS[,-1])


#Without using the backtick sign, we can't call the variable names
WascalTS$`Temp(min)`
WascalTS$`Temp(max)`

library(dplyr)

data_frame<-WascalTS[,-1]
my_dataframe <- data_frame %>% 
  rename("MinTemperature" = "Temp(min)",
         "MaxTemperature" = "Temp(max)",
         "HiHumidity"="Humid(H)",
         "LoHumidity"="Humid(L)")

View(my_dataframe)





#Summarize data
summary(my_dataframe)


#check dimension of data (rows by columns)
dim(my_dataframe)


#Visualize Data
#create histogram of values for price


library(ggplot2)
library(ggpubr)

p1<-ggplot(data=my_dataframe, aes(x=Rainfall)) +
  geom_histogram(fill="red", color="black") +
  ggtitle(" Rainfall values")

p2<-ggplot(data=my_dataframe, aes(x=MinTemperature)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Minimum Temperature values")


p3<-ggplot(data=my_dataframe, aes(x=MaxTemperature)) +
  geom_histogram(fill="yellow", color="black") +
  ggtitle("Maximum Temperature values")


p4<-ggplot(data=my_dataframe, aes(x=HiHumidity)) +
  geom_histogram(fill="green", color="black") +
  ggtitle("High Humidity values")

p5<-ggplot(data=my_dataframe, aes(x=LoHumidity)) +
  geom_histogram(fill="pink", color="black") +
  ggtitle(" Low Humidity values")

p6<-ggplot(data=my_dataframe, aes(x=Sunshine)) +
  geom_histogram(fill="brown", color="black") +
  ggtitle("Sunshine values")

ggarrange(p1, p2,p3, p4, p5, p6 + rremove("x.text"), 
          labels = c("A", "B", "C", "D","E", "F"),
          ncol = 3, nrow = 3)

#Histograms with density
library(UsingR)

hist(my_dataframe$Rainfall, freq = FALSE)
x <- seq(min(my_dataframe$Rainfall),max(my_dataframe$Rainfall),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(Rainfall), sd(Rainfall)))
lines(x, y, col = "red")


hist(my_dataframe$MinTemperature, freq = FALSE)
x <- seq(min(my_dataframe$MinTemperature, max(my_dataframe$MinTemperature)),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(MinTemperature), sd(MinTemperauture)))
lines(x, y, col = "blue")


hist(my_dataframe$MaxTemperature, freq = FALSE)
x <- seq(min(my_dataframe$MaxTemperature, max(my_dataframe$MaxTemperature)),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(MaxTemperature), sd(MaxTemperature)))
lines(x, y, col = "green")

hist(my_dataframe$HiHumidity, freq = FALSE)
x <- seq(min(my_dataframe$HiHumidity, max(my_dataframe$HiHumidity)),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(HiHumidity), sd(HiHumidity)))
lines(x, y, col = "yellow")

hist(my_dataframe$LoHumidity, freq = FALSE)
x <- seq(min(my_dataframe$LoHumidity, max(my_dataframe$LoHumidity)),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(LoHumidity), sd(LoHumidity)))
lines(x, y, col = "green")

hist(my_dataframe$Sunshine, freq = FALSE)
x <- seq(min(my_dataframe$Sunshine, max(my_dataframe$Sunshine)),length.out=100)
y <- with(my_dataframe, dnorm(x, mean(Sunshine), sd(Sunshine)))
lines(x, y, col = "pink")



#Creating a scatterplot
q1<-ggplot(data=my_dataframe, aes(x=Rainfall, y=MinTemperature)) + 
  geom_point()

q2<-ggplot(data=my_dataframe, aes(x=Rainfall, y=MaxTemperature)) + 
  geom_point()

q3<-ggplot(data=my_dataframe, aes(x=Rainfall, y=HiHumidity)) + 
  geom_point()

q4<-ggplot(data=my_dataframe, aes(x=Rainfall, y=LoHumidity)) + 
  geom_point()

q5<-ggplot(data=my_dataframe, aes(x=Rainfall, y=Sunshine)) + 
  geom_point()
ggarrange(q1, q2,q3, q4, q5 + rremove("x.text"), 
          labels = c("A", "B", "C", "D","E"),
          ncol = 3, nrow = 2)

#Correlation between variables
library(corrplot)
cor(my_dataframe$Rainfall,my_dataframe$MinTemperature)
#cor.test(my_dataframe$Rainfall,my_dataframe$MaxTemperature)
cor(my_dataframe)
corrplot(cor(my_dataframe))


#count total missing values in each column
sapply(my_dataframe, function(x) sum(is.na(x)))


#How to Impute Missing Values
#replace missing values in each column with column means

my_dataframe$MinTemperature

df <- lapply(my_dataframe, function(df) 
  if(is.numeric(df)) replace(df, is.na(df), mean(df, na.rm = TRUE)) else df)

df
df$MinTemperature
sapply(df, function(x) sum(is.na(x)))

dff<-as.data.frame(df)
dff


#replace missing values in each column with column medians

my_dataframe$MinTemperature


df1 <- lapply(my_dataframe, function(df) 
  if(is.numeric(df)) replace(df, is.na(df), median(df, na.rm = TRUE)) else df)

df1
df$MinTemperature
sapply(df, function(x) sum(is.na(x)))
dfg<-as.data.frame(df1)
dfg

#Correlation between variables after replacing missing values
library(corrplot)
library(ggcorrplot)

cor(dff)
corrplot(cor(dff))
ggcorrplot(cor(dff))
