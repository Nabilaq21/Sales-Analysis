Walmart_data <- read.csv("Walmart_Store_sales.csv",header = TRUE, sep = ",")
View(Walmart_data)
library(dplyr)

#Which store has maximum sales:

Max_sale <- head(arrange(Walmart_data,desc(Weekly_Sales)),1)
Max_sale

#OR
max(Walmart_data$Weekly_Sales)
Walmart_data[which.max(Walmart_data$Weekly_Sales),]$Store

#Which store has maximum standard deviation i.e., the sales vary a lot. 

Data1 <- summarise(group_by(Walmart_data,Store),SD = sd(Weekly_Sales),mean = mean(Weekly_Sales))
View(Data1)
Max_sd <- head(arrange(Data1,desc(SD)),1)
Max_sd

#ANOTHER METHOD
arrange(Data1,SD)
max(Data1$SD)
Data1[which.max(Data1$SD) ,]$Store

#Also, find out the coefficient of mean to standard deviation
library(raster)

Data2 <- summarise(group_by(Walmart_data,Store),COV = cv(Weekly_Sales, na.rm = FALSE))
View(Data2)
cov <- head(arrange(Data2,desc(COV)),1) # gives store having max. coefficiet of variation
cov

#Which store/s has good quarterly growth rate in Q3’2012
library(lubridate)

Walmart_data$Date <- dmy(Walmart_data$Date) #changing format of date var to Date
View(Walmart_data)

#storing second and third Quartile start and end dates as date format

Q2_Start<-dmy("1-04-2012")
Q2_End<-dmy("30-06-2012")
Q3_Start<-dmy("1-07-2012")
Q3_End<-dmy("30-09-2012")

#storing 2nd Quartile and 3rd Quartile Data

Q2_Data <-Walmart_data[Walmart_data$Date >Q2_Start & Walmart_data$Date < Q2_End,] #extracts those data in between start and end dates of Q2
Q3_Data <- Walmart_data[Walmart_data$Date >Q3_Start & Walmart_data$Date < Q3_End,]  #extracts those data in between start and end dates of Q3

#finding the sum of weekly sales of each store in Q2 and Q3 respectively

Q2 <- summarise(group_by(Q2_Data ,Store),Q2_Weekly_sales = sum(Weekly_Sales, na.rm = FALSE))
Q3 <- summarise(group_by(Q3_Data ,Store),Q3_Weekly_sales = sum(Weekly_Sales, na.rm = FALSE))

#mergeing Q2 and Q3 data on Store as a common column

Q3_growth <- merge(Q2,Q3,by='Store')
View(Q3_growth)

#Calculating Growth rate of each Store and collecting it. 

#growth rate = (present value - past value)
Q3_growth$Growth_rate = ((Q3_growth$Q3_Weekly_sales - Q3_growth$Q2_Weekly_sales)/Q3_growth$Q2_Weekly_sales)

Q3_growth$Growth_rate  = round(Q3_growth$Growth_rate ,2) #round off growth rates to 2 values
View(Q3_growth)

Max_growth <- head(arrange(Q3_growth,desc(Growth_rate)),1)# store having max. (good) growth rate in 3rd Quarter
Max_growth

Min_growth <- tail(arrange(Q3_growth,desc(Growth_rate)),1)# store having min.(loss in) growth rate in 3rd Quarter
Min_growth

#Some holidays have a negative impact on sales.
#holidays which have higher sales than the mean sales in non-holiday season for all stores together.

Mean_Holiday_NonHoliday_sales <- summarise(group_by(Walmart_data,Holiday_Flag),mean(Weekly_Sales)) #mean sales of holiday and no-holiday season
Mean_Holiday_NonHoliday_sales

# storing Holiday Dates
Christmas1 = ymd("2010,12,31") 
Christmas2 = ymd("2011,12,30") 
Christmas3 = ymd("2012,12,28")
Christmas4 = ymd("2013,12,27")

Thanksgiving1=ymd("2010,11,26")
Thanksgiving2=ymd("2011,11,25")
Thanksgiving3=ymd("2012,11,23")
Thanksgiving4=ymd("2013,11,29")

LabourDay1=ymd("2010,09,10")
LabourDay2=ymd("2011,09,9")
LabourDay3=ymd("2012,09,7")
LabourDay4=ymd("2013,09,6")

SuperBowl1=ymd("2010,02,12")
SuperBowl2=ymd("2011,02,11")
SuperBowl3=ymd("2012,02,10")
SuperBowl4=ymd("2013,02,8")

#calculating mean sales for each holidays respectively

#seperately extracting data from walmart data set having holidays
Christmas_mean_sales=Walmart_data[(Walmart_data$Date == Christmas1) | (Walmart_data$Date == Christmas2) | (Walmart_data$Date == Christmas3) | (Walmart_data$Date == Christmas4),]
Thanksgiving_mean_sales=Walmart_data[(Walmart_data$Date == Thanksgiving1) | (Walmart_data$Date == Thanksgiving2) | (Walmart_data$Date == Thanksgiving3) | (Walmart_data$Date == Thanksgiving4),]
LabourDay_mean_sales=Walmart_data[(Walmart_data$Date == LabourDay1) | (Walmart_data$Date == LabourDay2) | (Walmart_data$Date == LabourDay3) | (Walmart_data$Date == LabourDay4),]
SuperBowl_mean_sales=Walmart_data[(Walmart_data$Date == SuperBowl1) | (Walmart_data$Date == SuperBowl2) | (Walmart_data$Date == SuperBowl3) | (Walmart_data$Date == SuperBowl4),]

#mean sales of each holiday seasons

C_mean_sale <- mean(Christmas_mean_sales$Weekly_Sales)
C_mean_sale <- round(C_mean_sale,2)
Christmas_mean_sales1 = print(paste("Christmas_mean_sales:" , C_mean_sale)) # mean sales in christmas day

T_mean_sale <- mean(Thanksgiving_mean_sales$Weekly_Sales)
T_mean_sale <- round(T_mean_sale,2)
Thanksgiving_mean_sales1 = print(paste("Thanksgiving_mean_sales:" , T_mean_sale))# mean sales in thanksgiving day

L_mean_sale <- mean(LabourDay_mean_sales$Weekly_Sales)
L_mean_sale <- round(L_mean_sale,2)
LabourDay_mean_sales1 = print(paste("LabourDay_mean_sales:" , L_mean_sale))# mean sales in labour day

S_mean_sale <- mean(SuperBowl_mean_sales$Weekly_Sales)
S_mean_sale <- round(S_mean_sale,2)
SuperBowl_mean_sales1 = print(paste("SuperBowl_mean_sales:" , S_mean_sale))# mean sales in super bowl day

# mean sales of non holiday season
Non_holiday_week_sales <- filter(Walmart_data,Holiday_Flag==0)
Non_holiday_week_sales1 <- mean(Non_holiday_week_sales$Weekly_Sales)
Non_holiday_week_sales1 <- round(Non_holiday_week_sales1,2)
Non_holiday_week_sales2 = print(paste("Non_holiday_week_sales:",Non_holiday_week_sales1))

#combining each mean sales for better view
Mean_sales<- data.frame(Christmas_slaes = C_mean_sale ,Thanksgiving_sales = T_mean_sale,LabourDay_sales= L_mean_sale, SuperBowl_sales= S_mean_sale,Non_holiday_sales = Non_holiday_week_sales1)
Mean_sales

#Monthly,Quartely and Semester view of sales
Walmart_data$Months = months(Walmart_data$Date) # creating months var. from date var.
Monthly <- summarise(group_by(Walmart_data,Months), Monthly_Sales = sum(Weekly_Sales)) # monthly sales
View(Monthly)

#monthly sales plot
library(ggplot2)
monthly_sales_plot <- ggplot(Monthly,aes(x=Months,y=Monthly_Sales))
monthly_sales_plot + geom_point(color="red",shape=3,size = 3)

Walmart_data$Quarters <- paste0(year(Walmart_data$Date), "/0" , quarter(Walmart_data$Date)) #creating Quarters from date var
Quarterly <- summarise(group_by(Walmart_data,Quarters), Quarterly_Sales = sum(Weekly_Sales))#Quarterly sales
View(Quarterly)

#Quartely sales plot
Quarterly_sales_plot <- ggplot(Quarterly,aes(x=Quarters,y= Quarterly_Sales))
Quarterly_sales_plot + geom_point(color="blue",shape=3,size = 3)

Walmart_data$Semesters <-  paste0(year(Walmart_data$Date), "/0" ,semester(Walmart_data$Date)) #creating semesters from date var.
Semesterly <- summarise(group_by(Walmart_data,Semesters), Semesterly_Sales = sum(Weekly_Sales)) #semesterly data
View(Semesterly)

#Semesterly sales plot
Semesterly_sales_plot <- ggplot(Semesterly,aes(x=Semesters,y= Semesterly_Sales))
Semesterly_sales_plot + geom_point(color="brown",shape=3,size = 3)


