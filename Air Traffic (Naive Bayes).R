#Air Traffic Passengers Data

#Import libraries
library(caret)
library(caTools)
library(ggplot2)
library(lattice)
library(e1071)
library(dplyr)
library(ROSE)

#Read data from file
df <- read.csv("Air_Traffic_Passenger_Data.csv")

#Data pre-processing
#To check the data structure
str(df)

#Convert to the suitable data types
df$Activity.Period <- as.factor(df$Activity.Period)
df$Operating.Airline <- as.factor(df$Operating.Airline)
df$Operating.Airline.IATA.Code <- as.factor(df$Operating.Airline.IATA.Code)
df$Published.Airline <- as.factor(df$Published.Airline)
df$Published.Airline.IATA.Code <- as.factor(df$Published.Airline.IATA.Code)
df$GEO.Summary <- as.factor(df$GEO.Summary)
df$GEO.Region <- as.factor(df$GEO.Region)
df$Activity.Type.Code <- as.factor(df$Activity.Type.Code)
df$Price.Category.Code <- as.factor(df$Price.Category.Code)
df$Terminal <- as.factor(df$Terminal)
df$Boarding.Area <- as.factor(df$Boarding.Area)
df$Adjusted.Activity.Type.Code <- as.factor(df$Adjusted.Activity.Type.Code)
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)

#Summary of the finalized data frame
summary(df)


#Data Visualization
#Bar chart of the number of flights by GEO.Summary and GEO.Region
countGEOSummary <- df %>% group_by(GEO.Summary,GEO.Region) %>% summarise(count = n())
ggplot(countGEOSummary, aes(x = GEO.Summary, y = count, fill = GEO.Region, label = format(count))) +
  geom_bar(stat = "identity", width = 0.6) + 
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "black") + 
  labs(x = "GEO.Summary", y = "Number of Flight", title = "Number of Flight by GEO.Summary and GEO.Region") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 15), 
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 8))

#Bar chart of the number of passengers by terminal and boarding area
countPassenger <- df %>% group_by(Terminal,Boarding.Area) %>% summarise(passenger_count = sum(Adjusted.Passenger.Count))
ggplot(countPassenger, aes(x = Terminal, y = passenger_count, fill = Boarding.Area,label = format(passenger_count))) +
  geom_bar(stat = "identity", width = 0.6) + coord_flip() +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "black")
labs(x = "Terminal", y = "Number of Passengers", title = "Number of Passengers by Terminal and Boarding Area") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 15), 
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 8))

#Bar chart of the number of passengers by year of 2005-2016
countPassenger2 <- df %>% group_by(Year) %>% summarise(passenger_count = sum(Adjusted.Passenger.Count))
ggplot(countPassenger2, aes(x = Year, y = passenger_count,label = format(passenger_count))) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(x = "Year", y = "Number of Passengers", title = "Number of Passengers by Year") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 15), 
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 8))


#Data preparation for naive bayes model
#Define a column for the factorization of Adjusted Passenger Count
passenger_group <- vector(mode="character", length=length(df$Adjusted.Passenger.Count))
passenger_group[df$Adjusted.Passenger.Count < 29332] <- "Low"
passenger_group[df$Adjusted.Passenger.Count >= 29332] <- "High"
Passenger <- factor(passenger_group,levels=c("Low", "High"),ordered = TRUE)
df <- cbind(df, Passenger)

#To check the significance of each column using chi-squared test with p-value of 0.05
x <- df[,1:16]
y <- df[,16+1]
p_value <- sapply(x, function(f) chisq.test(f,y)$p.value)
threshold <- 0.05

#Select features with p-value < threshold
important_features <- x[,p_value < threshold]

#Select the significant features
demo1 <- subset(df, select = c(2,4,6,7,9,10,11,13,17))

#Split the data into 80% train set and 20% test set
split = sample.split(demo1, SplitRatio = 0.8)
train_data <- subset(demo1, split == TRUE)
test_data <- subset(demo1, split == FALSE)

#Fitting naive bayes model to train data set
nbModel <- naiveBayes(Passenger ~.,data = train_data)
nbModel

#Predicting on test data
prediction <- predict(nbModel, newdata = test_data)

#Model Evaluation
cm <- confusionMatrix(test_data$Passenger,prediction)
print(cm)



