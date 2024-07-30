#Read data from file
df1 <- read.csv("Housing_Prices.csv")

#Data preparation
#To check the data structure
str(df1)

#Convert to the suitable data types
df1$LOCATION <- as.factor(df$LOCATION)
df1$INDICATOR <- as.factor(df$INDICATOR)
df1$SUBJECT <- as.factor(df$SUBJECT)
df1$MEASURE <- as.factor(df$MEASURE)
df1$FREQUENCY <- as.factor(df$FREQUENCY)
df1$TIME <- as.factor(df$TIME)

#Summary of the converted data frame
summary(df1)

#Drop the unnecessary column
new_df1 <- subset(df1, select = c(1,3,6,7))

#Summary of the finalized data frame
summary(new_df1)

# Visualize the data set
# Create a figure which contains multiple plots
par(mfrow=c(2,2))
par(mar=c(4.5, 4.5, 0.5,0.5))
plot(Value~LOCATION+SUBJECT+TIME, data = new_df1)

# Create a housing price linear model based on the three input variables
linear_model <- lm(Value~LOCATION+SUBJECT+TIME, data = new_df1)
summary(linear_model)

#Create a 95% confidence interval of linear regression model
confint(linear_model)
