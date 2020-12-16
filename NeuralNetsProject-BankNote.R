#Load the dataset
df <- read.csv("https://raw.githubusercontent.com/goldenfingaz/Datasets/master/bank_note_data.csv")

#Check the head and structure of the dataframe
head(df)
str(df)

#Split the data into train and test sets
library(caTools)
sample <- sample.split(df$Class, SplitRatio=0.7)
train <- subset(df, sample==TRUE)
test <- subset(df, sample==FALSE)

#Call the neuralnet library
library(neuralnet)

#Train the model
nn <-  neuralnet(formula=Class  ~ Image.Var + Image.Skew + Image.Curt + Entropy, data=train, hidden=10, linear.output=FALSE)

#Predict using the test model
nn.predicted <-  compute(nn,test[1:4])
str(nn.predicted)

head(nn.predicted$net.result)
#Use the round function to change predicted values to 1s and 0s
nn.predicted <- round(nn.predicted$net.result)

head(nn.predicted)

#Create a confusion matrix of predicted vrs real values
table(nn.predicted,test$Class)


