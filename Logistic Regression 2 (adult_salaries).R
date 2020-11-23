#Read in the dataframe and set it to "adult"
adult <- read.csv("https://raw.githubusercontent.com/goldenfingaz/Datasets/master/adult_sal.csv")

#Check the first few rows of the dataset
head(adult)

#The index has been repeated. Let's drop this column using the dplyr library.
library(dplyr)
adult <- select(adult, -X)
head(adult)

#Now let's do a quick check on the summary and structure of the data set.
str(adult)
summary(adult)

#Data cleaning
#Let's check the frequency of the type_employer column using the table function.
table(adult$type_employer)

#Using a custom functon, let's combine "Never-worked" and "Without-pay" into a new category called "Unemployed", 
#"State-gov" and "Local-gov" into a new category called "SL-gov, and self-employed jobs into a category called "self-emp"

comb.unemployed <- function(job){
    job <- as.character(job)
    if (job == "Never-worked" | job == "Without-pay"){
      return ("Unemployed")
    }else if (job == "Local-gov" | job == "State-gov"){
      return ("SL-gov")
    }else if (job == "Self-emp-inc" | job == "Self-emp-not-inc"){
      return ("self-emp")
    }else{
      return(job)
    }
}
  

#Apply function

adult$type_employer <- sapply(adult$type_employer, comb.unemployed)

table(adult$type_employer)


#Now let's move on to the marital column
table(adult$marital)

#We will reduce the categories in this column to "Married", "Never-Married", "Not-Married" using a custom function.

comb.marital <- function(status){
  status <- as.character(status)
  if(status == "Married-AF-spouse" | status == "Married-civ-spouse" | status == "Married-spouse-absent"){
      return("Married")
    }else if(status == "Divorced" | status == "Widowed" | status == "Separated"){
      return("Not-Married")
    }else{
      return(status)
    }
}
  

#Apply custom function to marital column
adult$marital <- sapply(adult$marital, comb.marital)

table(adult$marital)

#Lets check the country column
table(adult$country)

#We will combine the different countries into their respective continents. 
adult$country <- as.character(adult$country)

#Create a custom function to combine the countries

comb.countries <- function(country){
    if (country %in% c("China", "Cambodia","Hong","India","Japan","Laos","Japan","Philippines","Taiwan","Thailand","Vietnam","Iran")){
      return ("Asia")
    }else if (country %in% c("Canada","United-States","Puerto-Rico")){
      return ("North America")
    }else if (country %in% c("England","France","Germany","Greece","Italy","Poland","Portugal","Hungary","Scotland","Yugoslavia","Ireland","Holand-Netherlands")){
      return ("Europe")
    }else if(country %in% c("Columbia","Cuba","Mexico","Outlying-US(Guam-USVI-etc)","Dominican-Republic","Ecuador","El-Salvador","Guatemala","Honduras","Haiti","Jamaica","Nicaragua","Peru","Trinadad&Tobago")){
      return ("Latin and South America")
    }else{
      return("Other")
    }
}
  

#Apply function
adult$country <- sapply(adult$country, comb.countries)

table(adult$country)

#Rename the country column to region
names(adult)[names(adult) == "country"] <- "region"

#Check the structure of the dataframe.
str(adult)

#Change the columns with combined categories into factors
adult$type_employer <- factor(adult$type_employer)
adult$marital <- factor(adult$marital)
adult$country <- factor(adult$country)

#Now let's deal with the missing data. First we will convert any cell with "?" to NA value.
is.na(adult) <- adult == "?"
sum(is.na(adult))


#Use the Amelia package to visualize missing values in the dataset
library(Amelia)
missmap(adult, y.at=c(1), y.labels = c(""), col=c("yellow","black"),main = "Missing Data")

#Drop all rows with NA values
adult <- na.omit(adult)
any(is.na(adult))

#Now let's do some exploratory data analysis
library(ggplot2)

#Age distribution by income
ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color = "black", bins = 100) + theme_classic()

#Distribution of number of hours worked
ggplot(adult, aes(hr_per_week)) + geom_histogram(fill="orange", color = "black", alpha = 0.8) + theme_classic()

#Proportion of income class by region
ggplot(adult, aes(region)) + geom_bar(aes(fill = income), color="black") + theme(axis.text.x = element_text(angle = 90)) + theme_classic()

#Now let's begin to build our logistic regression model
head(adult)

#Split the data into "train" set and "test" set
library(caTools)

sample <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, sample == TRUE)
test <- subset(adult, sample == FALSE)

#Build the model
model <- glm(formula = income ~ ., family = binomial(link = "logit"), data = train)
summary(model)

#Use the step function to create a new model
new.model <- step(model)

summary(new.model)

#Test the model using the predict function
test$predictions <- predict(new.model, newdata = test, type = "response")

table(test$income, test$predictions > 0.5)

#Finally, let's check the accuracy of the model
accuracy <- (6413+1392)/(6413+1392+903+507)
accuracy




