#reading test and train files
test <- read.csv("test.csv", header= TRUE)
train <- read.csv("train.csv", header= TRUE)

#adding survived column to test dataset
test.survived <- data.frame(Survived= rep("None",nrow(test)),test[,])
data.frame <- unname(data.frame)

#combine the train and test.survived
data.combined <- rbind(train, test.survived)
str(data.combined)

#converting required variables to factors
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

str(data.combined)

table(data.combined$Survived)

table(data.combined$Pclass)

#calling library ggplot2 for plotting graphs
library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
str(train)


ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width=0.5) + xlab("pclass") + ylab("Total Count") + labs(fill="Survived")

#examining the first few names in the train set
head(as.character(train$Name))

#finding out how many unique names are on train and test datasets
length(unique(as.character(data.combined$Name)))

#found two duplicate names
#get those duplicate names and put them in a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#take a look at records in combined dataset
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

#correlation between variables
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

#correlation with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#extract males from dataset
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

#create utility function and extract titles
extractTitle <- function(Name)
{ Name <- as.character(Name)

if(length(grep("Miss.", Name)) > 0) 
{ return("Miss.")
}
else if(length(grep("Mrs.", Name)) > 0)
{ return("Mrs.")
}
else if(length(grep("Master.", Name)) > 0)
{ return("Master.")
}
else if(length(grep("Mr.", Name)) > 0)
{ return("Mr.")
}
else { return("Other")
}
}

titles <- NULL
for(i in 1:nrow(data.combined))
{
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#plot the new graphs
ggplot(data.combined[1:891,], aes(x=title, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")

#distribution of females and males across test ands train set
table(data.combined$Sex)

#visualize the graphs with sex variable
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill="Survived")

#checking with age
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#visualizing with age variable
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived)) +
  facet_wrap(~Sex + Pclass)+
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

#validating boys with master title is a good proxy
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#now with miss title
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

#plotting the graph with misses survived
ggplot(misses[misses$Survived != "None",], aes(x=Age, fill=Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass)+
  ggtitle("Age for Miss. Pclass") +
  xlab("Age") +
  ylab("Total Count")

#summarize misses alone
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

#plot the sibsp with p class and title
ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title") +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")

#the same with parch
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")

#creating a family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

#plotting the graphs of family size
ggplot(data.combined[1:891,], aes(x=family.size, fill=Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")

#taking a look at ticket variable
str(data.combined$Ticket)

#display first 20 in character
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#extracting the unique first letters of each character for better understanding
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

data.combined$ticket.first.char <- as.factor(ticket.first.char)

#plotting the survivability by first char of ticket
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived)) +
  geom_bar() +
  ggtitle("Survivability by first char of ticket") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill="Survived")

#plotting based on pclass
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill="Survived")

#plotting with a combination of pclass and title
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+title) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill="Survived")

#based on fare paid by passengers
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#plots based on fare
ggplot(data.combined, aes(x = Fare)) +
  geom_bar(width = 5) +
  ggtitle("Distribution based on fare") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200) 

#checking if fare has a predictive power
ggplot(data.combined[1:891,], aes(x=Fare, fill=Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass+title) +
  ggtitle("Pclass+ title fare distribution") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill="Survived")

#analyzing the cabins
str(data.combined$Cabin)

#viewing as character string
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#replacing emply cells with U
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#extracting the first char of cabin
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

#plotting based on cabins
ggplot(data.combined[1:891,], aes(x=cabin.first.char[1:891], fill=Survived)) +
  geom_bar() +
  ggtitle("based on cabin") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill="Survived")

#categorizing on basis of pclass
ggplot(data.combined[1:891,], aes(x=cabin.first.char[1:891], fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("based on cabin") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill="Survived")

#now with title too
ggplot(data.combined[1:891,], aes(x=cabin.first.char[1:891], fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+title) +
  ggtitle("based on cabin") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill="Survived")

#check with multiple cabins
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x=cabin.multiple, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+ title) +
  ggtitle("based on multiple cabin") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill="Survived")

#does survivability depend on the place embarked
str(data.combined$Embarked)
levels(data.combined$Embarked)

#plot the embarked graph
ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+ title) +
  ggtitle("based on where embarked") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")
