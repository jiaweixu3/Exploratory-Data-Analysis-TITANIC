# First of all, set the working directory, import the library and load the titanic's data. 
rm(list=ls())
setwd("C:/Users/yojia/OneDrive/LAB1")
library("ggplot2")

# First of all, we load the data and explore its characteristics.

load("titanic_train.Rdata")
str(titanic.train)
summary(titanic.train)

# Does having cabin matters in terms of survival?

has_cabin = titanic.train$Cabin != ""
n_cabin = sum(has_cabin)
titanic.train$has_cabin[has_cabin == TRUE] = "Has cabin"
titanic.train$has_cabin[has_cabin == FALSE] = "Doesn't have cabin"
titanic.train$has_cabin = as.factor(titanic.train$has_cabin)

p_cabin = round(sum(has_cabin) * 100 / length(has_cabin),2) #Probability of having cabin
p_surv = round(mean(as.integer(titanic.train$Survived) - 1) * 100,2) #Probability of surviving
n_surv = p_surv * nrow(titanic.train) / 100

surv_cabin = as.integer(titanic.train$Survived[has_cabin]) - 1
p_surv_cabin = round(mean(surv_cabin) * 100,digits = 2)  #Probability of surviving having cabin
n_surv_cabin = p_surv_cabin * n_cabin / 100

surv_not_cabin = titanic.train$Survived[has_cabin] == 0
p_surv_not_cabin = round(mean(as.integer(surv_not_cabin))*100,2) #Probability of surviving without cabin

cabin_surv = as.integer(titanic.train$has_cabin[titanic.train$Survived == 1]) - 1
p_cabin_surv = round(mean(as.integer(cabin_surv))*100,2) #Probability of having a cabin having survived

# Graph

png(file="graphs/cabin_survived.png", res = 1/72 ,units = "cm", width = 16.9, height = 16.9)

ggplot() + aes(x=titanic.train$Survived, y = has_cabin) + geom_jitter() +
  scale_x_discrete(labels = c("No", "Yes"), name= "Survival") +
  scale_y_discrete(name="Has cabin", labels = c("No", "Yes")) +
  annotate("text",x = 1, y = 0.55, label= paste(100 - p_surv_not_cabin,"%"))+
  annotate("text",x = 1, y = 1.55, label= paste(100 - p_surv_cabin,"%"))+
  annotate("text",x = 2, y = 0.55, label= paste(p_surv_not_cabin,"%"))+
  annotate("text",x = 2, y = 1.55, label= paste( p_surv_cabin,"%"))+
  annotate("text",x = 0.5, y = 2, label= paste( p_cabin,"%"),size=3)+
  annotate("text",x = 2, y = 0.45, label= paste( p_surv,"%"),size=3)

#ggsave("graphs/cabin_survived.png",units = "cm", width = 16.9, height = 16.9)


# Ticket class – Port of embarkation and cabin number
Pclass_cabin = titanic.train$Pclass[has_cabin]
Port_cabin = titanic.train$Embarked[has_cabin]

Pclass_1_cabin = Port_cabin[Pclass_cabin == 1]
Pclass_2_cabin = Port_cabin[Pclass_cabin == 2]
Pclass_3_cabin = Port_cabin[Pclass_cabin == 3]

# Percentage of people with the same ticket class and different port

p_cabin_class1_C = round(mean(Pclass_1_cabin == "C") * 100,2)
p_cabin_class1_Q = round(mean(Pclass_1_cabin == "Q") * 100,2)
p_cabin_class1_S = round(mean(Pclass_1_cabin == "S") * 100,2)

p_cabin_class2_C = round(mean(Pclass_2_cabin == "C") * 100,2)
p_cabin_class2_Q = round(mean(Pclass_2_cabin == "Q") * 100,2)
p_cabin_class2_S = round(mean(Pclass_2_cabin == "S") * 100,2)

p_cabin_class3_C = round(mean(Pclass_3_cabin == "C") * 100,2)
p_cabin_class3_Q = round(mean(Pclass_3_cabin == "Q") * 100,2)
p_cabin_class3_S = round(mean(Pclass_3_cabin == "S") * 100,2)

p_cabin_class1 = c(p_cabin_class1_C,p_cabin_class1_Q,p_cabin_class1_S)
p_cabin_class2 = c(p_cabin_class2_C,p_cabin_class2_Q,p_cabin_class2_S)
p_cabin_class3 = p_cabin_class3_S    #Because it's 100%

#Graph

# With cabins
ggplot() + aes(x = Pclass_cabin, fill = titanic.train$Embarked[has_cabin]) + geom_bar() +
  scale_x_discrete(name="Ticket class") + 
  scale_fill_manual(labels=c("Cherbourg","Queenstown","Southampton"),name="Port of embarkation", values= c("firebrick2","cyan2","seagreen2")) +
  ylab("Passengers with cabin") + 
  annotate("text",x = rep(1,3), y = c(80,110,40), label = paste(sort(p_cabin_class1),"%"), size=3)+
  annotate("text",x = rep(2,3), y = c(9,13,5), label = paste(sort(p_cabin_class2),"%"), size=3)+
  annotate("text",x = 3, y = 5, label = paste(p_cabin_class3,"%"), size=3)+
  annotate("text",x=c(1,2,3),y=rep(-2,3), label = c(length(Pclass_1_cabin),length(Pclass_2_cabin),length(Pclass_3_cabin)), size = 2)

#ggsave("graphs/cabin_class_port1.png", units = "cm", width = 16.9, height = 16.9)

# Without cabins

Pclass_not_cabin = titanic.train$Pclass[has_cabin == FALSE]
Port_not_cabin = titanic.train$Embarked[has_cabin == FALSE]
Port_Pclass_1 = Port_not_cabin[Pclass_not_cabin == 1]
Port_Pclass_2 = Port_not_cabin[Pclass_not_cabin == 2]
Port_Pclass_3 = Port_not_cabin[Pclass_not_cabin == 3]

p_class1_C = round(mean(Port_Pclass_1 == "C") * 100,2)
p_class1_Q = round(mean(Port_Pclass_1 == "Q") * 100,2)
p_class1_S = round(mean(Port_Pclass_1 == "S") * 100,2)

p_class2_C = round(mean(Port_Pclass_2 == "C") * 100,2)
p_class2_Q = round(mean(Port_Pclass_2 == "Q") * 100,2)
p_class2_S = round(mean(Port_Pclass_2 == "S") * 100,2)

p_class3_C = round(mean(Port_Pclass_3 == "C") * 100,2)
p_class3_Q = round(mean(Port_Pclass_3 == "Q") * 100,2)
p_class3_S = round(mean(Port_Pclass_3 == "S") * 100,2)

p_class1 = c(p_class1_C,p_class1_S)       #p_class_Q == 0
p_class2 = c(p_class2_C,p_class2_Q,p_class2_S)
p_class3 = c(p_class3_C,p_class3_Q,p_class3_S)


ggplot() + aes(x = Pclass_not_cabin, fill = titanic.train$Embarked[has_cabin == FALSE]) + geom_bar() +
  scale_x_discrete(name="Ticket class") + 
  scale_fill_manual(labels=c("Cherbourg","Queenstown","Southampton"),name="Port of embarkation", values= c("firebrick2","cyan2","seagreen2")) +
  ylab("Passengers without cabins") +
  annotate("text",x = rep(1,2), y = c(8,24), label = paste(sort(p_class1),"%"), size=3)+
  annotate("text",x = rep(2,3), y = c(110,120,50), label = paste(sort(p_class2),"%"), size=3)+
  annotate("text",x = rep(3,3), y = c(340,290,150), label = paste(sort(p_class3),"%"), size=3)+
  annotate("text",x = c(1,2,3), y = rep(0,3), size = 2, label = c(length(Port_Pclass_1),length(Port_Pclass_2),length(Port_Pclass_3)))

#ggsave("graphs/cabin_class_port2.png", units = "cm", width = 16.9, height = 16.9)

# Passengers in different ports

n_S = sum(titanic.train$Embarked == "S")
n_Q = sum(titanic.train$Embarked == "Q")
n_C = sum(titanic.train$Embarked == "C")


# Fare - Cabin and ticket class

# Graph

# with the >500 outlier
ggplot(data=titanic.train) + aes(x=Pclass, y = Fare) + geom_boxplot() + 
  facet_grid(has_cabin~titanic.train$Embarked)+
  xlab("Ticket class")
#ggsave("graphs/fare_ticket_cabin1.png", units = "cm", width = 16.9, height = 16.9)

# without the >500 outlier

ggplot(data=titanic.train[-which(titanic.train$Fare >= 500),]) + aes(x=Pclass, y = Fare) +
  geom_boxplot() + facet_grid(has_cabin~titanic.train$Embarked[-which(titanic.train$Fare >= 500)]) +
  xlab("Ticket class")
ggsave("graphs/fare_ticket_cabin2.png", units = "cm", width = 16.9, height = 16.9)

outliers = titanic.train[which(titanic.train$Fare >= 500),]

# Zoom in box plots for classes 2 and 3

ggplot(data=titanic.train[which(titanic.train$Pclass != 1),]) + aes(x=Pclass, y = Fare) +
  geom_boxplot() + facet_grid(has_cabin~titanic.train$Embarked[which(titanic.train$Pclass != 1)]) +
  xlab("Ticket class")
#ggsave("graphs/fare_ticket_cabin_zoom.png", units = "cm", width = 16.9, height = 16.9)

# Sex and Age - Survival

  # First, we see the number of woman and men

titanic.train$passengers <- length(titanic.train$Sex) # Total number of passengers

ggplot(data = titanic.train, aes(x = "", y = passengers, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("male" = "royalblue", "female" = "gold")) +
  labs(title = "Women vs Men in the titanic", x="", y="") +
  theme(plot.title=element_text(hjust=0.5),
        axis.text = element_blank(),
        axis.title = element_blank())

  # Histogram to relate those variables

ggplot(data = titanic.train) + aes(x = Age, fill = Sex, y = stat(count)) + 
  geom_histogram(binwidth = 7.5, color = "white", position = "stack") + labs(x = "Age", y = "Survival", fill = "Sex") +
  ggtitle("Survival by Sex and Age") +  theme(plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values = c("male" = "royalblue", "female" = "gold"))

# Fare - Being with family // Being with family - Survival

titanic.train$FamilySize <- titanic.train$SibSp + titanic.train$Parch # I define a new variable that tells the size of the family

  # Bar chart to relate those variables

ggplot(data = titanic.train, aes(y = Fare, x = FamilySize, fill = Survived)) + scale_fill_manual(values = c("lightpink", "tomato1"), 
                                                                                            labels = c("No", "Yes")) +
 geom_bar(stat = "identity", position = "stack", width = 0.65) + labs(y = "Fare", x = "Family's size", fill = "Survival's proportion") + 
 ggtitle("Fare and Being with family") + theme(plot.title = element_text(hjust=0.5)) 

# Fare and Age // Survival

  # Scatter plot to relate those variables

ggplot(data = titanic.train, aes(y = Fare, x = Age, color = Survived)) + scale_color_manual(values = c("mediumpurple", "darkolivegreen"), 
                                                                                                 labels = c("No", "Yes")) +
  geom_point(stat = "identity", size = 1.5, alpha = 0.8) + labs(y = "Fare", x = "Age", color = "Survived") + 
  ggtitle("Fare and Age") + theme(plot.title = element_text(hjust=0.5))