
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

library("readxl")
library(ggplot2)
library(dplyr)
library(extrafont)
library(moments)
library(epiDisplay)
library(GGally)

# Loading
setwd("C:/HCMUS/Final_Project")

# xls files
Data <- read_excel("wage2.xls")
attach(Data)

save(Data,file="data.rda")
load("data.rda")

# first 6 observations
head(Data)

# structure of dataset
str(Data)

names(Data) <- c("wage","hours","IQ", "exper", "married", "black", "south")

Data$married <- factor(Data$married)
levels(Data$married) <- c("valua 0 (Unmarried)", "Valua 1(Married)")

Data$black <- factor(Data$black)
levels(Data$black) <- c("valua 0 (Other)", "Valua 1(Black)")

Data$south <- factor(Data$south)
levels(Data$south) <- c("valua 0 (Other)", "Valua 1(South)")

summary(Data)

barplot(table(Data$wage))

ggplot(Data, aes(x = wage)) + 
  scale_x_continuous(breaks = seq(0, 3000, 300)) +
  geom_histogram(binwidth = 300, color="black", fill="lightblue") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

barplot(table(hours))

Cut <- cut(hours, breaks=4)

ggplot(Data, aes(x = Cut)) + 
  geom_histogram(stat= "count", aes(fill = ..count..), color="black", fill="lightblue") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

ggplot(Data, aes(x=hours, y=wage)) + 
  geom_point(color="deepskyblue4") 

barplot(table(IQ))

Cut <- cut(IQ, breaks=19)

ggplot(Data, aes(x = Cut)) + 
  geom_histogram(stat= "count", aes(fill = ..count..), color="black", fill="lightblue") +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

ggplot(Data, aes(x=IQ, y=wage)) + geom_point(color="deepskyblue4")

barplot(table(exper))

ggplot(Data, aes(x=exper, y=wage)) + geom_point(color="deepskyblue4")

pie(table(married))

ggplot(Data, aes(x = wage, y = married, fill = married)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle('Box Plot showing the salaries of two groups of married and unmarried') +
  xlab('Wage') +
  ylab('Case') +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Unmarried', 'Married')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data,aes(x=wage, fill = married)) +
  geom_histogram(aes(y = ..density..), color = "grey17") +
  geom_density(alpha = .2, fill = "yellow") +
  facet_wrap(~married,ncol = 1,scale = "fixed")+
  xlab("Wage") +
  ylab("Density") +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Unmarried', 'Married')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

pie(table(black))

ggplot(Data, aes(x = wage, y = black, fill = black)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle('Box Plot showing the salaries of two groups of blacks and non-blacks') +
  xlab('Wage') +
  ylab('Case') +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Other', 'Black')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data,aes(x=wage, fill = black)) +
  geom_histogram(aes(y = ..density..), color = "grey17") +
  geom_density(alpha = .2, fill = "yellow") +
  facet_wrap(~black,ncol = 1,scale = "fixed")+
  xlab("Wage") +
  ylab("Density") +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Other', 'Black')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

pie(table(south))

ggplot(Data, aes(x = wage, y = south, fill = south)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle('Box Plot showing the salaries of two groups of people living in the south and not living in the south of the US') +
  xlab('Wage') +
  ylab('Case') +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Other', 'South')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data,aes(x=wage, fill = south)) +
  geom_histogram(aes(y = ..density..), color = "grey17") +
  geom_density(alpha = .2, fill = "yellow") +
  facet_wrap(~south,ncol = 1,scale = "fixed")+
  xlab("Wage") +
  ylab("Density") +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Other', 'South')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Data, aes(x = wage, y = married, fill = married)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle('Box Plot showing the salaries of two groups of married and unmarried') +
  xlab('Wage') +
  ylab('Case') +
  scale_fill_brewer(palette = 'Blues', name = 'Case',
                    labels = c('Unmarried', 'Married')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

t.test(wage~married, alternative="less")

ggplot(Data, aes(x = IQ, y = black, fill = black)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle('Box Plot the IQ score of blacks and non-blacks') +
    xlab('IQ score') +
    ylab('Case') +
    scale_fill_brewer(palette = 'Blues', name = 'Case',
                      labels = c('Other', 'Black')) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

t.test(IQ~black, alternative="greater")

blackvssouth <- Data %>%
group_by(south, black) %>%
summarise(count=n()) %>%
mutate(prec=count/sum(count))

ggplot(blackvssouth, aes(x="", y= prec, fill=black)) +
geom_bar(width = 2, stat = "identity") +
coord_polar("y", start=0) + facet_wrap(~south,ncol = 2,scale =
"fixed")+
ggtitle("Proportion of blacks in the south and not in the south of the US")+
xlab("")+
ylab("")+
scale_fill_discrete(labels = c("Other","Black")) 

prop.test(table(south, black), correc=FALSE, alternative="less")

blackvsmarried <- Data %>%
group_by(black, married) %>%
summarise(count=n()) %>%
mutate(prec=count/sum(count))

ggplot(blackvsmarried, aes(x="", y= prec, fill=married)) +
geom_bar(width = 2, stat = "identity") +
coord_polar("y", start=0) + facet_wrap(~black,ncol = 2,scale =
"fixed")+
ggtitle("Proportion of unmarried people among blacks and the others")+
xlab("")+
ylab("")+
scale_fill_discrete(labels = c("Unmarried","Married")) 

prop.test(table(black, married), correc=FALSE)

model1 <- lm(wage~hours)
model1

ggplot(Data,aes(hours,wage,color = hours))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Monthly working hours") +
  ylab("Salary")

summary(model1)

confint(model1)

model2 <- lm(wage~IQ)
model2

ggplot(Data,aes(IQ,wage,color = IQ))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", se = FALSE) +
  xlab("IQ score") +
  ylab("Salary")


summary(model2)

confint(model2)

model3 <- lm(wage~exper)
model3

ggplot(Data,aes(exper,wage,color = exper))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Years of work experience") +
  ylab("Salary")

summary(model3)

confint(model3)

pairs(wage~hours+IQ+exper)

model <- lm(wage~hours+IQ+exper)
model

confint(model, level=0.95)

summary(model)

anova(model1,model)
