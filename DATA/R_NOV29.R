# installing packages
install.packages("fastDummies")
install.packages("tidyverse")
install.packages("lmtest")
install.packages("stargazer")
install.packages("tmap")
install.packages("maps")
install.packages("mapproj")
install.packages("ggplot2")
install.packages("RColorBrewer")
library('RColorBrewer')
library('magrittr')
library('ggplot2')
library('maps')
library('mapproj')
library('tmap') 
library("stargazer")
library("lmtest")
library("fastDummies")
library("tidyverse")

#### FOR 2015 ####

# reading in the 2015 Results
results2015 = read.csv("/Users/anahat/Desktop/490DATA/Election Results/2015Results.csv")

# reading in the 2019 Results
results2019 = read.csv("/Users/anahat/Desktop/490DATA/Election Results/2019Results.csv")

# creating dummy variables

# for 2015
results2015 = dummy_cols(results2015, select_columns = 
                           c('Gender', 'ranfor43', 'reelected43',
                             'Backbencher', 'Party'))

# for 2019
results2019 = dummy_cols(results2019, select_columns = 
                           c('Gender', 'ranfor44', 'reelected44',
                             'Backbencher', 'Party'))

# renaming columns

# for 2015
names(results2015)[names(results2015) == "Party_Green Party of Canada"] <- "GPC"
names(results2015)[names(results2015) == "Party_Bloc Québécois"] <- "BQ"
names(results2015)[names(results2015) == "AgeOnElectionDay"] <- "Age"
names(results2015)[names(results2015) == "YearsServedAtTimeOfElection"] <- "YearsInOffice"
names(results2015)[names(results2015) == "Gender_F"] <- "Gender"
names(results2015)[names(results2015) == "ranfor43_Yes"] <- "RanForNE"
names(results2015)[names(results2015) == "reelected43_Yes"] <- "ReelectedInNE"
names(results2015)[names(results2015) == "Backbencher_Yes"] <- "Backbencher"
names(results2015)[names(results2015) == "Party_Conservative Party of Canada"] <- "ConservativeMem"
names(results2015)[names(results2015) == "Party_Liberal Party of Canada"] <- "LiberalMem"
names(results2015)[names(results2015) == "Party_New Democratic Party"] <- "NDPMem"
names(results2015)[names(results2015) == "Parliament"] <- "Election"
names(results2015)[names(results2015) == "RidingID"] <- "Riding.ID"

# for 2019
names(results2019)[names(results2019) == "Party_Green Party of Canada"] <- "GPC"
names(results2019)[names(results2019) == "Party_Bloc Québécois"] <- "BQ"
names(results2019)[names(results2019) == "AgeOnElectionDay"] <- "Age"
names(results2019)[names(results2019) == "YearsServedAtElection"] <- "YearsInOffice"
names(results2019)[names(results2019) == "Gender_F"] <- "Gender"
names(results2019)[names(results2019) == "ranfor44_Yes"] <- "RanForNE"
names(results2019)[names(results2019) == "reelected44_Yes"] <- "ReelectedInNE"
names(results2019)[names(results2019) == "Backbencher_Yes"] <- "Backbencher"
names(results2019)[names(results2019) == "Party_Conservative Party of Canada"] <- "ConservativeMem"
names(results2019)[names(results2019) == "Party_Liberal Party of Canada"] <- "LiberalMem"
names(results2019)[names(results2019) == "Party_New Democratic Party"] <- "NDPMem"
names(results2019)[names(results2019) == "Name"] <- "Candidate"
names(results2019)[names(results2019) == "StatementsByMembers"] <- "Statements"

# removing unnecessary columns

# for 2015
results2015 = subset(results2015, select = -c(Gender, ranfor43, reelected43, 
                                              Backbencher, Gender_M, 
                                              ranfor43_No, reelected43_., reelected43_No, 
                                              Backbencher_No, Party, DOB1, BQ, GPC))

# for 2019
results2019 = subset(results2019, select = -c(Gender, ranfor44, reelected44, Backbencher,
                                              DOB, Gender_M, ranfor44_No,
                                              reelected44_., reelected44_No, Backbencher_No, BQ, GPC, 
                                              Party, Party_Independent))

# converting variable to log

# for 2015
results2015$QuestionsLog = log(results2015$QuestionPeriod)

# for 2019
results2019$QuestionsLog = log(results2019$QuestionPeriod)

# combining the two data frames
RESULTS = rbind(results2015, results2019)

# creating data frame using subset 

# creating data frame RESULTS - candidates who ran for reelection
Results = subset(RESULTS, RanForNE != 0)

# creating data frame Results_1 - candidates who asked atleast 1 question
Results = subset(Results, QuestionPeriod != 0)

# creating data frame Results_1 - candidates who asked atleast 1 question
Results = subset(Results, Candidate != 'Justin Trudeau')

# checking distribution
# converting to QuestionsLog gives a normal distribution
hist(Results$QuestionPeriod)
hist(Results$QuestionsLog)
hist(Results$PvtMemBills)
hist(Results$Statements)

# regression1 - just questionslog and pvtmembills

reg1 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills + Results$Statements)
summary(reg1)

# regression2 - reg1 + personal characterstics (gender, age)

reg2 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills + 
            Results$Statements+ Results$Gender + Results$Age)
summary(reg2)

# regression3 - reg2 + yearsinoffice at time of election, party info
reg3 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + 
            Results$PvtMemBills + + Results$Statements + 
            Results$Gender + Results$Age + Results$YearsInOffice +
            Results$ConservativeMem + Results$LiberalMem + 
            Results$NDPMem + Results$Backbencher)
summary(reg3)
coeftest(reg3)

# regression1 - just questionslog and pvtmembills

reg4 = glm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills + Results$Statements, family = "binomial")
summary(reg4)

# regression2 - reg1 + personal characterstics (gender, age)

reg5 = glm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills + 
            Results$Statements+ Results$Gender + Results$Age, family = "binomial")
summary(reg5)

# regression3 - reg2 + yearsinoffice at time of election, party info
reg6 = glm(Results$ReelectedInNE ~ Results$QuestionsLog + 
            Results$PvtMemBills + + Results$Statements + 
            Results$Gender + Results$Age + Results$YearsInOffice +
            Results$ConservativeMem + Results$LiberalMem + 
            Results$NDPMem + Results$Backbencher, family = "binomial")
summary(reg6)

# reg4
# graphs
#Questions = ggplot(Results, aes(x=QuestionPeriod, y=ReelectedInNE)) + geom_boxplot()
#Questions

reelectedlabs <- c("Yes", "No")
  
ggplot(Results, aes(x=as.factor(ReelectedInNE), y=QuestionsLog, fill=QuestionsLog)) + 
  geom_boxplot(fill = "#ffdada") + labs(y="Questions Asked (log)", x="Reelected") + 
  scale_x_discrete(labels= reelectedlabs) + ggtitle('Distribution of Questions Asked by Reelection Results') +
  theme(plot.title = element_text(hjust = 0.5))


# stargazer things

stargazer(Results, type = 'html', out = '/Users/anahat/Desktop/490DATA/R/summary.htm')
stargazer(reg4, reg5, reg6, title = "Table of Results", align = TRUE,
          type = 'html', out = '/Users/anahat/Desktop/490DATA/R/tablereg1.htm', 
          dep.var.labels = c("Reelected in Next Election"), 
          covariate.labels =c("Questions Asked (log)", "Private Member Bills Proposed", 
                              "Statements Made", "Gender", "Age", "Years in House of Commons",
                              "Conservative Member", "Liberal Member", "NDP Member", 
                              "Backbencher"))

colnames(results2019)
colnames(results2015)

rm(results2019)
rm(results2015)