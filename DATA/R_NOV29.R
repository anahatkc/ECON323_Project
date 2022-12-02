# installing packages
install.packages("fastDummies")
install.packages("tidyverse")
install.packages("lmtest")
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

# regression1 - just questionslog and pvtmembills

reg1 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills)
summary(reg1)
colnames(Results)

# regression2 - reg1 + personal characterstics (gender, age)

reg2 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + Results$PvtMemBills + 
            Results$Gender + Results$Age)
summary(reg2)

# regression3 - reg2 + yearsinoffice at time of election, party info
reg3 = lm(Results$ReelectedInNE ~ Results$QuestionsLog + 
            Results$PvtMemBills + Results$Gender + Results$Age +
            Results$YearsInOffice + Results$ConservativeMem + 
            Results$LiberalMem + Results$NDPMem)
summary(reg3)

colnames(results2019)
colnames(results2015)

rm(results2019)
rm(results2015)