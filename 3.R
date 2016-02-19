
# EXPLORATORY ANALYSIS
# explore the dataset
help(Titanic)
class(Titanic)
Titanic

# transform the dataset
dfTitanic <- data.frame(Titanic)
str(dfTitanic)
library(data.table)
dtTitanic <- data.table(dfTitanic)
head(dtTitanic)

# explore the survived attribute
nTot <- dtTitanic[, sum(Freq)]
nTot
dtSurvived <- dtTitanic[, list(n=sum(Freq)), by='Survived']
dtSurvived

# build a histogram about the survived attribute
help(barplot)
vectorHeight <- dtSurvived[, n]
vectorNames <- dtSurvived[, Survived]
barplot(height=vectorHeight, names.arg=vectorNames)
barplot(height=dtSurvived[, n], names.arg=dtSurvived[, Survived])

# build a nice histogram
dtSurvived[, percentage := n / sum(n)]
dtSurvived[, colorPlot := ifelse(Survived == 'Yes', 'blue', 'red')]
barplot(
  height=dtSurvived[, percentage],
  names.arg=dtSurvived[, Survived],
  col=dtSurvived[, colorPlot],
  ylim=c(0, 1)
)
dtSurvived[, textPercentage := paste(round(percentage * 100), '%', sep='')]
plotTitle <- 'Proportion of passengers surviving or not'
ylabel <- 'percentage'
barplot(
  height=dtSurvived[, percentage],
  names.arg=dtSurvived[, Survived],
  col=dtSurvived[, colorPlot],
  ylim=c(0, 1),
  legend.text=dtSurvived[, textPercentage],
  ylab=ylabel,
  main=plotTitle
)

# build a nice histogram for the male survival rate
dtGender <- dtTitanic[, list(n=sum(Freq)), by=c('Survived', 'Sex')]
dtGender
dtGender[, percentage := n / sum(n), by='Sex']
dtGender[, colorPlot := ifelse(Survived == 'Yes', 'blue', 'red')]
dtGender[, textPercentage := paste(round(percentage * 100), '%', sep='')]
dtGenderMale <- dtGender[Sex == 'Male']
barplot(
  height=dtGenderMale[, percentage],
  names.arg=dtGenderMale[, Survived],
  col=dtGenderMale[, colorPlot],
  ylim=c(0, 1),
  legend.text=dtGenderMale[, textPercentage],
  ylab='percentage',
  main='Survival rate for the males'
)

# build a nice histogram for the female survival rate
barplot(
  height=dtGender[Sex == 'Female', percentage],
  names.arg=dtGender[Sex == 'Female', Survived],
  col=dtGender[Sex == 'Female', colorPlot],
  ylim=c(0, 1),
  legend.text=dtGender[Sex == 'Female', textPercentage],
  ylab='percentage',
  main='Survival rate for the females'
)

# build a nice histogram for the survival rate by gender
barplot(
  height=dtGender[Survived == 'Yes', percentage],
  names.arg=dtGender[Survived == 'Yes', Sex],
  col=dtGender[Survived == 'Yes', Sex],
  ylim=c(0, 1),
  legend.text=dtGender[Survived == 'Yes', textPercentage],
  ylab='percentage',
  main='Survival rate by gender'
)

# visualize the survival rate by social class
dtClass <- dtTitanic[, list(n=sum(Freq)), by=c('Survived', 'Class')]
dtClass[, percentage := n / sum(n), by='Class']
dtClass[, textPercentage := paste(round(percentage * 100), '%', sep='')]
barplot(
  height=dtClass[Survived == 'Yes', percentage],
  names.arg=dtClass[Survived == 'Yes', Class],
  col=dtClass[Survived == 'Yes', Class],
  ylim=c(0, 1),
  legend.text=dtClass[Survived == 'Yes', textPercentage],
  ylab='survival rate',
  main='Survival rate by class'
)

# visualize the gender rate by social class
dtGenderFreq <- dtTitanic[, list(n=sum(Freq)), by=c('Sex', 'Class')]
dtGenderFreq[, percentage := n / sum(n), by='Class']
dtGenderFreq <- dtGenderFreq[Sex == 'Female']
dtGenderFreq[, textPercentage := paste(round(percentage * 100), '%', sep='')]
barplot(
  height=dtGenderFreq[, percentage],
  names.arg=dtGenderFreq[, Class],
  col=dtGenderFreq[, Class],
  ylim=c(0, 1),
  legend.text=dtGenderFreq[, textPercentage],
  ylab='survival rate',
  main='Percentage of females'
)

# Visualize the survival rate by social class and gender
dtGenderClass <- dtTitanic[, list(n=sum(Freq)), by=c('Survived', 'Sex', 'Class')]
dtGenderClass[, nTot := sum(n), by=c('Sex', 'Class')]
dtGenderClass[, percentage := n / sum(n), by=c('Sex', 'Class')]
dtGenderClass <- dtGenderClass[Survived == 'Yes']
dtGenderClass[, textPercentage := paste(round(percentage * 100), '%', sep='')]
dtGenderClass[, colorPlot := rainbow(nrow(dtGenderClass))]
dtGenderClass[, SexAbbr := ifelse(Sex == 'Male', 'M', 'F')]
dtGenderClass[, barName := paste(Class, SexAbbr, sep='')]
dtGenderClass[, barLabel := paste(barName, nTot, sep='\n')]
barplot(
  height=dtGenderClass[, percentage],
  names.arg=dtGenderClass[, barLabel],
  col=dtGenderClass[, colorPlot],
  xlim=c(0, 11),
  ylim=c(0, 1),
  ylab='survival rate',
  legend.text=dtGenderClass[, textPercentage]
)

# explore the three features combines
dtTitanic[, nTot := sum(Freq), by=c('Sex', 'Class', 'Age')]
dtTitanic[, percentage := Freq / nTot]
dtAll <- dtTitanic[Survived == 'Yes', ]
dtAll[, ClassAbbr := substring(Class, 1, 1)]
dtAll[, SexAbbr := ifelse(Sex == 'Male', 'M', 'F')]
dtAll[, AgeAbbr := ifelse(Age == 'Child', 'C', 'A')]
dtAll[, textLegend := paste(ClassAbbr, SexAbbr, AgeAbbr, sep='')]
dtAll[, colorPlot := rainbow(nrow(dtAll))]
dtAll[, labelPerc := paste(round(percentage * 100), '%', sep='')]
dtAll[, label := paste(labelPerc, nTot, sep='\n')]
barplot(
  height=dtAll[, percentage],
  names.arg=dtAll[, label],
  col=dtAll[, colorPlot],
  xlim=c(0, 23),
  legend.text=dtAll[, textLegend],
  cex.names=0.5
)


# EXPLORATORY DECISION TREE

# install and load the packages
install.packages('rpart')
install.packages('rpart.plot')
library('rpart')
library('rpart.plot')

# prepare the decision tree inputs
dtLong <- dtTitanic[
  , list(Freq = rep(1, Freq)),
  by=c('Survived', 'Sex', 'Age', 'Class')
  ]
dtLong[, Freq := NULL] #removes Freq column
dtLong[, Survived := ifelse(Survived == 'Yes', 1, 0)] #replaces Yes with 1, otherwise 0
head(dtLong)
help(rpart)
formulaRpart <- formula('Survived ~ Sex + Age + Class')

# build the decision trees
treeRegr <- rpart(
  formula=formulaRpart,
  data=dtLong
)
treeClass = rpart(
  formula='Survived ~ Sex + Age + Class',
  data=dtLong,
  method='class'
)

# visualize the decision trees
prp(treeRegr)
prp(treeClass)


# PREDICTION WITH RANDOM FOREST

# install and load the package
install.packages('randomForest')
library('randomForest')

# prepare the data (TRUE/FALSE inputs)
dtDummy <- copy(dtLong)
dtDummy[, Male := Sex == 'Male'] # adds Male clomn with TRUE values for sex=male
dtDummy[, Sex := NULL]
dtDummy[, Child := Age == 'Child']
dtDummy[, Age := NULL]
dtDummy[, Class1 := Class == '1st']
dtDummy[, Class2 := Class == '2nd']
dtDummy[, Class3 := Class == '3rd']
dtDummy[, Class := NULL]

# build a default Random Forest
formulaRf <- formula('Survived ~ Male + Child + Class1 + Class2 + Class3')
forest <- randomForest(
  formula=formulaRf,
  data=dtDummy,
  type = 'regression'
)

# explore the Random Forest model
forest$ntree
forest$mtry
forest$type

# build a Random Forest setting parameters
forest <- randomForest(
  formula=formulaRf,
  data=dtDummy,
  ntree=1000,
  mtry=3,
  sampsize=1500
)

# predict the survival rate on a row
rowRandom <- dtDummy[100]
predict(forest, rowRandom)

# predict the survival rates
prediction = predict(forest, dtDummy)
sample(prediction, 6)
#dtDummy[, SurvivalRatePred := predict(forest, dtDummy)] #adds prediction column to data (wrong)
dtDummy[, SurvivedPred := predict(forest, dtDummy)]  #[SM] fixed above line

# measure the accuracy (although on the same data)
#dtDummy[, SurvivedPred := ifelse(SurvivalRatePred > 0.5, 1, 0)] #[SM] no longer needed
dtDummy[, error := SurvivedPred != Survived]
percError <- dtDummy[, sum(error) / .N]
percError

# determine the error baseline
dtTitanic[Survived == 'No', sum(Freq)] / dtTitanic[, sum(Freq)]

# VALIDATE THE RANDOM FOREST

# split the dataset
indexTrain <- sample(
  x=c(TRUE, FALSE),
  size=nrow(dtDummy),
  replace=TRUE,
  prob=c(0.8, 0.2)
)
dtTrain <- dtDummy[indexTrain] # training set
dtTest <- dtDummy[!indexTrain] # test set

# build the random forest using the training set
forest <- randomForest(
  formula=formulaRf,
  data=dtTrain,
  ntree=1000,
  mtry=3,
  sampsize=1200
)

# measure the accuracy on the test set
dtTest[, SurvivedPred := predict(forest, dtTest)] # chaged to SurvivedPred as above
#dtTest[, SurvivedPred := ifelse(SurvivalRatePred > 0.5, 1, 0)] #[SM] not needed
dtTest[, error := SurvivedPred != Survived]
percError <- dtTest[, sum(error) / .N]
percError

## Same as above but uses x instead of formula in randomforest()

# training predictors & response data
dtTrain2 <- dtTrain[,.(Male,Child,Class1,Class2,Class3)] # removed Survived column
dtResp2 <- dtTrain[,Survived]

#using all data, compare to first example above 
# gives same error rate as above example using formula argument
# dtTrain2 <- dtDummy[,.(Male,Child,Class1,Class2,Class3)]
# dtResp2 <- dtDummy[,Survived]

# test data
dtTest2 <- dtTest[,.(Male,Child,Class1,Class2,Class3)]
dtTestResp2 <- dtTest[,Survived]

forest2 <- randomForest(x=dtTrain2,y=dtResp2,  ntree=1000, mtry=3, sampsize=1200)

# Added test set
# Calculates error rates for training & test data
# $importance
# $err.rate gives error rate for i'th tree - use to judge ntrees?
forest2 <- randomForest(x=dtTrain2, 
                        y=dtResp2,
                        xtest = dtTest2,
                        ytest = dtTestResp2,
                        ntree=1000,
                        mtry=3,
                        sampsize=1200,
                        importance=TRUE
                        )
forest2

# Error rates manually 
sum(forest2$predicted != dtResp2) / length(dtResp2)
sum(forest2$test$predicted != dtTestResp2) / length(dtTestResp2)
