

# standard backprop
library(RSNNS)
model <- mlp(trainInputs, trainOutputs,
                size=5,
                learnFunc="Std_Backpropagation", learnFuncParams=c(0.02, 0),
                maxit=500,
                linOut = FALSE,
                inputsTest = testInputs,
                targetsTest = testOutputs
)
model <- mlp(x = trainInputs, 
                y = as.numeric(trainOutputs),
                size=c(5,3),
                learnFunc="Quickprop", 
                learnFuncParams=c(0.01, 2.0, 0.0001, 0.1),
                maxit=100,
                inputsTest = testInputs,
                targetsTest = testOutputs,
                linOut = FALSE
)
model <- mlp(x = trainInputs, 
             y = as.numeric(trainOutputs),
             #y = trainOutputs,
                size=5,
                learnFunc="BackpropWeightDecay", 
                learnFuncParams=c(0.1, .0005, 0, 0),
                maxit=500,
                inputsTest = testInputs,
                targetsTest = testOutputs,
                linOut = FALSE
)
#print(model)
summary(model)
# predict on test set
predict_test <- predict(model, testInputs)

# Initial model plots on cross-validation data
par(mfrow=c(2,2))
plotIterativeError(model, ylim = errlim) # SSE error vs. training iteration
plotRegressionError(testOutputs, predict_test) # regression quality
plotROC(fitted.values(model), trainOutputs) # ROC on training data
plotROC(predict_test, testOutputs)

detach(RSNNS)

## nnet vs neuralnet
library(neuralnet)

nn.bp <- neuralnet(fo, data = trainData,
                   hidden=2,
                   threshold = 0.01,
                   stepmax = 1e+5,
                   learningrate = 0.01,
                   lifesign = "full",
                   algorithm = "backprop",
                   err.fct = "ce",
                   act.fct = "logistic",
                   linear.output = FALSE
)
nn.bp

nn <- neuralnet(fo, data = trainData,
                   hidden=2,
                   threshold = 0.01,
                   stepmax = 1e+5,
                   learningrate = 0.01,
                   lifesign = "full",
                   algorithm = "rprop+",
                   err.fct = "ce",
                   act.fct = "logistic",
                   linear.output = FALSE
)
nn
detach(neuralnet)


library(nnet)
nn.nnet <- nnet(fo, data = trainData, 
                size = 5,
                #entropy = T,
                abstol = 0.01,
                decay = 5e-4,
                maxit = 5000,
                linout = FALSE
                )
nn.nnet <- nnet(x = trainInputs,
                #y = trainOutputs,
                y = as.numeric(trainOutputs),
                size = 5,
                entropy = T,
                abstol = 0.01,
                decay = 5e-4,
                maxit = 5000,
                linout = FALSE
)



nn.nnet

detach(nnet)