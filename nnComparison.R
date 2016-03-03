

# standard backprop
library(RSNNS)
modelstd <- mlp(trainInputs, trainOutputs,
                size=5,
                learnFunc="Std_Backpropagation", learnFuncParams=c(0.01, 0),
                maxit=500,
                linOut = FALSE,
                inputsTest = cvInputs,
                targetsTest = cvOutputs
)
modelstd <- mlp(trainInputs, trainOutputs,
                size=5,
                learnFunc="Quickprop", 
                learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),
                maxit=500,
                linOut = FALSE
)
modelstd <- mlp(trainInputs, trainOutputs,
                size=5,
                learnFunc="BackpropWeightDecay", 
                learnFuncParams=c(0.2, .01, 0, 0),
                maxit=500,
                linOut = FALSE
)
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