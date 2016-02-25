

# standard backprop
modelstd <- mlp(trainInputs, trainOutputs,
                size=2,
                learnFunc="Std_Backpropagation", learnFuncParams=c(0.01, 0),
                maxit=500,
                linOut = FALSE,
                inputsTest = cvInputs,
                targetsTest = cvOutputs
)

## nnet vs neuralnet
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

nn.nnet <- nnet(fo, data = trainData, size = 2, entropy = T, abstol = 0.01)
nn.nnet