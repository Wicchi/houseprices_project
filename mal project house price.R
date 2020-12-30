library(tidyverse) #utility function
library(rpart) #for regression tree
library(randomForest) #for random forest
library(readr)
melbourne_data <- read_csv("C:/study/Data Science Study/archive/melb_data.csv")
#Now I need divide the data to traning, valid, test
fractionTraning  <- 0.6 # 60% to traning
fractionValidation <- 0.2 # 20% to valid
fractionTest <- 0.2 # 20% to test

#Compute sample size
sampleSizeTraining <- floor(fractionTraning * nrow(melbourne_data))
sampleSizeValidation <- floor(fractionValidation * nrow(melbourne_data))
sampleSizeTest <- floor(fractionTest * nrow(melbourne_data))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.

indicesTraning <- sort(sample(seq_len(nrow(melbourne_data)), size = sampleSizeTraining))
indicesNotTraning <- setdiff(seq_len(nrow(melbourne_data)), indicesTraning)
indicesValidation <- sort(sample(indicesNotTraning, size = sampleSizeValidation))
indicesTest <- setdiff(indicesNotTraning, indicesValidation)

# Output the three dataframes for training, validation and test
ml_dataTraning <- melbourne_data[indicesTraning, ]
ml_dataValid <- melbourne_data[indicesValidation, ]
ml_dataTest <- melbourne_data[indicesTest, ]

# Ill use prart() function from rpart package to build my decision tree
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + 
               YearBuilt + Lattitude + Longtitude, data = ml_dataTraning)
# So I can show the decision tree
plot(fit, uniform = TRUE)
text(fit, cex = .6)

# Now Ill make a predict by using function predict()
print("The prediction are")
print(round(predict(fit, head(ml_dataTraning))))

print("Actual price")
print(head(ml_dataTraning$Price))

# Add result as df 
result_of_traning <- data.frame(Real_price = head(ml_dataTraning$Price), Predicted_price = round(predict(fit, head(ml_dataTraning))))

# now let try to calculate mae to clarify that our model good or bad
library(modelr)
result_of_traning$MAE_decision_tree <- mae(model = fit, data = ml_dataTest)
result_of_traning <- result_of_traning[, !(names(result_of_traning) %in% 'MAE')]

# So mae is to large, probobly the change of depth our dicision tree would help

get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  
  # turn the predictors & target into a formula to pass to rpart()
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  
  # build our model
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  # get the mae
  mae <- mae(model, testing_data)
  return(mae)
}

# Now let's add target, predicator and calculate mae with different depth

target <- 'Price'
predicator <- c("Rooms", "Bathroom", "Landsize", "BuildingArea",
                "YearBuilt", "Lattitude", "Longtitude")
for (i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predicator, 
                 training_data = ml_dataTraning, testing_data = ml_dataTest
                 )
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}

# As we can see the mae still high. So it happened because of that fact that our
# data is not so large, better to try use random forest

fitRandomForest <- randomForest(Price ~ Rooms + Bathroom + Landsize + BuildingArea + 
                           YearBuilt + Lattitude + Longtitude, data = ml_dataTraning, na.action = na.exclude, ntree = 500)
result_of_traning$MAE_randomForest <- mae(model = fitRandomForest, data = ml_dataTest)

# As we can see the mae was decreased, of course we can increase the number of trees in model, that will decrease mae more.



