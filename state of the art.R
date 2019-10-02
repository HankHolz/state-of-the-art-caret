# install.packages("caret")
library(caret)

# Loading in data 
# install.packages("mlbench")
library(mlbench)
data("Sonar")

# Get the number of observations
n_obs <- nrow(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)

# Randomly order data: Sonar
Sonar_shuffled <- Sonar[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- Sonar_shuffled[1:split, ]

# Create test
test <- Sonar_shuffled[(split + 1):n_obs, ]



# Fit glm model: model
model <- glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")


# Creating Confusion matrix (finding out probabilities of false positives n shit)
# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > .5, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# True Positive Rate (or Sensitivity): .3
# True Negative Rate (or Specificity): .28

# install.packages("caTools")
library(caTools)
# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)


# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv", # breaks data set into folds
  number = 10, # Number of folds 
  summaryFunction = twoClassSummary, # This says use AUC as parameter
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model <- train(
  Class ~ ., 
  Sonar, 
  method = "glm",
  trControl = myControl
)

# Print model to console
model

# Creating smaller dataset
set.seed(42)
rows <- sample(nrow(ok))
shuffled_Kali <- bs_with_transparency[rows, ]
split <- round(nrow(ok)*.99)
train <- ok[1:split, ]
test <- ok[(split+1):nrow(ok),]

model <- train(
  transparency_score ~. ,
  tunLength = 3,
  data = test,
  na.action = na.exclude,
  method = "ranger",
  trControl = trainControl(
    method = "cv" ,
    number = 5, 
    verboseIter =  TRUE
  )
)
modRF <- train(transparency_score ~ ., 
               data=train, 
               na.action = na.exclude, 
               method="ranger", 
               trControl=trainControl(method="cv",number=5)
)

summary(warnings())
memory.limit()

modRF

plot(modRF)