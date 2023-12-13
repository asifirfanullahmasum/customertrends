library(tidyverse)
library(ggplot2)
library(leaps)
library(nnet)
library(tree)
library(randomForest)

csv_file_path <- 'shopping_trends_updated.csv'
shopping_data <- read.csv(csv_file_path)
head(shopping_data)

###############################################################################
########                      DATA PREPROCESSING                    ###########
###############################################################################

region_mapping <- c(
  'Connecticut' = 'Northeast Region',
  'Delaware' = 'Northeast Region',
  'Maine' = 'Northeast Region',
  'Maryland' = 'Northeast Region',
  'Massachusetts' = 'Northeast Region',
  'New Hampshire' = 'Northeast Region',
  'New Jersey' = 'Northeast Region',
  'New York' = 'Northeast Region',
  'Pennsylvania' = 'Northeast Region',
  'Rhode Island' = 'Northeast Region',
  'Vermont' = 'Northeast Region',
  
  'Alabama' = 'Southeast Region',
  'Arkansas' = 'Southeast Region',
  'Florida' = 'Southeast Region',
  'Georgia' = 'Southeast Region',
  'Kentucky' = 'Southeast Region',
  'Louisiana' = 'Southeast Region',
  'Mississippi' = 'Southeast Region',
  'North Carolina' = 'Southeast Region',
  'South Carolina' = 'Southeast Region',
  'Tennessee' = 'Southeast Region',
  'Virginia' = 'Southeast Region',
  'West Virginia' = 'Southeast Region',
  
  'Illinois' = 'Midwest Region',
  'Indiana' = 'Midwest Region',
  'Iowa' = 'Midwest Region',
  'Kansas' = 'Midwest Region',
  'Michigan' = 'Midwest Region',
  'Minnesota' = 'Midwest Region',
  'Missouri' = 'Midwest Region',
  'Nebraska' = 'Midwest Region',
  'North Dakota' = 'Midwest Region',
  'Ohio' = 'Midwest Region',
  'South Dakota' = 'Midwest Region',
  'Wisconsin' = 'Midwest Region',
  
  'North Dakota' = 'Great Plains Region',
  'South Dakota' = 'Great Plains Region',
  'Nebraska' = 'Great Plains Region',
  'Kansas' = 'Great Plains Region',
  'Oklahoma' = 'Great Plains Region',
  'Texas' = 'Great Plains Region',
  
  'Arizona' = 'Southwest Region',
  'New Mexico' = 'Southwest Region',
  'Oklahoma' = 'Southwest Region',
  'Texas' = 'Southwest Region',
  
  'Colorado' = 'Rocky Mountain Region',
  'Idaho' = 'Rocky Mountain Region',
  'Montana' = 'Rocky Mountain Region',
  'Utah' = 'Rocky Mountain Region',
  'Wyoming' = 'Rocky Mountain Region',
  
  'Alaska' = 'West Coast Region',
  'California' = 'West Coast Region',
  'Hawaii' = 'West Coast Region',
  'Nevada' = 'West Coast Region',
  
  'Oregon' = 'Pacific Northwest Region',
  'Washington' = 'Pacific Northwest Region'
)

shopping_data$Region <- region_mapping[shopping_data$Location]

frequency_mapping <- c('Weekly' = 52, 'Monthly' = 12, 'Annually' = 1, 'Quarterly' = 4, 'Bi-Weekly' = 26, 'Fortnightly' = 26, 'Every 3 Months' = 4)
shopping_data$Frequency.Numeric <- frequency_mapping[shopping_data$Frequency.of.Purchases]

min.purchase <- min(shopping_data$Purchase.Amount..USD.)
max.purchase <- max(shopping_data$Purchase.Amount..USD.)

shopping_data$Avg.Purchase.Amount <- cut(
  shopping_data$Purchase.Amount..USD.,
  breaks = c(min.purchase, 31, 60, 81, max.purchase),
  labels = c(15, 45, 70, 90),
  include.lowest = TRUE
)
View(shopping_data)

subset_list = c('Avg.Purchase.Amount', 'Age', 'Gender', 'Region', 'Size', 'Color','Season', 'Frequency.Numeric')
shopping_data_subset = shopping_data[subset_list]
View(shopping_data_subset)
###############################################################################
########                        DATA ANALYSIS                       ###########
###############################################################################

num_cols <- 1

# Question 1: What is the distribution of customers based on age and gender?
age_gender_distribution <- shopping_data %>%
  group_by(Age, Gender) %>%
  summarise(CustomerCount = n())

ggplot(age_gender_distribution, aes(x = Age, fill = Gender, y = CustomerCount)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Customers based on Age and Gender")

# Question 2: Which items are the most frequently purchased?
top_items <- shopping_data$Item.Purchased %>% table() %>% sort(decreasing = TRUE) %>% head(10)
all_items <- shopping_data$Item.Purchased %>% table()

top_ten_mask <- shopping_data$Item.Purchased %in% names(top_items)

ggplot(shopping_data, aes(x = Item.Purchased, fill = factor(Item.Purchased %in% names(top_items)))) +
  geom_bar() +
  labs(title = "Top 10 Most Frequently Purchased Items Highlighted",
       x = "Item", y = "Frequency") +
  scale_fill_manual(values = c('skyblue', 'orange'), name = 'Legend',
                    labels = c('Other Items', 'Top 10 Items')) +
  theme_minimal() +
  theme(legend.position = "top")

# Question 3. What are the different items purchased by different Age Group and Gender?
age_groups <- list(
  `0-18` = c(0, 18),
  `19-30` = c(19, 30),
  `31-45` = c(31, 45),
  `46-60` = c(46, 60),
  `61+` = c(61, Inf)
)

map_age_to_group <- function(age) {
  for (group in names(age_groups)) {
    range <- age_groups[[group]]
    if (age >= range[1] && age <= range[2]) {
      return(group)
    }
  }
  return('Unknown')
}

subset_agegroup_data <- shopping_data %>%
  mutate(AgeGroup = sapply(Age, map_age_to_group))

colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")

fig <- ggplot(subset_agegroup_data, aes(x = Item.Purchased, fill = AgeGroup)) +
  facet_wrap(~AgeGroup, scales = "free_y", ncol = num_cols) +
  geom_bar(position = "stack", color = "black") +
  scale_fill_manual(values = colors) +
  labs(title = "Items Purchased by Age Group", x = "Item Purchased", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        legend.position = "none")   

print(fig)

gender_colors <- c("#1f78b4", "#e31a1c")

fig <- ggplot(shopping_data, aes(x = Item.Purchased, fill = Gender)) +
  facet_wrap(~Gender, scales = "free_y", ncol = num_cols) +
  geom_bar(position = "stack", color = "black") +
  scale_fill_manual(values = gender_colors) +
  labs(title = "Items Purchased by Gender", x = "Item Purchased", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        legend.position = "none")   

print(fig)


# Question 4. How does the location region impact the type of items purchased?
unique_regions <- unique(shopping_data$Region)
region_colors <- rainbow(length(unique_regions))

fig <- ggplot(shopping_data, aes(x = Item.Purchased, fill = Region)) +
  facet_wrap(~Region, scales = "free_y", ncol = num_cols) +
  geom_bar(position = "stack", color = "black") +
  scale_fill_manual(values = region_colors) +
  labs(title = "Items Purchased by Region", x = "Item Purchased", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10),
        legend.position = "none")   

print(fig)

# Question 5. How does item size and type preference change with seasons?
fig <- ggplot(shopping_data, aes(x = Season, fill = Size)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = 'Item Size Preference Change with Seasons', x = 'Season', y = 'Frequency') +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "yellow")) +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position
print(fig)

fig <- ggplot(shopping_data, aes(x = Item.Purchased, fill = Season)) +
  facet_wrap(~Season, scales = "free_y", ncol = num_cols) +
  geom_bar(color = "black") +
  labs(title = 'Item Type Preference Change with Seasons', x = 'Season', y = 'Frequency') +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "yellow")) +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position
print(fig)

# Question 6. What are the popular sizes and colors among customers?
ggplot(shopping_data, aes(x = Size, fill = Size)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = 'Popular Sizes Among Customers', x = 'Size', y = 'Frequency') +
  theme_minimal() +
  theme(legend.position = "none")   

ggplot(shopping_data, aes(x = Color, fill = Color)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = 'Popular Colors Among Customers', x = 'Color', y = 'Frequency') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")   

###############################################################################
########                       MODEL PREDICTION                     ###########
###############################################################################

set.seed(1011)
kfolds <- 5
folds <- rep_len(1:kfolds, dim(shopping_data_subset)[1])
folds <- sample(folds, dim(shopping_data_subset)[1])

logistic.test.accuracy.fold <- rep(0, kfolds)
svm.test.accuracy.fold <- rep(0, kfolds)
rf.test.accuracy.fold <- rep(0, kfolds)

for(k in 1:kfolds){
  fold <- which(folds == k)
  
  train.set <- shopping_data_subset[-fold, ]
  test.set <- shopping_data_subset[fold, ]
  p.by3 <- round(ncol(train.set)/3)
  
  logistic.model <- multinom(Avg.Purchase.Amount ~ ., data = train.set)
  logistic.predictions <- predict(logistic.model, newdata = test.set, type = "class")
  logistic.test.accuracy <- sum(logistic.predictions == test.set$Avg.Purchase.Amount) / nrow(test.set)
  logistic.test.accuracy.fold[k] <- logistic.test.accuracy
  
  svm.model <- svm(Avg.Purchase.Amount ~ ., data = train.set, kernel = "radial")
  svm.predictions <- predict(svm.model, newdata = test.set)
  svm.conf.matrix <- table(svm.predictions, test.set$Avg.Purchase.Amount)
  svm.test.accuracy <- sum(diag(svm.conf.matrix)) / sum(svm.conf.matrix)
  svm.test.accuracy.fold[k] <- svm.test.accuracy
  
  rf.model <- randomForest(Avg.Purchase.Amount ~ ., data = train.set, mtry = p.by3, importance = TRUE)
  rf.predictions <- predict(rf.model, newdata = test.set)
  rf.conf_matrix <- table(Actual = test.set$Avg.Purchase.Amount, Predicted = rf.predictions)
  rf.test.accuracy <- sum(diag(rf.conf_matrix)) / sum(rf.conf_matrix)
  rf.test.accuracy.fold[k] <- rf.test.accuracy
}

cv.logistic.accuracy <- mean(logistic.test.accuracy.fold)
cv.logistic.accuracy

cv.svm.accuracy <- mean(svm.test.accuracy.fold)
cv.svm.accuracy

cv.rf.accuracy <- mean(rf.test.accuracy.fold)
cv.rf.accuracy

best.model.accuracy <- max(c(cv.logistic.accuracy, cv.svm.accuracy, cv.rf.accuracy))
best.model.accuracy