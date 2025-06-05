library(ggplot2)
library(dplyr)

# Your trained model: e.g. model <- randomForest(target ~ ., data = train_data)
# Your data: your_data
# Your original point: x_interest[1, ]
# Your counterfactual: cfactuals[1, ]

# Step 1: Create a grid over FSEARN and FSDEPDED
fearn_range <- seq(min(train_df$FSEARN) * 0.9, max(train_df$FSEARN) * 1.1, length.out = 100)
fdep_range <- seq(min(train_df$FSDEPDED) * 0.9, max(train_df$FSDEPDED) * 1.1, length.out = 100)

grid <- expand.grid(FSEARN = fearn_range, FSDEPDED = fdep_range)

# Step 2: Add median values for all other features
for (col in setdiff(names(train_df), c("FSEARN", "FSDEPDED", "IS_APPROVED"))) {
  grid[[col]] <- median(train_df[[col]], na.rm = TRUE)
}



# Step 3: Predict probabilities
grid$pred <- predict(rf, newdata = grid, type = "response")
grid$pred <- as.numeric(as.character(grid$pred))
# Step 4: Add original and counterfactual points
original_point <- x_interest[1999, ]
cf_point <- cfactuals$data[7,]

# Step 5: Plot
ggplot(grid, aes(x = FSEARN, y = FSDEPDED)) +
  geom_tile(aes(fill = pred), alpha = 0.7) +
  geom_contour(aes(z = pred), breaks = 0.5, color = "black") +
  geom_point(data = original_point, aes(x = FSEARN, y = FSDEPDED), color = "red", size = 3) +
  geom_point(data = cf_point, aes(x = FSEARN, y = FSDEPDED), color = "green", size = 3) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Decision Boundary with Original and Counterfactual",
       fill = "P(class = 1)")
