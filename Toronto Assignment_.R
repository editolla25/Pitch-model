#Question 1

getwd()
# Load the CSV files
training <- read.csv("training.csv")
deploy <- read.csv("deploy.csv")

deploy <- deploy %>%
  filter(SpinRate %in% training$SpinRate)

# Build the logistic regression model on the training data
model_2 <- glm(InPlay ~ Velo + SpinRate + HorzBreak + InducedVertBreak, data = training, family = "binomial")

# Make predictions on the deploy data
predictions <- predict(model, deploy, type = "response") 

# Write predictions to a CSV file
predictions_df <- data.frame(PitchId = 1:nrow(deploy), 
                             InPlayProb = predictions)
write.csv(predictions_df, "predictions.csv", row.names = FALSE)

# Create a scatter plot of spin rate vs. probability of being put in play
ggplot(deploy, aes(x = SpinRate, y = predictions)) +
  geom_point(aes(color = cut(predictions, breaks = seq(0, 1, length.out = 5)))) +
  labs(title = "Probability of Being Put in Play vs. Spin Rate",
       x = "Spin Rate (rpm)",
       y = "Probability of Being Put in Play") + 
  scale_color_discrete(name = "Probability Range")
      
 
# Question 2
# I did come across a couple of bumps in building this as I have some background in coding so with some help of my book and some research online
# this is what has been produced. I came across a couple different errors which had me re-do the model hence there is model_2.
# There were about 200 pitches that were different in regards to spin rates so between the two files I filtered them out to have the same out in both the training and the deploy. The prupose is to determine the probability which is 
# why I inlcuded all the four columns in the model. 

#Question 3
# The four essentials to building a foundation for a pitcher is based on well his fastball plays. Having a high velociy fastball most times 
#comes with being spin efficient, minimal gyro spin, and high induced vertical break which will allow for the pitch to "play" faster up in the strike zone, at the 
#same time it will give the hitter a smaller window/reaction time to get to the ball to put into play.

#Question 4
# To know the count, runners on base, the all around game situation could benefit this model in  determining the probability of the ball going into play
# Also pitch location could also be something added to the model, as well as running this against different teams/players as all players have distinct habits when at the plate
# Maybe add the probability of what type of hit it may be.
       