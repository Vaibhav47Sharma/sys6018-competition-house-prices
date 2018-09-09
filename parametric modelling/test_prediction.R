# Setting df.test$Id to seperate variable
test.id = df.test$Id
df.test$Id = NULL

# Using GLM Model for prediction
prediction = predict(glm_1, df.test)

# Sending solution to csv

solution = data_frame('Id' = test.id, 'SalePrice' = prediction)
write_csv(solution, 'parametric modelling\\solution1.csv')
str(df.test)

