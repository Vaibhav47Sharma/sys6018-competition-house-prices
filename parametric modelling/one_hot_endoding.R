# Using caret package for onehot encoding

library(caret)

encoder = dummyVars(" ~ .", df.train.test)

df.train.test.encoded = data.frame(predict(encoder, newdata = df.train.test))

missmap((df.train.test.encoded[101:278]))
