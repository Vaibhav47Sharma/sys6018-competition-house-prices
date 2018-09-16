# We will perform imputation with Hmisc's argimpute
library(Hmisc)
str(df.train.test)
var.names = colnames(dummy.var)[1 : 50]
fmla = as.formula(paste("~", paste(names(missing), collapse=" +")))
df.train.test.imputed = aregImpute(fmla, data = df.train.test, n.impute = 1)
paste(var.names, collapse=" + ")
as.formula("BsmtUnfSF + TotalBs")

dummy.var = df.train.test %>% rename(variable = BsmtUnfSF)
dummy.var = df.train.test
dummy.var$BsmtUnfSF = NULL
dummy.var$BsmtFinType2 = NULL
table(is.na(dummy.var))
# FALSE   TRUE 
# 218752   3092 
dummy.var$BsmtFinType1 = NULL
table(is.na(dummy.var))
# FALSE   TRUE 
# 215912   3013 
dummy.var = df.train.test[complete.cases(df.train.test)]
table(is.na(dummy.var))
as.formula("~ BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBs + BsmtFinType1")
paste(" ~ ", paste(var.names, collapse=" +"))
as.formula("~  GrLivArea +BsmtFullBath +BsmtHalfBath +FullBath +HalfBath")
fmla

# missForest imputation
library(missForest)
missForest(as.matrix(df.train.test))
str(df.train.test)
table(df.train.test$Exterior1st)

numeric.vecs
numeric.vecs = which(sapply(df.train.test, is.numeric))
categorical.vecs = which(sapply(df.train.test, is.factor))

df.numeric.imputed = df.train.test %>%
  transmute_at(vars(numeric.vecs), funs(func={ifelse(is.na(.), mean(., na.rm=T), .)}))
df.categorical.imputed = df.train.test %>%
  transmute_at(vars(categorical.vecs), funs(func = {ifelse(is.na(.), recode(NA), .)})) %>% missmap

df.numeric.imputed
numeric.vecs
str(df.train.test)
    