# S1 - Read csv with data.table
library(data.table)
library(tidyverse)
library(microbenchmark)
library(dplyr)

# Sequential time taken using micro benchmark
df <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  list.files(path = "C:/Users/kelvi/OneDrive/Documents/5011CEM_QuahZhiChao/", pattern = "*.csv") %>%
    map_df(~fread(.))
}
mbm1 <- microbenchmark("Sequential Processing"={lapply(1:100, df)})
mbm1

library(ggplot2)
autoplot(mbm1)

# Parallel time taken using micro benchmark===========================================================
df2 <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  list.files(path = "C:/Users/kelvi/OneDrive/Documents/5011CEM_QuahZhiChao/", pattern = "*.csv") %>%
    map_df(~fread(.))
}

df3 <- function(i){
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, df2)
  stopCluster(cl)
}
mbm2 <- microbenchmark(lapply(1:100, df2), df3())
mbm2

library(ggplot2)
autoplot(mbm2)

# S2 - Read csv with data.table======================================================================
da1 <-read.csv ("C:/Users/kelvi/OneDrive/Documents/5011CEM_QuahZhiChao/dataset.csv")
str(da1)
summary(da1)
std<-sd(da1$population)
print(std)
getwd()

library(dplyr)

# Correlation between the numerical data 
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model2 <- select(da1, 'age_0_17', 'age_18_64', 'num_transactions')
mosthighlycorrelated(model2, 20)


#Correlation of consumer==============================================================================
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model2 <- select(da1, 'weight', 'volume', 'population')
mosthighlycorrelated(model2, 20)


# Correlation Visualization===========================================================================
library ("ggpubr")
ggscatter(da1, x = "age_0_17", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "age between 0 to 17 consumer", ylab = "number of transaction")

library ("ggpubr")
ggscatter(da1, x = "age_18_64", y = "num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "age between 18 to 64 consumer", ylab = "number of transaction")

# Correlation Visualization of consumer ===========================================================================
library ("ggpubr")
ggscatter(da1, x = "weight", y = "population", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "weight", ylab = "population")

# Correlation Visualization of consumer ===========================================================================
library ("ggpubr")
ggscatter(da1, x = "volume", y = "population", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "volume", ylab = "population")


# Hypothesis testing code==============================================================================
num_trans_0_17 <- lm(num_transactions ~ age_0_17, data = da1)
print(summary(num_trans_0_17))

num_trans_18_64 <- lm(num_transactions ~ age_18_64, data = da1)
print(summary(num_trans_18_64))

vol <- lm(population ~ volume, data = da1)
print(summary(vol))

wei <- lm(population ~ weight, data = da1)
print(summary(wei))

#Regression Analysis===================================================================================

#Consumer age between 0 to 17
# Create the predictor and response variable.
x <- c(da1$age_0_17)
y <- c(da1$num_transactions)
relation <- lm(y~x)
# Plot the chart.
plot(y,x,col = "blue",main = "Number of transactions in age between 0 to 17 consumer  ",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age between 0 to 17",ylab = "number of transactions")


#Consumer age between 18 to 64
# Create the predictor and response variable.
x <- c(da1$age_18_64)
y <- c(da1$num_transactions)
relation <- lm(y~x)
# Plot the chart.
plot(y,x,col = "blue",main = "Number of transactions in age between 18 to 64 consumer  ",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age between 18 to 64",ylab = "number of transactions")


#weight_consumer
x <- c(da1$population)
y <- c(da1$weight)
relation <- lm(y~x)
# Plot the chart.
plot(y,x,col = "blue",main = "The consumer on weight",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "population",ylab = "weight")

#volume_consumer
x <- c(da1$population)
y <- c(da1$volume)
relation <- lm(y~x)
# Plot the chart.
plot(y,x,col = "blue",main = "The consumer on volume",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "population",ylab = "volume")

# Save the file.
dev.off()
