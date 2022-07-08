# Statistical tests I – comparisons and correlations

#While there are many different statistical tests that can be used to examine every facet of your data they all have one aim in common,
#to significantly differentiate any pattern in your data from one that could be caused by random variation:
#that is, is your observed pattern likely to have a cause beyond chance alone? This is referred to as a null hypothesis

library(strap)

asaphus<-Asaphidae$ages
# Descriptive statistics



# mean
mean(asaphus$FAD)

# median
median(asaphus$FAD)

# summary
summary(asaphus)

# standard deviation
sd(asaphus$FAD)

# the values for ± 1SD are retrieved by the following commands:

mean(asaphus$FAD) + sd(asaphus$FAD)
mean(asaphus$FAD) - sd(asaphus$FAD)

# Testing for a normal distribution
# A common visual way to test for the normality of a distribution is the quantile-quantile (Q-Q) plot that plots the ranked quantiles of your data against a distribution of theoretical quantiles taken from a normal distribution.
# A normal sample will show a straight line, while a non-normal distribution displays a deviation from a straight line, typically as an S-shape. 

# we can compare the Asaphus data with a randomlygenerated dataset of 1,000 values taken from a normal distribution using the function rnorm.

X <- rnorm(1000)
hist(X)

# A Q-Q plot can then be generated for both the random and the Asaphus datasets using the code below

par(mfrow=c(1,2))
qqnorm(X)
qqline(X)
qqnorm(log(asaphus$FAD))
qqline(log(asaphus$FAD))
hist(asaphus$FAD)

# A formal test for normality is to use the Shapiro-Wilk test 
shapiro.test(X)
shapiro.test(asaphus$FAD)

# Two sample comparisons

sample1<-sample(asaphus$FAD, size = 15,replace = T)
  
sample2<-sample(asaphus$FAD,size = 20,replace = T)

boxplot(sample1,sample2)

# are these samples  statistically different?

t.test(sample1, sample2)

# I mentioned that an assumption of the t-test is that the data are normally distributed. 
# As we have already seen this is not true of the Asaphidae data provided here. 

wilcox.test(sample1, sample2)

# Linear correlations
# Correlations are an important and commonly-used statistical test, 
# used when you want to examine whether two sets of data show a statistical relationship.

x <- c(-3,-1,1,2)
y <- c(-2,-1,2,3)


# The Pearson product-moment correlation coefficient (typically written as r or R) is a measure of the linear correlation between the two variables. 

# 'manual' computation
cov(x,y) / (sd(x) * sd(y))

# built-in test
cor.test(x,y)

# Another statistic that is commonly quoted along with the results of linear correlations is the r-squared value (r2 or R2), also known as the coefficient of determination. 
# In the case of linear correlations this is typically calculated as the squared value of the correlation coefficient and represents how much of the variation of one variable is explained by the second.

(cov(x,y) / (sd(x) * sd(y)))^2



