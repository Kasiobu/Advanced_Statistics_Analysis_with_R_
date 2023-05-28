# Exploratory Data analysis.........
# We shall be analysing the time a system takes to complete batches of exams.
# Linear regression will be used for build our model. Secondly, our assumption will be checked with One-way Anova.

# Use library
library(tidyverse)
library(ggplot2)
library(qqplotr)

# Now, load data
df <-read_csv("C:/Users/peace/r/analysis/dst.csv")
view(df)

# summarise dataset
summary(df)
# df Dimension
dim(df)
str(df)

# Correlation
#cor(df)
cor.test(df$exams, df$time, method = "pearson")
# cor.test(df$exams, df$time, method = "kendall")

# Exams distrubution with Histogram
ggplot(df, aes(exams)) +
  geom_histogram(binwidth = 3, col="pink", alpha=0.7)+
  ggtitle("Batches in each Exams category") +
  theme_bw()

# show distribution with scatter plot
# plot(df$time, col="blue")
ggplot(df, aes(exams, time)) +
  geom_point(color="purple") +
  ggtitle("Scatter Plot show the time distribution") +
  theme_bw()

# show distribution with boxplot
#boxplot(df, col="pink")
ggplot(df, aes(exams,time))+
  geom_boxplot(stat="boxplot", outlier.color="red", position="dodge2")+
  theme_bw()

qqnorm(df$time)
qqline(df$time)

###########################
# Classical data analysis #
###########################

# Linear regression model
lin = lm(time~exams, df)
summary(lin)

# Check assumption with normality plot etc
plot(lin)


# It's more assuring to TRANSFORM data that seems unclear into a linear form, Just to play safe 
trans_lin = lm(time~log(exams), df)
summary(trans_lin)

# Now, check assumption with q-q normality plot, etc 
plot(trans_lin)


# Create dataset
df1 = c(df$time)
df1


# Check the confidence interval (Mean)
t.test(df1, conf.level=0.9)

