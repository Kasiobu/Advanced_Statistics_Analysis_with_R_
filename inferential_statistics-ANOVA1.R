# .............INFERENTIAL STATISTIC.........
# In continuation of the linear regression, we shall use one way ANOVA to validate our the linear assumption.
# ANOVA aims to determine whether there are significant differences in the means of the groups 
# at each level of the independent variable.
# ANOVA can be seen as a special case of linear and multiple regression where all predictors are categorical.


# Use library
library(tidyverse)

# load data
df <-read_csv("C:/Users/peace/r/analysis/dst.csv")
view(df)

# ONE WAY ANOVA to check our initial linear regression result
regcheck = aov(time ~ exams, data = df)
regcheck

# Summarise the anova
summary(regcheck)

# Check assumption
plot(regcheck)

# Post-hoc analysis
pairwise.t.test(df$time,df$exams, p.adjust.method="holm")

#testing normality of the residuals
shapiro.test(regcheck$res)

