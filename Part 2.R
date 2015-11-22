library(datasets)
library(ggplot2)
data("ToothGrowth")

str(ToothGrowth)
unique(ToothGrowth$dose)

ge <- ggplot(ToothGrowth, aes(factor(dose), len))
ge <- ge + geom_violin(aes(fill = factor(supp)))
ge <- ge + geom_boxplot(aes(fill = factor(supp)), width = .2, position = position_dodge(.9), outlier.colour = "red", outlier.size = 3)
ge <- ge + xlab("Dose [mg]")
ge <- ge + ylab(expression(paste("Odontoblasts lenght [", mu,"m]")))
ge <- ge + ggtitle("Odontoblasts lenght as a function of dose given supp")
# ge <- ge + geom_jitter(alpha=0.5, aes(color = supp), position = position_jitter(width = 0.1))
ge <- ge + stat_summary(aes(color = supp), fun.y = mean, geom = "point", size = 4, shape = 5)
ge <- ge + scale_color_discrete(name = "Mean for\ndelivery method")
ge <- ge + scale_fill_discrete(name = "Delivery\nmethod")
ge

# Hypotheses for testing
# 
# 1. There is difference in length between OJ and VC dose-wise (OJ vs VC | dose)
# 2. There is advantage of higher doses for both OJ and VC (0.5 vs 2 | supp)
# 
# Assumptions:
# 1. Approximately normal distributions of data in each group
# 2. Unequal variances (based on violin plot)
# 3. Uncontrolled variables are randomized across groups
# 
# 