library(ggplot2)

set.seed(1901)
lambda <- 0.2
mu <- 1/lambda
sigma <- 1/lambda
n <- 40
nsim <- 1000
m <- matrix(rexp(n*nsim,lambda),ncol = n)
means <- apply(m, 1, function(x) mean(x))
vars <- apply(m, 1, function(x) var(x))

df <- data.frame(means = means)
gg <- ggplot(df, aes(x = means, fill = as.factor(1))) + geom_histogram(binwidth = .2, aes(y = ..density..), color = "black")
gg <- gg + stat_function(fun = dnorm, arg = list(mean = mu, sd = sigma/sqrt(n)), size = 1, color = "red")
gg <- gg + geom_vline(xintercept = mu, color = "blue") + ggtitle("Histogram of sample means vs. theoretical distribution")
gg <- gg + theme(legend.position="none")
gg

ggplot(as.data.frame(vars = vars), aes(x = vars, fill = as.factor(1))) + geom_histogram(binwidth = .2, aes(y = ..density..), color = "black")

sortmeans <- sort(means)
sortref <- sort(rnorm(1000,mu,sigma/sqrt(40)))
ggqq <- ggplot() + geom_point(aes(sortref, sortmeans, color = as.factor(1)), alpha = .5, size = 4)
ggqq <- ggqq + geom_abline(intercept = 0, slope = 1, color = "red")
ggqq <- ggqq + ggtitle("Q-Q plot of sample means distribution") + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
ggqq <- ggqq + theme(legend.position = "none")
ggqq

sortmeans <- sort((means-mu)/(sigma/sqrt(n))) # Normalize sample distribution
sortref <- sort(rnorm(1000))                  # generate 1000 samples of N(0,1)
ggqq <- ggplot() + geom_point(aes(sortref, sortmeans, color = as.factor(1)), alpha = .5, size = 4)
ggqq <- ggqq + geom_abline(intercept = 0, slope = 1, color = "red")
ggqq <- ggqq + ggtitle("Q-Q plot of sample means distribution") 
ggqq <- ggqq + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
ggqq <- ggqq + theme(legend.position = "none") + xlim(-4,4) + ylim(-5,5)
ggsave("qqplot", plot = ggqq)


ggplot(df) + geom_point(aes(x = seq_along(means), y = means), color = "firebrick") + geom_hline(yintercept = mean(means), color = "blue") + geom_polygon(aes(x, y), data = data.frame(x = c(0,1000,1000,0), y = c(4,4,6,6)), alpha = .3)

                                                                                                                             