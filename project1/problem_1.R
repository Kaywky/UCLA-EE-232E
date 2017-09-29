library("igraph")
library("ggplot2")

g = read.graph("/Users/kay/Documents/course/ee232/project1/facebook_combined.txt", directed = FALSE)
is.connected(g)
# TRUE: connected
diameter(g)
#diameter = 8
deg = degree(g)
deg_dis = degree_distribution(g)
plot(deg_dis, type = 'h', main = "Degree Distribution", xlab = "degree", ylab = "density")
avg_degree = mean(deg)
avg_degree
# avg_degree = 43.69101

h = hist(deg, breaks = seq(0, by = 1, length.out = max(deg) + 2),
         freq = F, main = "Degree Distribution of Hist Figure",
         xlab = "degree")
df = data.frame(x = h$mids, y = h$density)

fit1 = nls(y ~ I(exp(1)^(a + b*x)), data = df, start = list(a=0, b=0))
fit2 = nls(y ~ I(1/x*a) + b, data = df, start = list(a=0, b=0))
fit3 = nls(y ~ I(1/x*a) + b*x, data = df, start = list(a=0, b=0))

ggplot(df, mapping = aes(x, y)) + geom_point(size = 1) + 
  stat_smooth(method = "nls", 
              method.args = list(formula = y ~ I(exp(1)^(a + b*x)), start = list(a=0, b=0)),
              size = 1, se = FALSE) +
  stat_smooth(method = "nls",
              method.args = list(formula = y ~ I(1/x*a) + b, start = list(a=0, b=0)),
              size = 1, se = FALSE, color = 'red') + 
  stat_smooth(method = "nls",
              method.args = list(formula = y ~ I(1/x*a) + b*x, start = list(a=0, b=0)),
              size = 1, se = FALSE, color = 'yellow')

summary(fit1)
summary(fit2)
summary(fit3)

# fit1 model fits best, from summary reports, we know that:
a_res = -3.5940045
b_res = -0.0291488
#thus:
df_curve = data.frame(x=h$mids, y=exp(1)^(a_res + b_res*h$mids))
total_mse = sum((df_curve$y - df$y)^2/max(deg))
total_mse
# total_mse: 4.01487e-07