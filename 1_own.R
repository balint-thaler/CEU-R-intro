x <- seq(0, 20, by = 0.1)
plot(x, sin(x), type = "l")
?curve
curve(sin, from = 0, to = 20)
curve(cos, from = 0, to = 20, add = TRUE, col = "red")

## TODO brownian motion / random walk
x <- 0
## TODO 100 iterations
for(i in 1:100) {
  if(runif(1) < 0.5) {
    x <- x + 1  
  } else {
    x <- x - 1
  }
}
x


## vectorize 
runif(100) < 0.5
?ifelse
round(runif(100))
set.seed(42)
cumsum(round(runif(100)*2 - 1))

h <- c(174, 170, 160)
w <- c(90, 80, 70)
min(w)
max(w)
range(w)
diff(range(w))
mean(w)
median(w)
sum(w)
summary(w)
str(summary(w))
cor(w, h)
lm(w ~ h)

## let's see the predicted w for some 165cm
-146.154 + 165*1.346
fit <- lm(w ~ h)
str(fit)
summary(fit)
predict(fit, newdata = list(h = 165))

predict(fit, newdata = list(h = 52))

plot(h, w)
abline(fit, col = "red")

df <- data.frame(weight = w, height = h)
df
str(df)
df$weight
df$weight[1]
df[1,2]
df[2,1]

nrow(df)
ncol(df)
dim(df)


plot(df)
cor(df)

df

df$bmi <- df$weight / (df$height/100)**2
df

df <- read.csv("http://bit.ly/CEU-R-heights")
df
summary(df)
str(df)

df$bmi <- (df$weightLb*0.4536)/((df$heightIn*2.54/100)^2)
df$weight <- df$weightLb*0.45
df$height <- df$heightIn*2.54
df$heightIn <- df$heightIn <- NULL
df$bmi <- df$weight / (df$height/100)**2
df

plot(df)

install.packages("pairsD3")
library(pairsD3)
pairsD3::pairsD3(df)

install.packages("GGally")
library(GGally)
df$weightLb <- NULL
ggpairs(df)


library(ggplot2)
ggplot(df, aes(x = height)) + geom_histogram()
ggplot(df, aes(x = height, y = weight)) + geom_point()
ggplot(df, aes(x = height, y = weight, col=sex)) + geom_point()
system.time(g <- ggplot(df, aes(x = height, y = weight, col=sex)) + geom_point())
system.time(print(g))

g + theme_bw()
g + theme_void()
g + geom_smooth()

g + geom_smooth(method = "lm")
g + geom_smooth(method = "lm", se = FALSE)

ggplot(df, aes(x = height, y = weight)) + 
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(aes(color = sex), method = "lm", se = FALSE) +
  theme_bw()

g + scale_y_log10()

ggplot(df, aes(x=height)) + geom_boxplot()
ggplot(df, aes(sex, height)) + geom_boxplot()
ggplot(df, aes(sex, height)) + 
  geom_boxplot() + 
  geom_violin(alpha = 0.5) + 
  geom_jitter()

ggplot(df, aes(x = height)) + geom_density()
ggplot(df, aes(x = height, fill = sex)) + geom_density()
ggplot(df, aes(x = height, fill = sex)) + 
  geom_density(alpha = 0.25) +
  theme_bw() +
  ggtitle("Height of boys and girls") +
  xlab("Height (cm)") + ylab("") +
  theme(legend.position = "top")


## TODO bar chart on nr of f/m
ggplot(df, aes(x = sex)) + geom_bar()

## TODO histogram of weight
ggplot(df, aes(weight)) + geom_histogram()

## TODO histogram of weight by gender
ggplot(df, aes(x = weight)) + 
       geom_histogram(position = "identity") +
  facet_wrap(~sex) + 
  theme_bw()
## TODO bar chart on the number of f/m above and below 160cm
df$height_cat <- cut(df$height, breaks = c(0, 160, Inf))
ggplot(df, aes(sex)) + geom_bar() + facet_wrap(~height_cat)

ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = "dodge")
ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = "fill")
