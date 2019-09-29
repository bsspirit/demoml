
# 1 look at data ----------------------------------------------------------
library(tidyverse)
library(lubridate)

data(iris)

# variable names & class
df <- iris
names(df) <- gsub('\\.','_',names(df))
df$Species <- as.factor(ifelse(iris$Species == 'versicolor',1,0))

# structure
str(df)

# simple statistics
summary(df)

# distribution
ggplot() +           # histogram
  geom_histogram(data = df, aes(Sepal_Width), bins = 10)

ggplot() +           # scatter plot
  geom_point(data = df, aes(Sepal_Length, Sepal_Width))

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(df[,1:4], upper.panel = panel.cor, lower.panel = panel.smooth)

table(df$Species)
ggplot() +           # box plot
  geom_boxplot(data = df, aes(Species, Sepal_Length, color=Species))

ggplot() +           # density plot
  geom_density(data = df, aes(Sepal_Length, fill=Species)) +
  facet_wrap(~Species, ncol = 1)



# 2 preprocessing ---------------------------------------------------------

############################################
# discretizing
quantile(df$Sepal_Width)

library(smbinning)

df$Species <- as.numeric(df$Species) - 1
result <- smbinning(df, 'Species', 'Sepal_Width')
result$cuts
###设置2*2的绘图样式###
par(mfrow=c(2,2))
###在第一幅图中绘制在fgood的条件下的cbs1的箱图###
boxplot(df$Sepal_Width ~ df$Species,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")
mtext("Credit Score",3)
###第二幅图中绘制分箱后的分布图###
smbinning.plot(result,option="dist",sub="Credit Score")
###第三幅图中绘制分箱后的坏样本率###
smbinning.plot(result,option="badrate",sub="Credit Score")
###第四幅图中绘制分箱后WOE值###
smbinning.plot(result,option="WoE",sub="Credit Score")
par(mfrow=c(1,1))

df$Sepal_Width_level <- cut(df$Sepal_Width,
                            breaks = c(0,result$cuts,max(df$Sepal_Width)),
                            labels = c('1','2','3'))

########################################
# standardization
min_max <- function(x) {(x-min(x)) / (max(x)-min(x))}
apply(df[,1:4], 2, min_max)
apply(df[,1:4], 2, scale)
