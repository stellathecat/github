require(xts)
test <- xts(sample_matrix, as.Date(1:180))
head(test)
require(ggplot2)

ggplot(test, aes(x = Index, y = Open)) + geom_area() + xlab('') + ylab('')
autoplot(test, facet = NULL) + xlab('') + ylab('') + theme(legend.title = element_blank(), legend.position="right")
autoplot(test, facet = NULL) + xlab('') + ylab('') + theme(legend.title = element_blank(), legend.position="top")

require(broom)
require(tidyverse)
a <- tidy(test) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()
b <- tidy(test) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()

require(gridExtra)
grid.arrange(a, b, nrow=2)

ggplot(test, aes(x = Index, y = Open)) + geom_area() + xlab('') + ylab('')
# yaxis2 in plotly
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
ggplot(test, aes(x = Index, y = Open)) + geom_line() + 
  scale_y_continuous(
    "mpg (US)", 
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  )
# https://rpubs.com/MarkusLoew/226759
# https://stackoverflow.com/questions/49185583/two-y-axes-with-different-scales-for-two-datasets-in-ggplot2 # 
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales

d1 = data.frame(x=c(100, 200, 300, 400), y=seq(0.1, 0.4, by=0.1)) # 1st dataset
d2 = data.frame(x=c(100, 200, 300, 400), y=seq(0.8, 0.5, by=-0.1)) # 2nd dataset
d1$z <- "data1"
d2$z <- "data2"
d3 <- within(d2, { y = y/2 })
d4 <- rbind(d1, d3)
d4
test[,1] <- test[,1]*2
c <- tidy(test[,c(1,4)])
ggplot(c, aes(x=index,y=value, color=series)) + geom_line()
ggplot(c, aes(x=index,y=value, color=series)) + geom_line() + scale_y_continuous(name="Close", sec.axis = sec_axis(~ 0.5*., name="Open"))
ggplot(c, aes(x=index,y=value, color=series)) + geom_line() + scale_y_continuous(name="Close", sec.axis = sec_axis(~ 0.5*.+100, name="Open"))
# make transformation such that same starting value
