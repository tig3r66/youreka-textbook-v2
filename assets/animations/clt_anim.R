library(ggplot2) 
library(readr)
library(animation)
library(gridExtra)
library(gapminder)

set.seed(0)

dt <- as.data.frame(gapminder$lifeExp)
m_dt <- round(mean(gapminder$lifeExp), 2)

sum <- data.frame(seq(0, 100, 0.01))
sum$count <- 0
colnames(sum) <- c('Mean', 'Frequency')
i <- 1

no <- 10000
plots <- seq(0, no, 100)

saveGIF(
  while (i <= no) {

    s <- sample(gapminder$lifeExp, 30, replace=T)
    m <- round(mean(s), 2)
    sum$Frequency[sum$Mean == m] <- sum$Frequency[sum$Mean == m] + 1

    if (i %in% plots) {

      plot1 <- ggplot(data=dt, aes(gapminder$lifeExp)) +
        geom_histogram(bins = 30, colour="black", fill="white") +
        ggtitle("Histogram of Life Expectancies") +
        geom_vline(xintercept = s, colour="red", linetype = "longdash", size = 0.35) +
        labs(x="Life expectancy (years)", y="Count") +
        theme_bw() +
        geom_blank() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
      plot2 <- ggplot(data=sum, aes(Mean, Frequency)) +
        geom_point(size=0.5) +
        geom_path() +
        scale_y_continuous(limits = c(0, 30)) +
        scale_x_continuous(limits = c(40, 80)) +
        ggtitle("Sample Means of Life Expectancies") +
        annotate("text", x = 47.5, y = 27.5, label = paste("No. of samples: ", i)) +
        annotate("text", x = 73, y = 27.5, label = "Sample size: 30") +
        annotate("text", x = 73, y = 26, label = paste("Population mean: ", m_dt)) +
        labs(x="Mean life expectancy (years)", y="Count") + 
        theme_bw() +
        geom_blank() +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
      grid.arrange(plot1, plot2, ncol=2)
    }
    i <- i+1
  },
movie.name = "gap_clt.gif", interval = 0.01, convert = "convert", ani.width = 1000, 
ani.height = 500)
