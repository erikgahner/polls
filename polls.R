# Load packages
library(downloader)
library(ggplot2)

# Download file
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# See top rows
head(polls)

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

# Remove polls from before September 15, 2011
polls <- polls[polls$date > as.Date("2011-09-15"),]

# Calcuate standard error for Venstre
polls$se_v <- 1.96 * sqrt( (polls$party_v * (100-polls$party_v) ) / polls$n)  

# Plot polls for Venstre
ggplot(polls, aes(x=as.Date(date), y=party_v)) + 
  geom_errorbar(aes(ymin=party_v-se_v, ymax=party_v+se_v), color="gray90") +
  geom_point(shape=1) + 
  ggtitle("Venstre") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank())