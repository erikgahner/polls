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

# Calcuate 95% confidence intervals for Venstre
polls$ci_v <- 1.96 * sqrt( (polls$party_v * (100-polls$party_v) ) / polls$n)  

# Plot polls for Venstre
ggplot(polls, aes(x=as.Date(date), y=party_v)) + 
  geom_smooth(colour="blue") +
  geom_point(size=.5, colour="blue") + 
  geom_ribbon(aes(ymin=party_v-ci_v, ymax=party_v+ci_v), fill="blue", alpha=0.3) +
  ggtitle("Venstres opbakning i meningsmålingerne") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()

