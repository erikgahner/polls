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


# Calcuate 95% confidence intervals
for(i in c("a", "b", "c", "d", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

# Remove polls from before September 15, 2011
polls <- polls[polls$date > as.Date("2011-09-15"),]

# Plot polls for Venstre
ggplot(polls, aes(x=as.Date(date), y=party_v)) + 
  geom_smooth(colour="blue", method="loess", se=FALSE) +
  geom_point(size=1, alpha=0.5, colour="blue") + 
  ggtitle("Venstres opbakning i meningsmålingerne") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()

