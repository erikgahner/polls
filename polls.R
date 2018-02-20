library("ggplot2")
library("rio")

polls <- read.csv("polls.csv")

# Convert
## Stata
convert("polls.csv", "polls.dta")
## SPSS
convert("polls.csv", "polls.sav")
## Excel
convert("polls.csv", "polls.xlsx")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


for(i in c("a", "b", "c", "d", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls.10m <- polls[polls$date > seq(as.Date(Sys.Date()), length = 2, by = "-10 months")[2],]

theme_polls <- function () { # Build on: https://medium.com/@henry.partridge/developing-a-data-visualisation-style-cd24f88fa59
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 14, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", hjust = 1, margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain", hjust = 1),
      axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 12, face = "plain")
    )
}

png('figs/support-a.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_a)) + 
  geom_smooth(se=FALSE, method="loess", colour="#E3515D") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-b.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_b)) + 
  geom_smooth(se=FALSE, method="loess", colour="#EB4295") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-c.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_c)) + 
  geom_smooth(se=FALSE, method="loess", colour="#429969") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-d.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_d)) + 
  geom_smooth(se=FALSE, method="loess", colour="#05454F") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-f.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_f)) + 
  geom_smooth(se=FALSE, method="loess", colour="#9C1D2A") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-i.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_i)) + 
  geom_smooth(se=FALSE, method="loess", colour="#EE9A5F") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-k.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_k)) + 
  geom_smooth(se=FALSE, method="loess", colour="#F4CE97") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-o.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_o)) + 
  geom_smooth(se=FALSE, method="loess", colour="#3D6F8D") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-v.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_v)) + 
  geom_smooth(se=FALSE, method="loess", colour="#459BC8") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-oe.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_oe)) + 
  geom_smooth(se=FALSE, method="loess", colour="#914A4F") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()

png('figs/support-aa.png', width = 642, height = 367.7, units = "px", res = 150)
ggplot(polls.10m, aes(x=as.Date(date), y=party_aa)) + 
  geom_smooth(se=FALSE, method="loess", colour="#AEFEAF") +
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  theme_polls()
dev.off()