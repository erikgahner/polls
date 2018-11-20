library("tidyverse")

polls <- read_csv("polls.csv")

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year,month, day, sep="-")), by = "days"))
  )

for(i in c("a", "b", "c", "d", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls.10m <- polls[polls$date > seq(as.Date(Sys.Date()), length = 2, by = "-10 months")[2],]
polls.100 <- polls[order(as.Date(polls$date)),] %>% top_n(100, as.Date(polls$date))

plot_party <- function(x, y){
  ggplot(polls.100, aes_string(x="as.Date(date)", y=x)) + 
    geom_smooth(se=FALSE, method="loess", colour=y) +
    geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
    ylab("Stemmer (%)") +
    xlab("") +
    scale_colour_brewer(palette="Paired") +
    scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
}

png('figs/support-a.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_a", "#E3515D")
dev.off()

png('figs/support-b.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_b", "#EB4295")
dev.off()

png('figs/support-c.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_c", "#429969")
dev.off()

png('figs/support-d.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_d", "#05454F")
dev.off()

png('figs/support-f.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_f", "#9C1D2A")
dev.off()

png('figs/support-i.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_i", "#EE9A5F")
dev.off()

png('figs/support-k.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_k", "#F4CE97")
dev.off()

png('figs/support-o.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_o", "#3D6F8D")
dev.off()

png('figs/support-v.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_v", "#459BC8")
dev.off()

png('figs/support-oe.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_oe", "#914A4F")
dev.off()

png('figs/support-aa.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_aa", "#AEFEAF")
dev.off()
