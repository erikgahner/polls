library("tidyverse")

polls <- read.csv("polls.csv")

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year,month, day, sep="-")), by = "days"))
  )

for(i in c("a", "b", "c", "d", "e", "f", "i", "k", "o", "p", "v", "oe", "aa")) {
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
    {if(with(polls.100, min(get(x), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
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

png('figs/support-e.png', width = 642, height = 400, units = "px", res = 105)
plot_party("party_e", "#537D7A")
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

png('figs/support-p.png', width = 642, height = 400, units = "px", res = 105)
ggplot(polls.100, aes_string(x="as.Date(date)", y="party_p")) + 
  geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
  {if(with(polls.100, min(get("party_d"), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
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

png('figs/support-all.png', width = 700, height = 500, units = "px", res = 105)
polls.100 %>%
  gather(party, support, party_a:party_aa) %>%
  ggplot(aes(x=as.Date(date), y=support, colour=party)) +
  geom_point(size=1, alpha=0.3) +
  geom_smooth(se=FALSE, method="loess") +
  geom_hline(yintercept=2, linetype = "dashed") +
  labs(y = "Stemmer (%)",
       x = "") +
  scale_colour_manual(labels = c("Socialdemokraterne", "Alternativet", "Radikale Venstre", "Konservative", "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance", "Kristendemokraterne", "Dansk Folkeparti", "Enhedslisten", "Stram Kurs", "Venstre"), 
                      values = c("#E3515D", "#AEFEAF", "#EB4295", "#429969", "#05454F", "#537D7A", "#9C1D2A", "#EE9A5F", "#F4CE97", "#3D6F8D", "#914A4F", "#000000", "#459BC8"),
                      guide = guide_legend(ncol = 4)) +
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
    legend.position = "bottom",
    legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
  ) 
dev.off()