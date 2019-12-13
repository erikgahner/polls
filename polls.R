library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

polls <- read.csv("polls.csv")

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year, month, day, sep="-")), by = "days"))
  )

for(i in c("a", "b", "c", "d", "e", "f", "i", "k", "o", "p", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls_10m <- polls[polls$date > seq(as.Date(Sys.Date()), length = 2, by = "-10 months")[2],]
polls_100 <- polls %>%
  arrange(desc(as.Date(date))) %>%
  top_n(100, as.Date(date))

plot_party <- function(x, y){
  ggplot(polls_100, aes_string(x="as.Date(date)", y=x)) + 
    geom_smooth(se=FALSE, method="loess", colour=y) +
    geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5) +
    ylab("Stemmer (%)") +
    labs(x = NULL,
         colour = NULL,
         shape = NULL) +
    scale_colour_brewer(palette="Paired") +
    scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
    {if(with(polls_100, min(get(x), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
    theme_minimal(base_size = 12, base_family = "Helvetica") %+replace% 
    theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
          panel.grid.minor.y = element_line(colour = "grey20", size = 0.2, linetype="dotted"),
          axis.ticks.x = element_line(colour = "gray48"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line()
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
plot_party("party_p", "#000000")
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
polls_100 %>%
  gather(party, support, party_a:party_aa) %>%
  ggplot(aes(x=as.Date(date), y=support, colour=party)) +
  geom_point(size=1, alpha=0.3) +
  geom_smooth(se=FALSE, method="loess") +
  geom_hline(yintercept=2, linetype = "dashed") +
  labs(y = "Stemmer (%)",
       x = NULL) +
  scale_colour_manual(labels = c("Socialdemokraterne", "Alternativet", "Radikale Venstre", "Konservative", "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance", "Kristendemokraterne", "Dansk Folkeparti", "Enhedslisten", "Stram Kurs", "Venstre"), 
                      values = c("#E3515D", "#AEFEAF", "#EB4295", "#429969", "#05454F", "#537D7A", "#9C1D2A", "#EE9A5F", "#F4CE97", "#3D6F8D", "#914A4F", "#000000", "#459BC8"),
                      guide = guide_legend(ncol = 4)) +
  theme_minimal(base_size = 12, base_family = "Helvetica") %+replace% 
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
        panel.grid.minor.y = element_line(colour = "grey20", size = 0.2, linetype="dotted"),
        axis.ticks.x = element_line(colour = "gray48"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(),
        legend.position = "bottom", legend.title = element_blank()
  )
dev.off()