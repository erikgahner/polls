library("tidyverse")
library("lubridate")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

polls <- read.csv("polls.csv")

polls <- polls %>% 
  mutate(date = make_date(year, month, day),
         across(starts_with("party"), ~ .x + 1.96 * sqrt((.x * (100 - .x)) / n), .names = "ci_max_{.col}"),
         across(starts_with("party"), ~ .x - 1.96 * sqrt((.x * (100 - .x)) / n), .names = "ci_min_{.col}")
         )

#polls_use <- polls[polls$date > seq(as.Date(Sys.Date()), length = 2, by = "-12 months")[2],]
polls_use <- polls %>% 
  arrange(desc(as.Date(date))) %>% 
  top_n(75, as.Date(date)) %>% 
  mutate_at(vars(starts_with("ci_min_party")), ~ ifelse(.x < 0, 0.001, .x))

plot_party <- function(x, parti){
  ggplot(polls_use, aes_string(x="as.Date(date)", y=paste0("party_", x))) + 
    geom_smooth(se = FALSE, colour = "gray70", span = .3, size = 1) +
    geom_errorbar(aes_string(colour = "pollingfirm", ymin = paste0("ci_min_party_", x), ymax = paste0("ci_max_party_", x)), alpha = .4) +
    geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
    scale_colour_manual(breaks = c("Voxmeter", "Gallup", "YouGov", "Epinion", "Megafon", "Greens", "Norstat"),
                        values = c("#C74B4B", "#5C8F4A", "#DD7E3A", "#456491", "#183B66", rep("black", 2))) +
    scale_shape_manual(breaks = c("Voxmeter", "Gallup", "YouGov", "Epinion", "Megafon", "Greens", "Norstat"),
                       values = c(16, 15, 17, 18, 4, 3, 2)) +
    labs(y = NULL, x = NULL, colour = NULL, shape = NULL,
         caption = paste0("Opbakning til ", parti, " (%)\n", 
                          "m. 95% konfidensintervaller\n",
                          NROW(polls_use[!is.na(polls_use[,paste0("party_", x)]),]),
                          " meningsmålinger\n", tolower(format(min(as.Date(polls_use$date)), "%B %Y")), "-", tolower(format(max(as.Date(polls_use$date)), "%B %Y")))
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    {if(with(polls_use, min(get(paste0("party_", x)), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
    ylim(c(
      ifelse(
        (with(
          polls_use, 
          3*min(get(paste0("party_", x)), na.rm=TRUE)) - with(polls_use, max(get(paste0("ci_max_party_", x)), na.rm=TRUE)))/2 <= 0 | with(polls_use, max(get(paste0("party_", x)))) < 1, 
        0, (with(polls_use, 3*min(get(paste0("party_", x)), na.rm=TRUE)) - with(polls_use, max(get(paste0("ci_max_party_", x)), na.rm=TRUE)))/2), 
      with(polls_use, max(get(paste0("ci_max_party_", x)), na.rm=TRUE)) + 0.2)) +
    theme_minimal(base_size = 12, base_family = "Barlow") %+replace% 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor.y = element_blank(),
          plot.caption = element_text(hjust = 1, size = 10, margin = margin(t = -57), lineheight = 1.2),
          legend.justification = c(0, 0),
          legend.position = "bottom",
          plot.margin=unit(c(.5, .5, 1.5, .5),"cm"),
          axis.ticks.x = element_line(colour = "gray48"),
          axis.ticks.y = element_blank()
    ) +
    guides(col = guide_legend(ncol = 2), fill = FALSE)
}

png('figs/support-a.png', width = 800, height = 700, units = "px", res = 135)
plot_party(x = "a", parti = "Socialdemokratiet")
dev.off()

png('figs/support-b.png', width = 800, height = 700, units = "px", res = 135)
plot_party("b", "Radikale Venstre")
dev.off()

png('figs/support-c.png', width = 800, height = 700, units = "px", res = 135)
plot_party("c", "Konservative")
dev.off()

png('figs/support-d.png', width = 800, height = 700, units = "px", res = 135)
plot_party("d", "Nye Borgerlige")
dev.off()

png('figs/support-f.png', width = 800, height = 700, units = "px", res = 135)
plot_party("f", "SF")
dev.off()

png('figs/support-g.png', width = 800, height = 700, units = "px", res = 135)
plot_party("g", "Veganerpartiet")
dev.off()

png('figs/support-i.png', width = 800, height = 700, units = "px", res = 135)
plot_party("i", "Liberal Alliance")
dev.off()

png('figs/support-k.png', width = 800, height = 700, units = "px", res = 135)
plot_party("k", "Kristendemokraterne")
dev.off()

png('figs/support-o.png', width = 800, height = 700, units = "px", res = 135)
plot_party("o", "Dansk Folkeparti")
dev.off()

png('figs/support-v.png', width = 800, height = 700, units = "px", res = 135)
plot_party("v", "Venstre")
dev.off()

png('figs/support-oe.png', width = 800, height = 700, units = "px", res = 135)
plot_party("oe", "Enhedslisten")
dev.off()

png('figs/support-aa.png', width = 800, height = 700, units = "px", res = 135)
plot_party("aa", "Alternativet")
dev.off()

png('figs/support-all.png', width = 800, height = 700, units = "px", res = 115)
polls_use %>%
  gather(party, support, party_a:party_aa) %>%
  filter(!party %in% c("party_e", "party_p")) %>% 
  ggplot(aes(x=as.Date(date), y=support, colour=party)) +
  geom_point(size=1, alpha=0.3) +
  geom_hline(yintercept = 0) +
  geom_smooth(se=FALSE, method="loess", span = .3) +
  geom_hline(yintercept=2, linetype = "dashed") +
  labs(y = "Stemmer (%)",
       x = NULL) +
  scale_colour_manual(labels = c("Socialdemokraterne", "Alternativet", "Radikale Venstre", "Konservative", "Nye Borgerlige", "SF", "Veganerpartiet", 
                                 "Liberal Alliance", "Kristendemokraterne", "Dansk Folkeparti", "Enhedslisten", "Venstre"), 
                      values = c("#E3515D", "#AEFEAF", "#EB4295", "#429969", "#05454F", "#9C1D2A", "green",
                                 "#EE9A5F", "#F4CE97", "#3D6F8D", "#914A4F", "#459BC8"),
                      guide = guide_legend(ncol = 4)) +
  theme_minimal(base_size = 12, base_family = "Barlow") %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 1, size = 10, margin = margin(t = -71), lineheight = 1.2),
        legend.justification = c(0, 0),
        legend.position = "bottom",
        plot.margin=unit(c(.5, .5, 1.5, .5),"cm"),
        axis.ticks.x = element_line(colour = "gray48"),
        axis.ticks.y = element_blank(),
        legend.title = element_blank()
  )
dev.off()