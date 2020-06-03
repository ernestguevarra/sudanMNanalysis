## Libraries
library(ggplot2)

## load data
load("indicators.Rdata")

## Plotting theme
themeSettings <- theme_bw() +
  theme(panel.border = element_rect(colour = "gray50",
                                    size = 0.5),
        panel.grid.major = element_line(linetype = 1,
                                        size = 0.1,
                                        colour = "gray90"),
        panel.grid.minor = element_line(linetype = 0),
        strip.background = element_rect(colour = "gray50",
                                        fill = "gray70"),
        strip.text = element_text(colour = "white", size = 16),
        legend.text = element_text(size = 16),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_line(colour = "gray50", size = 0.5))

##
x <- merge(unique(locNames[ , c("stateID", "state")]), indicators, all.y = TRUE)

y <- x %>%
  mutate(indicatorGroup = ifelse(ageGrp == 1, "Child", 
                                 ifelse(pregnant == 1, "Pregnant",
                                        ifelse(pregnant == 2 & lactating == 1, "Lactating",
                                          ifelse(pregnant == 2 & lactating == 2, "Non-pregnant non-lactating", NA)))))


x %>%
  mutate(indicatorGroup = ifelse(ageGrp == 1, "Child", 
                            ifelse(pregnant == 1, "Pregnant",
                              ifelse(pregnant == 2 & lactating == 1, "Lactating", NA)))) %>%
  filter(!is.na(indicatorGroup)) %>%
  ggplot(mapping = aes(x = indicatorGroup)) +
  geom_bar(colour = "gray50", fill = "gray70") +
  labs(x = "", y = "n") +
  facet_wrap( ~ state, ncol = 6) +
  coord_flip() +
  themeSettings +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10))

ggsave(filename = "figures/nSample.png", device = "png", width = 12, height = 8, units = "in", dpi = 300)

## Violin plot - Hb

hbData <- indicators[ , c("stateID", "hb", "ageGrp", "pregnant", "lactating")]
hbData <- merge(hbData, unique(locNames[ , c("stateID", "state")]), all.x = TRUE)

hbData %>%
  mutate(indicatorGroup = ifelse(ageGrp == 1, "Child", 
                            ifelse(pregnant == 1, "Pregnant",
                              ifelse(pregnant == 2 & lactating == 1, "Lactating", NA)))) %>%
  #filter(!is.na(indicatorGroup)) %>%
  filter(indicatorGroup == "Child") %>%
  mutate(sort(state)) %>%
  ggplot(mapping = aes(x = state, y = hb)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.2) +
  labs(x = "", y = "Serum Hb concentration (g/dL)") +
  #facet_wrap(indicatorGroup ~ ., nrow = 3) +
  coord_flip() +
  themeSettings

ggsave(filename = "figures/childHb.png", device = "png", width = 8, height = 12, units = "in", dpi = 300)

