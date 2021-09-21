#This script corresponds to the following article
#Anion exchange capacity explains deep soil nitrate accumulation in Brazilian Amazon croplands
#Alexandra Huddell, Christopher Neill, Cheryl A. Palm, Darlisson Nunes, and Duncan N. L. Menge


#AEC calculations
setwd('') #set to the appropriate working directory containing the dat file
dat <- read.csv('AEC_soil_nutrients_for_github.csv')

library(ggplot2)
library(dplyr)
library(patchwork)

#translating depth variable to factor
dat$depth_num <- as.factor(dat$depth_num)

# plots -------------------------------------------------------------------

#theme for plots
theme_default <- function(axis_text_size = 11) {
  theme(
    text = element_text(size = 16, colour = "black"),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = axis_text_size, colour = "black"),
    axis.text.y = element_text(size = axis_text_size, colour = "black"),
    axis.title.y = element_text(angle = 90, vjust = 2),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.position = "right",
    legend.key = element_blank()
  )
}

cbPalette <- c("#D55E00", "blue", "#009E73")

dat$treatment <-
  factor(dat$treatment, levels = c("maize", "soy", "forest"))

#calculating median NO3-N
NO3_median <- dat %>%
  group_by(depth_num, treatment) %>%
  summarise(median = median(KCL_ug_NO3_N_g_soil, na.rm = T))



#plotting soil NO3-N concentrations by treatment and depth
p1 <- ggplot() +
  geom_jitter(data = dat,
              aes(
                x = forcats::fct_rev(depth_num),
                y = KCL_ug_NO3_N_g_soil,
                col = treatment
              )) +
  geom_line(data = NO3_median,
            aes(
              x = forcats::fct_rev(depth_num),
              y = median,
              col = treatment,
              group = treatment
            )) +
  geom_point(
    data = NO3_median,
    aes(
      x = forcats::fct_rev(depth_num),
      y = median,
      col = treatment,
      group = treatment
    ),
    shape = 17,
    size = 3
  )  +
  coord_flip() +
  scale_x_discrete(labels = rev(c(
    '0-1', '1-2', '2-3', '3-4', '4-5',
    '5-6', '6-7', '7-8'
  )),
  name = 'soil depth (m)') +
  scale_y_continuous(name = expression(paste('Soil ', NO[3] ^ '-',  ~
                                               '(µg N g ', soil ^ -1, ')')),
                     position = 'right') +
  geom_segment(aes(
    x = 0,
    xend = 8.5,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 8.5,
    xend = 8.5,
    y = 0,
    yend = 50
  ), colour = "black") +
  theme_default() +
  theme(legend.position = "none") +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  )
p1


NH4_median <- dat %>%
  group_by(depth_num, treatment) %>%
  summarise(median = median(KCL_ug_NH4_N_g_soil, na.rm = T))

#plot of soil NH4-N in KCl
p2 <-   ggplot() +
  geom_jitter(
    data = dat,
    aes(
      x = forcats::fct_rev(depth_num),
      y = KCL_ug_NH4_N_g_soil,
      col = treatment
    ),
    alpha = 0.7,
    height = 0.1,
    width = 0.1
  ) +
  geom_line(data = NH4_median,
            aes(
              x = forcats::fct_rev(depth_num),
              y = median,
              col = treatment,
              group = treatment
            )) +
  geom_point(
    data = NH4_median,
    aes(
      x = forcats::fct_rev(depth_num),
      y = median,
      col = treatment,
      group = treatment
    ),
    shape = 17,
    size = 3
  )  +
  
  coord_flip() +
  scale_x_discrete(labels = rev(c(
    '0-1', '1-2', '2-3', '3-4', '4-5',
    '5-6', '6-7', '7-8'
  )),
  name = 'soil depth (m)') +
  scale_y_continuous(name = expression(paste('Soil ', NH[4] ^ '+',  ~
                                               '(µg N g ', soil ^ -1, ')')),
                     position = 'right') +
  geom_segment(aes(
    x = 0,
    xend = 8.5,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 8.5,
    xend = 8.5,
    y = 0,
    yend = 8
  ), colour = "black") +
  theme_default() +
  theme(legend.position = "bottom", legend.text = element_text(size = 16)) +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  )
p2



# soil N concentration to kg N/ha m soil ----------------------------------

#multiplying soil N concentrations by bulk density values and converting to
#kg N per m of soil
soil_N_to_kg_N_m_soil_ha <- function(soil_N, bd) {
  soil_N * bd * 10
}
dat$kg_N_m_soil <-
  soil_N_to_kg_N_m_soil_ha(soil_N = dat$KCL_ug_NO3_N_g_soil,
                           bd = dat$bulk_density)

#summing total NO3-N in top 8m by depth and treatment
N_summed <- dat %>%
  group_by(treatment, site) %>%
  summarize(kg_N_soil_total = sum(kg_N_m_soil))
N_summed

write.csv(N_summed, file = "Nitrate summed by site top 8m.csv")

N_summed$treatment <-
  factor(N_summed$treatment, levels = c("forest", "soy", "maize"))

#plot of total soil NO3-N in top 8m
p3 <- ggplot(N_summed, aes(
  x = treatment,
  y = log10(kg_N_soil_total) ,
  col = treatment
)) +
  geom_point(
    alpha = .6,
    position = position_jitterdodge(jitter.width = .4),
    size = 5
  ) +
  scale_x_discrete(name = " ", labels = (c('forest', 'soybean', 'soybean-\nmaize'))) +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = 1,
    yend = 3.4
  ), colour = "black") +
  geom_segment(aes(
    x = 0,
    xend = 3,
    y = 1,
    yend = 1
  ), colour = "black") +
  theme_default() +
  theme(legend.position = "none",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 18
        )) +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  ) +
  annotate(
    "text",
    x = c(1, 2, 3),
    y = c(3.8, 3.8, 3.8),
    label = c("a", "b", "b"),
    size = 5 ,
    fontface = "bold"
  ) +
  scale_y_continuous(
    breaks = c(1, 2, 2.69897, 3, 3.30103),
    labels = c(10, 100, 500, 1000, 2000),
    name = expression(paste(
      'Total 0-8m soil ',  ~ NO[3] ^ '-',  ~ '(kg N',  ~
        ha ^ -1, ')'
    ))
  )

p3


#arrange plots on grid
fig2 <-
  p1 + p2 + p3 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'a')
fig2
ggsave(
  'fig2.jpeg',
  plot = fig2,
  device = NULL,
  width = 14,
  height = 7,
  units = 'in',
  path = NULL,
  scale = 1,
  dpi = 300,
  limitsize = TRUE
)

#ANOVA on total soil N in kg N per ha on untransformed data
summary(aov(lm(
  kg_N_soil_total ~ as.factor(treatment), data = N_summed
)))
lm1 <- (lm(kg_N_soil_total ~ as.factor(treatment), data = N_summed))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)

#ANOVA on total soil N in kg N per ha on log10-transformed data
summary(aov(lm(
  log10(kg_N_soil_total) ~ as.factor(treatment), data = N_summed
)))
lm1 <-
  (lm(log10(kg_N_soil_total) ~ as.factor(treatment), data = N_summed))
aov1 <- aov(lm1)
TukeyHSD(aov1)





# analysis of soil C and soil N stocks ------------------------------------
#read in soil nutrient data
dat <- read.csv('AEC_soil_nutrients_for_github.csv')


dat$treatment <-
  factor(dat$treatment, levels = c("maize", "soy", "forest"))
levels(dat$treatment)

dat$depth_num <-
  factor(dat$depth_num, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))

dat$site <- factor(dat$site)

#multiplying soil N % by bulk density values and converting to
#kg N per m of soil per ha
soil_N_to_Mg_N_m_soil_ha <-
  function(soil_pct_N, bd) {
    soil_pct_N * bd * 10 ^ 2
  }
dat$Mg_N_m_soil_ha <-
  soil_N_to_Mg_N_m_soil_ha(soil_pct_N = dat$soil_pct_N,
                           bd = dat$bulk_density)

#soil C per ha
soil_C_to_Mg_N_m_soil_ha <-
  function(soil_pct_C, bd) {
    soil_pct_C * bd * 10 ^ 2
  }
dat$Mg_C_m_soil_ha <-
  soil_C_to_Mg_N_m_soil_ha(soil_pct_C = dat$soil_pct_C,
                           bd = dat$bulk_density)

#testing for notmality
shapiro.test(dat$Mg_N_m_soil_ha)

#log transformed response variable is more appropriate for lognormal data
#running anovas on log-transformed data
lm1 <-
  (lm(log10(Mg_N_m_soil_ha) ~ as.factor(treatment) + depth_num, data = dat))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)

#calculating median soil-N
soil_N_median <- dat %>%
  group_by(depth_num, treatment) %>%
  summarise(median = median(soil_pct_N * 10, na.rm = T))


#subsetting data at each depth
dat1 <- dat[dat$depth_num == '1', ]
dat2 <- dat[dat$depth_num == '2', ]
dat3 <- dat[dat$depth_num == '3', ]
dat4 <- dat[dat$depth_num == '4', ]
dat5 <- dat[dat$depth_num == '5', ]
dat6 <- dat[dat$depth_num == '6', ]
dat7 <- dat[dat$depth_num == '7', ]
dat8 <- dat[dat$depth_num == '8', ]

dat1_narm <- na.omit(dat1)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat1_narm
)))

dat2_narm <- na.omit(dat2)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat2_narm
)))

dat3_narm <- na.omit(dat3)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat3_narm
)))

dat4_narm <- na.omit(dat4)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat4_narm
)))

dat5_narm <- na.omit(dat5)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat5_narm
)))

dat6_narm <- na.omit(dat6)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat6_narm
)))

dat7_narm <- na.omit(dat7)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat7_narm
)))

dat8_narm <- na.omit(dat8)
summary(aov(lm(
  log10(Mg_N_m_soil_ha) ~ as.factor(treatment), data = dat8_narm
)))



#plotting soil N concentrations by treatment and depth
p4 <- ggplot() +
  geom_jitter(data = dat,
              aes(
                x = forcats::fct_rev(depth_num),
                y = soil_pct_N * 10,
                col = treatment
              )) +
  geom_line(data = soil_N_median,
            aes(
              x = forcats::fct_rev(depth_num),
              y = median,
              col = treatment,
              group = treatment
            )) +
  geom_point(
    data = soil_N_median,
    aes(
      x = forcats::fct_rev(depth_num),
      y = median,
      col = treatment,
      group = treatment
    ),
    shape = 17,
    size = 3
  )  +
  coord_flip() +
  scale_x_discrete(labels = rev(c(
    '0-1', '1-2', '2-3', '3-4', '4-5',
    '5-6', '6-7', '7-8'
  )),
  name = 'soil depth (m)') +
  scale_y_continuous(name = expression(paste('Soil total nitrogen (mg N g ', soil ^
                                               -1, ')')),
                     position = 'right') +
  geom_segment(aes(
    x = 0,
    xend = 8.5,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 8.5,
    xend = 8.5,
    y = 0,
    yend = 1
  ), colour = "black") +
  theme_default() +
  theme(legend.position = "none") +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  )
p4

#testing soil C for normality
shapiro.test(dat$Mg_C_m_soil_ha)

#log transformed data is more appropriate for lognormal data
#running anovas on log-transformed data
lm1 <-
  (lm(log10(Mg_C_m_soil_ha) ~ as.factor(treatment) + depth_num, data = dat))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)

#testing for treatment differences within each depth
dat1_narm <- na.omit(dat1)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat1_narm
)))

dat2_narm <- na.omit(dat2)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat2_narm
)))

dat3_narm <- na.omit(dat3)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat3_narm
)))

dat4_narm <- na.omit(dat4)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat4_narm
)))

dat5_narm <- na.omit(dat5)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat5_narm
)))

dat6_narm <- na.omit(dat6)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat6_narm
)))

dat7_narm <- na.omit(dat7)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat7_narm
)))

dat8_narm <- na.omit(dat8)
summary(aov(lm(
  log10(Mg_C_m_soil_ha) ~ as.factor(treatment), data = dat8_narm
)))

#calculating median soil-C
soil_C_median <- dat %>%
  group_by(depth_num, treatment) %>%
  summarise(median = median(soil_pct_C * 10, na.rm = T))

#plotting soil C concentrations by treatment and depth
p5 <- ggplot() +
  geom_jitter(data = dat,
              aes(
                x = forcats::fct_rev(depth_num),
                y = soil_pct_C * 10,
                col = treatment
              )) +
  geom_line(data = soil_C_median,
            aes(
              x = forcats::fct_rev(depth_num),
              y = median,
              col = treatment,
              group = treatment
            )) +
  geom_point(
    data = soil_C_median,
    aes(
      x = forcats::fct_rev(depth_num),
      y = median,
      col = treatment,
      group = treatment
    ),
    shape = 17,
    size = 3
  )  +
  coord_flip() +
  scale_x_discrete(labels = rev(c(
    '0-1', '1-2', '2-3', '3-4', '4-5',
    '5-6', '6-7', '7-8'
  )),
  name = 'soil depth (m)') +
  scale_y_continuous(name = expression(paste('Soil carbon (mg C g ', soil ^
                                               -1, ')')),
                     position = 'right') +
  geom_segment(aes(
    x = 0,
    xend = 8.5,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 8.5,
    xend = 8.5,
    y = 0,
    yend = 15
  ), colour = "black") +
  theme_default() +
  theme(legend.position = "right") +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  )
p5


#total N sum from 0-8 m in Mg N/ha by treatment and site
TN_soil_sum <- dat %>%
  group_by(treatment, site) %>%
  summarize(Mg_N_ha = sum(Mg_N_m_soil_ha))
TN_soil_sum

#median total N by treatment
TN_soil_sum %>% group_by(treatment) %>% summarize(median(Mg_N_ha))

#checking the distribution of summed 0-8 m soil N
hist(TN_soil_sum$Mg_N_ha)
shapiro.test(TN_soil_sum$Mg_N_ha)
#it's normally distributed

#ANOVA
lm1 <- (lm(Mg_N_ha ~ as.factor(treatment), data = TN_soil_sum))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)


p6 <- ggplot(TN_soil_sum, aes(x = treatment, y = Mg_N_ha, col = treatment)) +
  geom_point() +
  scale_y_continuous(name = expression(paste('Total 0-8 m soil N (Mg N ',  ~
                                               ha ^ -1, ')'))) +
  scale_x_discrete(name = " ", labels = (c('forest', 'soybean', 'soybean-\nmaize'))) +
  
  theme(legend.position = "right") +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  ) +
  theme_default() +
  geom_segment(aes(
    x = 0,
    xend = 4,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = 0,
    yend = 25
  ), colour = "black")

p6

#total C
TC_soil_sum <- dat %>%
  group_by(treatment, site) %>%
  summarize(Mg_C_ha = sum(Mg_C_m_soil_ha))
TC_soil_sum

#checking the distribution of summed 0-8 m soil C
hist(TC_soil_sum$Mg_C_ha)
shapiro.test(TC_soil_sum$Mg_C_ha)
#it's normally distributed

#ANOVA
lm1 <- (lm(Mg_C_ha ~ as.factor(treatment), data = TC_soil_sum))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)


#reordering treatment levels
TC_soil_sum$treatment <-
  factor(TC_soil_sum$treatment, levels = c("forest", "soy", "maize"))

p7 <- ggplot(TC_soil_sum, aes(x = treatment, y = Mg_C_ha, col = treatment)) +
  geom_point() +
  scale_y_continuous(name = expression(paste('Total 0-8 m soil C (Mg C ',  ~
                                               ha ^ -1, ')'))) +
  theme(legend.position = "right") +
  scale_color_manual(
    name = "Land use",
    values = c(cbPalette[3], cbPalette[1], "blue"),
    breaks = c("forest", "soy", "maize"),
    labels = c("forest",  "soybean", "soybean-maize")
  ) +
  theme_default() +
  scale_x_discrete(name = " ", labels = (c('forest', 'soybean', 'soybean-\nmaize'))) +
  geom_segment(aes(
    x = 0,
    xend = 4,
    y = 0,
    yend = 0
  ), colour = "black") +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = 0,
    yend = 500
  ), colour = "black")

p7

#arrange plots on grid
fig5 <- (p4 + p5 + plot_layout(guides = "collect")) /
  (p6 + p7 + plot_layout(guides = "collect")) +
  plot_annotation(tag_levels = 'a')
fig5

ggsave(
  'fig5.jpeg',
  plot = fig5,
  device = NULL,
  width = 12,
  height = 10,
  units = 'in',
  path = NULL,
  scale = 1,
  dpi = 300,
  limitsize = TRUE
)

