#This script corresponds to the following article
#Anion exchange capacity explains deep soil nitrate accumulation in Brazilian Amazon croplands
#Alexandra Huddell, Christopher Neill, Cheryl A. Palm, Darlisson Nunes, and Duncan N. L. Menge


#AEC calculations
setwd('') #set to the appropriate working directory containing the dat file
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmerTest)

dat <-
   read.csv(
      'Tanguro AEC extractions for github.csv',
      header = T,
      stringsAsFactors = F
   )

dat$AEC <-
   (as.numeric(dat$AEC)) #converting the AEC column to numeric
dat$depth_num <-
   as.factor(dat$depth_num) #converting the depth number column to factor
levels(dat$depth_num) #checking levels

#summary stats of the anion exchange capacity measurement
AEC_summary <- dat %>%
   summarize(
      mean_AEC = mean(AEC, na.rm = T),
      median_AEC = median(AEC, na.rm = T),
      min_AEC = min(AEC, na.rm = T),
      max_AEC = max(AEC, na.rm = T),
      sd_AEC = sd(AEC, na.rm = T)
   )
AEC_summary

#summary stats of the pH in deionized water
pH_DI_summary <- dat %>%
   summarize(
      mean_pH_DI = mean(pH_DI, na.rm = T),
      median_pH_DI = median(pH_DI, na.rm = T),
      min_pH_DI = min(pH_DI, na.rm = T),
      max_pH_DI = max(pH_DI, na.rm = T),
      sd_pH_DI = sd(pH_DI, na.rm = T)
   )
pH_DI_summary

#summary stats of the pH in 1M potassium chloride
pH_1M_KCl_summary <- dat %>%
   summarize(
      mean_pH_1M_KCl = mean(ph_1M_KCl, na.rm = T),
      median_pH_1M_KCl = median(ph_1M_KCl, na.rm = T),
      min_pH_1M_KCl = min(ph_1M_KCl, na.rm = T),
      max_pH_1M_KCl = max(ph_1M_KCl, na.rm = T),
      sd_pH_1M_KCl = sd(ph_1M_KCl, na.rm = T)
   )
pH_1M_KCl_summary


# ANOVAs on treatment differences in anion exchange capacity measurements --------

#separating data by each depth
dat1 <- dat[dat$depth_num == '1',]
dat2 <- dat[dat$depth_num == '2',]
dat3 <- dat[dat$depth_num == '3',]
dat4 <- dat[dat$depth_num == '4',]
dat5 <- dat[dat$depth_num == '5',]
dat6 <- dat[dat$depth_num == '6',]
dat7 <- dat[dat$depth_num == '7',]
dat8 <- dat[dat$depth_num == '8',]


#ANOVA across all depths
lm <- (lm(AEC ~ as.factor(treatment) + depth_num, data = dat))
aov <- (aov(lm))
summary(aov)
TukeyHSD(aov)

dat1_narm <- na.omit(dat1)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat1_narm)))

dat2_narm <- na.omit(dat2)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat2_narm)))

#This depth has significant differences
dat3_narm <- na.omit(dat3)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat3_narm)))
lm3 <- (lm(AEC ~ as.factor(treatment), data = dat3_narm))
aov3 <- (aov(lm3))


dat4_narm <- na.omit(dat4)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat4_narm)))

dat5_narm <- na.omit(dat5)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat5_narm)))

dat6_narm <- na.omit(dat6)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat6_narm)))

dat7_narm <- na.omit(dat7)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat7_narm)))

#this depth has signicant differences
dat8_narm <- na.omit(dat8)
summary(aov(lm(AEC ~ as.factor(treatment), data = dat8_narm)))
lm8 <- (lm(AEC ~ as.factor(treatment), data = dat8_narm))
aov8 <- (aov(lm8))


#Tukey Honest Significant Difference Tests for the two depths with
#significant treatment differences
TukeyHSD(aov3) #maize (mean 1.75) is larger than forest mean (0.93)
TukeyHSD(aov8) #maize (mean (1.87) is larger than the forest mean (0.89)

#Means by treatment
dat3_narm %>% group_by(treatment) %>% summarize(mean(AEC))
dat8_narm %>% group_by(treatment) %>% summarize(mean(AEC))

# delta pH calculation and ANOVAS on treatment differences in pH data ---------------------------------------------------------

dat$delta_pH <- dat$ph_1M_KCl - dat$pH_DI
hist(dat$delta_pH)

#########% of observations that fall above -0.5
delta_pH_high <- nrow(filter(dat, delta_pH > -0.5))
delta_pH_low <- nrow(filter(dat, delta_pH <= -0.5))
delta_pH_high / (delta_pH_high + delta_pH_low) * 100
#76% of observations fall above -0.5, indicating that the soils are of variable charge


# ANOVAs on pH in 1M KCl across treatment and within each depth -----------

summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat1_narm)))
lmKCl1 <- (lm(ph_1M_KCl ~ as.factor(treatment), data = dat1_narm))
aovlmKCl1 <- (aov(lmKCl1))
TukeyHSD(aovlmKCl1)

summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat2_narm)))
lmKCl2 <- (lm(ph_1M_KCl ~ as.factor(treatment), data = dat2_narm))
aovlmKCl2 <- (aov(lmKCl2))
TukeyHSD(aovlmKCl2)

summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat3_narm)))
summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat4_narm)))
summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat5_narm)))
summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat6_narm)))
summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat7_narm)))
summary(aov(lm(ph_1M_KCl ~ as.factor(treatment), data = dat8_narm)))


# ANOVAs on pH in deionized water across treatment and within each --------
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat1_narm)))
lmDI1 <- (lm(pH_DI ~ as.factor(treatment), data = dat1_narm))
aovlmDI1 <- (aov(lmDI1))
TukeyHSD(aovlmKCl1)
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat2_narm)))
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat3_narm)))
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat4_narm)))
lmDI4 <- (lm(pH_DI ~ as.factor(treatment), data = dat4_narm))
aovlmDI4 <- (aov(lmDI4))
TukeyHSD(aovlmDI4)
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat5_narm)))
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat6_narm)))
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat7_narm)))
summary(aov(lm(pH_DI ~ as.factor(treatment), data = dat8_narm)))



# translating AEC charge to kg N m soil^-1  ha^-1 --------------------------------
AEC_to_kg_N_m_soil_ha <- function(AEC, bd) {
   AEC * 14.007 * bd * 100
}

dat$kg_N_m_ha_soil <-
   AEC_to_kg_N_m_soil_ha(AEC = dat$AEC, bd = dat$bulk_density)

#estimated max soil N summarized from 0-8m
summary(dat$kg_N_m_ha_soil)

N_max_summed_0_8m <- dat %>%
   group_by(treatment, site) %>%
   summarize(kg_N_soil_total = sum(kg_N_m_ha_soil, na.rm = T))
N_max_summed_0_8m

summary(N_max_summed_0_8m$kg_N_soil_total)
mean(N_max_summed_0_8m$kg_N_soil_total, na.rm = T)

kg_N_m_ha_soil_median <- dat %>%
   group_by(depth_num, treatment) %>%
   summarise(median = median(kg_N_m_ha_soil, na.rm = T))


# compare predicted to measured nutrients based on AEC -------------------

#reading in soil nutrient data
measured_soil_N <- read.csv('AEC_soil_nutrients_for_github.csv')

#creating ID variables to match across the two dataframes
measured_soil_N$ID <-
   paste0(measured_soil_N$site, measured_soil_N$depth_num)
dat$ID <- paste0(dat$site, dat$depth_num)

#selecting relevant variables from dat dataframe
dat <-
   dat %>% select(
      site,
      depth_num,
      treatment,
      predicted_kg_N_m_ha_soil = kg_N_m_ha_soil,
      ID,
      bulk_density,
      years_since_deforested,
      years_soy,
      years_maize
   )

#selecting relevant variables from measured soil N data
measured_soil_N <-
   measured_soil_N %>% select(ID, KCL_ug_NO3_N_g_soil)

#joining two dataframes
joined_dat <- left_join(dat, measured_soil_N, by = "ID")


#convert ug NO3-N/g soil measurements to kg N m soil^-1 ha^-1
joined_dat$measured_kg_N_m_ha <-
   joined_dat$KCL_ug_NO3_N_g_soil * joined_dat$bulk_density * 10


#calculating the "N saturation (%)", (N_saturation_pct)
#based on measured over predicted total N sorption capacity
joined_dat$N_saturation_pct <- (joined_dat$measured_kg_N_m_ha /
                                   joined_dat$predicted_kg_N_m_ha_soil) * 100

#summarizing N saturation percent data
N_saturation_pct_summary <- joined_dat %>%
   summarize(
      mean_N_saturation_pct = mean(N_saturation_pct, na.rm = T),
      median_N_saturation_pct = median(N_saturation_pct, na.rm = T),
      min_N_saturation_pct = min(N_saturation_pct, na.rm = T),
      max_N_saturation_pct = max(N_saturation_pct, na.rm = T),
      sd_N_saturation_pct = sd(N_saturation_pct, na.rm = T),
      n_N_saturation_pct = n()
   )
N_saturation_pct_summary

#doubling the estimate of N saturation % (per our explanation in the manuscript
#that we assume only half of the soil anion exchange sites are available to NO3-
#and perhaps half are occupied by other anions)
N_saturation_pct_summary * 2

#calculating maximum N sorption capacity in absolute numbers
N_max_est <- joined_dat %>%
   group_by(treatment, site) %>%
   summarize(
      N_max_total_est =
         sum(predicted_kg_N_m_ha_soil, na.rm = T),
      N_measured_total_est =
         sum(measured_kg_N_m_ha, na.rm = T)
   )

#estimating number of years of N accumulation until exchange sites are saturated
N_max_est$potential_N_future <-
   N_max_est$N_max_total_est - N_max_est$N_measured_total_est

#number of years based on estimated N accumulation rates of 19 and 43 kg N ha^-1 y^-1
N_max_est$years_remaining_19 <- N_max_est$potential_N_future / 19
N_max_est$years_remaining_43 <- N_max_est$potential_N_future / 43

summary(N_max_est$years_remaining_19)
summary(N_max_est$years_remaining_43)


# summary statistics and ANOVA on percent N saturation differences by treatment at each depth -------------------------

joined_dat <- na.omit(joined_dat)

joined_dat %>% group_by(treatment) %>% summarize(mean = mean(N_saturation_pct, na.rm =
                                                                T))

PctN_treat_depth <-
   joined_dat %>% group_by(depth_num, treatment) %>%
   summarize(mean_N_saturation_pct = mean(N_saturation_pct, na.rm = T))

#subsetting data at each depth
joined_dat1 <- joined_dat[joined_dat$depth_num == '1',]
joined_dat2 <- joined_dat[joined_dat$depth_num == '2',]
joined_dat3 <- joined_dat[joined_dat$depth_num == '3',]
joined_dat4 <- joined_dat[joined_dat$depth_num == '4',]
joined_dat5 <- joined_dat[joined_dat$depth_num == '5',]
joined_dat6 <- joined_dat[joined_dat$depth_num == '6',]
joined_dat7 <- joined_dat[joined_dat$depth_num == '7',]
joined_dat8 <- joined_dat[joined_dat$depth_num == '8',]

joined_dat1_narm <- na.omit(joined_dat1)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat1_narm
)))

joined_dat2_narm <- na.omit(joined_dat2)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat2_narm
)))

#This depth has significant differences
joined_dat3_narm <- na.omit(joined_dat3)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat3_narm
)))
lm3 <-
   (lm(N_saturation_pct ~ as.factor(treatment), data = joined_dat3_narm))
aov3 <- (aov(lm3))

#This depth has significant differences
joined_dat4_narm <- na.omit(joined_dat4)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat4_narm
)))
lm4 <-
   (lm(N_saturation_pct ~ as.factor(treatment), data = joined_dat4_narm))
aov4 <- (aov(lm4))


joined_dat5_narm <- na.omit(joined_dat5)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat5_narm
)))

joined_dat6_narm <- na.omit(joined_dat6)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat6_narm
)))

joined_dat7_narm <- na.omit(joined_dat7)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat7_narm
)))

joined_dat8_narm <- na.omit(joined_dat8)
summary(aov(lm(
   N_saturation_pct ~ as.factor(treatment), data = joined_dat8_narm
)))

TukeyHSD(aov3)
TukeyHSD(aov4)

# Data on N accumulation totals in top 8m and time under different land uses ----------------------------------------------

#sum of total NO3-N summed in top 8m by site and years under different land uses

#read in data on summed soil nitrate-N
N_summed <- read.csv('Nitrate summed by site top 8m.csv')

#extract data on years under different land uses
years_data <- joined_dat %>%
   select(site, years_soy, years_maize,
          years_since_deforested) %>%
   group_by(site) %>%
   summarize(
      years_soy = years_soy[1],
      years_maize = years_maize[1],
      years_since_deforested = years_since_deforested[1]
   )

#join dataframes on summed soil N and years under different land uses
N_summed <- full_join(years_data, N_summed, by = "site")

#renaming total soil N variable
N_summed$measured_kg_N_ha_total <- N_summed$kg_N_soil_total

#formating treatment variable
N_summed$treatment <-
   factor(N_summed$treatment, levels = c('forest', 'soy', 'maize'))
levels(N_summed$treatment)

#summarizing soil N total for forest
N_summed_forest_mean <- N_summed %>%
   filter(treatment == "forest") %>%
   summarize(mean(measured_kg_N_ha_total))

#estimating N accumulation rates based on previous empirical estimates of
#annual N surpluses from the soybean single-cropping and soybean-maize
#double-cropping sites
N_summed$N_surplus_est <-
   (N_summed$years_soy - N_summed$years_maize) * 43 +
   N_summed$years_soy * 19 - as.numeric(N_summed_forest_mean)


# ANOVAs on total NO3-N accumulation -------------------------------------------------

#ANOVA on total NO3-N accumulation by treatment
summary(aov(lm(
   measured_kg_N_ha_total ~ as.factor(treatment), data = N_summed
)))
lm1 <- (lm(kg_N_soil_total ~ as.factor(treatment), data = N_summed))
aov1 <- aov(lm1)
summary(aov1)
TukeyHSD(aov1)

#ANOVA on log-transformed total NO3-N accumulation by treatment
summary(aov(lm(
   log10(kg_N_soil_total) ~ as.factor(treatment), data = N_summed
)))
lm1 <-
   (lm(log10(kg_N_soil_total) ~ as.factor(treatment), data = N_summed))
aov1 <- aov(lm1)
TukeyHSD(aov1)


# linear models on soil N totals versus land use and time since deforestation ------------------------------------
hist((N_summed$measured_kg_N_ha_total))

m1 <-
   lm(measured_kg_N_ha_total ~ years_since_deforested + treatment,
      data = N_summed)
summary(m1)
#treatment has no significant effect, R2=.65

m2 <-
   lm(measured_kg_N_ha_total ~ years_since_deforested , data = N_summed)
summary(m2)
#years since deforested has a significant effect on total measured N p=0.00, R2=0.64

