# created 2025-04-22 by eucarida
# ats updated 2025-04-22 by eucarida
# GOAL: Generate a model for the Many Analyst Exercise in BIO176
# IMPORTANT the script is aranged into sections and not the order of actions all the time. ONE must load all of the "data wrangeling" section befor proseading 



# setup ####
# clean up
rm(list = ls())
gc()


# libraries
library(boot)
library(lme4)  
library(pbkrtest)
library(merTools) 
library(MuMIn)
library(sjPlot)
library(performance)
library(tidyverse)
theme_set(theme_bw())


# load dataframe
df_blue <- read_csv("blue_tit_data_updated_2020-04-18.csv")






# data wrangeling ####
# --- this section id for tidying up the data for model use

# Replacing all placeholder "." with NA
df_blue <- replace(df_blue,
        df_blue == "." & !is.na(df_blue), NA)

# find the amount of NA of listed coloum
sum(is.na(df_blue$d14_hatch_nest_brood_size))
#== 96

# find the number of NA in all colums
df_blue %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

# find the number of NA across Mother IDs 
sum(is.na(df_blue$hatch_mom_Ring))
sum(is.na(df_blue$rear_mom_Ring))


# how many chicks are gone (rear) by day 14
sum(is.na(df_blue$rear_Cs_at_start_of_rearing))

# how many have not been reared
length(which(df_blue$net_rearing_manipulation == 0))

# changing many chr into int
df_blue_dNA$rear_Cs_at_start_of_rearing <- 
  as.numeric(df_blue_dNA$rear_Cs_at_start_of_rearing)

# removing all the coloums that contain any number of NA #
#---
# df_blue_dNA <- df_blue %>% 
#   select_if(~ !any(is.na(.)))
#--- this is not somthing that we found to be useful at the moment as more filtering is needed to extract the desired functional data frame for modelling

# removing all rows that containg NA in $rear_Cs_a_start_of_rearing to funktionaly compare the datat from day 14 and day ~2 ($rear_Cs_a_start_of_rearing) regarding clutch size
df_blue_dNA <- df_blue %>% 
  drop_na(rear_Cs_at_start_of_rearing)
#--- checking that it worked
sum(is.na(df_blue_dNA$rear_Cs_at_start_of_rearing))
#--- it works (=0)

# how many NA in "new coloume"
sum(is.na(df_blue_dNA$rear_mom_Ring))
sum(is.na(df_blue_dNA$rear_dad_Ring))
#--- we will keep rearing mother in the model as the "absent" fathers where plentiful and the theory of paternety uncertenty might be in effect


# removing incorectly counted nests
df_blue_dNA <- df_blue_dNA %>% 
  filter(chicks_lost_percent >= 0)





# data visulisation ####

# maybe we will work the data by the mean of the wight per box/nest not the sigel chick
df_blue %>% 
  group_by(rear_Box) %>% 
  summarise(Mean_mass = mean(day_14_weight))
#--- this visiulasation did not give much

# find the coloum type
str(df_blue_dNA)







# ggplot simple graphs ####

# 1.0 
df_blue %>% 
  ggplot(aes(x = d14_rear_nest_brood_size,
             y = day_14_weight)) +
  geom_point()
#--- it is very messy but it looks somewhat negative

# 1.1 log veriant
df_blue %>% 
  ggplot(aes(x = log(d14_rear_nest_brood_size + 1),
             y = day_14_weight)) +
  geom_point()


df_blue %>% 
  ggplot(aes(x = d14_rear_nest_brood_size,
             y = day_14_weight,
             colour = )) +
  geom_jitter(height = 0,
              width = 0.4,
              alpha = 0.3) +
  geom_smooth(method = "lm")

df_blue %>% 
  ggplot(aes(x = d14_rear_nest_brood_size)) +
  geom_histogram()

# ploted for the amount of chicks at the day of rearing
df_blue %>% 
  ggplot(aes(x = rear_Cs_at_start_of_rearing,
             y = day_14_weight,
             colour = )) +
  geom_jitter(height = 0,
              width = 0.4,
              alpha = 0.3) +
  geom_smooth(method = "lm")
#--- we need to remove NA

# updated df
df_blue_dNA %>% 
  ggplot(aes(x = rear_Cs_at_start_of_rearing,
             y = day_14_weight,
             colour = )) +
  geom_jitter(height = 0,
              width = 0.4,
              alpha = 0.3) +
  geom_smooth(method = "lm")
#--- that looks better




# make new coloum for the lost chicks
df_blue_dNA <- df_blue_dNA %>% 
  mutate(chicks_lost = (rear_Cs_at_start_of_rearing -
                          d14_rear_nest_brood_size))

df_blue_dNA <- df_blue_dNA %>% 
  mutate(chicks_lost_percent = (chicks_lost/rear_Cs_at_start_of_rearing)*100)

df_blue_dNA %>% 
  summarise(chicks_lost_percent) %>% 
  filter(chicks_lost_percent < 0)



# are the factors gausien
df_blue_dNA %>% 
  ggplot(aes(x = rear_Cs_at_start_of_rearing)) +
  geom_histogram()

df_blue_dNA %>% 
  ggplot(aes(x = day_14_weight)) +
  geom_histogram()
#--- we do not need to log our main factors


df_blue_dNA %>% 
  ggplot(aes(x = chicks_lost_percent)) +
  geom_histogram()


df_blue_dNA %>% 
  filter(chicks_lost_percent < -1) %>% 
  group_by(chicks_lost_percent) %>% 
  summarise(d14 = d14_rear_nest_brood_size,
            d2 = rear_Cs_at_start_of_rearing,
            ID = rear_nest_breed_ID,
            DidrID = chick_ring_number)
# how does the distribution of chicks lost look graphed with and without 0s
df_blue_dNA %>% 
  ggplot(aes(x = chicks_lost_percent)) +
  geom_histogram()

df_blue_dNA %>% 
  filter(chicks_lost_percent > 0) %>% 
  ggplot(aes(x = chicks_lost_percent)) +
  geom_histogram()

  # simple lpm ####

# setup #
# lm v.1.0
lm_bleu_dNA <- lm( day_14_weight ~ rear_Cs_at_start_of_rearing,
                   data = df_blue_dNA)

check_model(lm_bleu_dNA)
summary(lm_bleu_dNA)
#--- looks good, why?... to hell with it all

df_blue_dNA %>% 
  ggplot(aes(x = Date_of_day14)) +
  geom_histogram()

df_blue_dNA %>% 
  ggplot(aes(x = Date_of_day14,
             y = day_14_weight)) +
  geom_jitter(height = 0,
              width = 0.2,
              alpha = 0.3) +
  facet_wrap(rear_area ~ hatch_year) +
  geom_smooth()

df_blue_dNA %>% 
  ggplot(aes(x = hatch_year,
             y = Date_of_day14)) +
  geom_jitter(height = 0,
              width = 0.2,
              alpha = 0.3) +
  facet_wrap(rear_area ~ hatch_year) +
  geom_smooth()


# making sure that all of the catagorical data is a factor
df_blue_dNA <- df_blue_dNA %>% 
  mutate(hatch_year = as_factor(hatch_year),
         rear_mom_Ring = as_factor(rear_mom_Ring),
         rear_area = as_factor(rear_area),
         hatch_mom_Ring = as_factor(hatch_mom_Ring),
         home_or_away = as_factor(home_or_away),
         net_rearing_manipulation = as_factor(net_rearing_manipulation))




# mixed models ####
gblue_dNA.mixed <- lmer(day_14_weight ~ 
                          rear_Cs_at_start_of_rearing + 
                          chicks_lost_percent +
                          hatch_year +
                          Date_of_day14 +
                          hatch_year:Date_of_day14 +
                          rear_Cs_at_start_of_rearing:chicks_lost_percent +
                          (1|hatch_mom_Ring) +
                          (1|rear_area) +
                          (1|rear_mom_Ring) +
                          (1|home_or_away) +
                          (1|net_rearing_manipulation),
                        data = df_blue_dNA)


check_model(gblue_dNA.mixed)
#--- oooo so much collinearity
summary(gblue_dNA.mixed)
#--- home-or _away does not seam to be that great of a effect,
#--- 
