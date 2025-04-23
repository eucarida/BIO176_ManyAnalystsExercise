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
mblue_dNA.mixed <- lmer(day_14_weight ~ 
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


check_model(mblue_dNA.mixed)
#--- oooo so much collinearity
summary(mblue_dNA.mixed)
#--- home-or _away does not seam to be that great of a effect,
#--- we would like to center date of day14

# center date of day 14
df_blue_dNA <- df_blue_dNA %>% 
  mutate(cen_Date_of_day14 = Date_of_day14 - 
                                mean(df_blue_dNA$Date_of_day14, 
                                     na.rm = TRUE))

# v.1.2
mblue_dNA.mixed2 <- lmer(day_14_weight ~ 
                          rear_Cs_at_start_of_rearing + 
                          chicks_lost_percent +
                          hatch_year +
                          cen_Date_of_day14 +
                          hatch_year:cen_Date_of_day14 +
                          rear_Cs_at_start_of_rearing:chicks_lost_percent +
                          (1|hatch_mom_Ring) +
                          (1|rear_area) +
                          (1|rear_mom_Ring) +
                          (1|home_or_away) +
                          (1|net_rearing_manipulation),
                        data = df_blue_dNA)

check_model(mblue_dNA.mixed2)

summary(mblue_dNA.mixed2)

df_blue_dNA <- df_blue_dNA
  mutate(chicks_lost_percent = chicks_lost_percent/100)

check_model(mblue_dNA.mixed2,
            check = "vif",
            text = element_text(size = 3))


df_blue_dNA %>% 
  ggplot(aes(x = chicks_lost_percent)) +
  geom_histogram()

df_blue_dNA %>% 
  ggplot(aes(x = log(chicks_lost_percent + 1))) +
  geom_histogram()


# v.1.3 we remove chicks lost rear Cs interaction
mblue_dNA.mixed3 <- lmer(day_14_weight ~ 
                           rear_Cs_at_start_of_rearing + 
                           chicks_lost_percent +
                           hatch_year +
                           cen_Date_of_day14 +
                           hatch_year:cen_Date_of_day14 +
                           (1|hatch_mom_Ring) +
                           (1|rear_area) +
                           (1|rear_mom_Ring) +
                           (1|home_or_away) +
                           (1|net_rearing_manipulation),
                         data = df_blue_dNA)

check_model(mblue_dNA.mixed3,
            check = "vif")
summary(mblue_dNA.mixed3)

# v.1.4 we remove chicks lost rear Cs interaction
mblue_dNA.mixed4 <- lmer(day_14_weight ~ 
                           rear_Cs_at_start_of_rearing + 
                           chicks_lost_percent +
                           hatch_year +
                           cen_Date_of_day14 +
                           (1|hatch_mom_Ring) +
                           (1|rear_area) +
                           (1|rear_mom_Ring) +
                           (1|home_or_away) +
                           (1|net_rearing_manipulation),
                         data = df_blue_dNA)

check_model(mblue_dNA.mixed4,
            check = "vif")
check_model(mblue_dNA.mixed4,
            check = "outliers")
summary(mblue_dNA.mixed4)

#--- after discus ions we deside that his is the mainf 

#--- after discussing the vif 


# anova

mblue_dNA.mixed5 <- update(mblue_dNA.mixed4,
                           ~. - (1|home_or_away))

anova(mblue_dNA.mixed5,
      mblue_dNA.mixed4,
      test = "Chisq")

AIC(mblue_dNA.mixed,
    mblue_dNA.mixed2,
    mblue_dNA.mixed3,
    mblue_dNA.mixed4,
    mblue_dNA.mixed5)

mblue_dNA.mixed4.2 <- update(mblue_dNA.mixed4,
                           ~. - (1|rear_area))

anova(mblue_dNA.mixed4.2,
      mblue_dNA.mixed4,
      test = "Chisq")

mblue_dNA.mixed4.3 <- update(mblue_dNA.mixed4,
                             ~. - (1|net_rearing_manipulation))

anova(mblue_dNA.mixed4.3,
      mblue_dNA.mixed4,
      test = "Chisq")

sum(is.na(df_blue_dNA$net_rearing_manipulation))


df_blue_dNA %>% 
  filter(is.na(df_blue_dNA$net_rearing_manipulation)) %>% 
  group_by(net_rearing_manipulation) %>% 
  summarise(d14 = d14_rear_nest_brood_size,
            d2 = rear_Cs_at_start_of_rearing,
            ID = rear_nest_breed_ID,
            DidrID = chick_ring_number)

df_blue_dNA %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  str()

# more filtering out NA from the data set as it seams to disrupt the anova
df_blue_dNA <- df_blue_dNA %>% 
  drop_na(net_rearing_manipulation,
          hatch_mom_Ring,
          rear_mom_Ring)



sum(is.na(df_blue_dNA$net_rearing_manipulation))
sum(is.na(df_blue_dNA$hatch_mom_Ring))
sum(is.na(df_blue_dNA$rear_mom_Ring))
#--- no more NA in the model, yipi

## A redo of what is above but with removed NA ####

# v.1.4 we remove chicks lost rear Cs interaction
blue_dNA.mixed.t1 <- lmer(day_14_weight ~ 
                           rear_Cs_at_start_of_rearing + 
                           chicks_lost_percent +
                           hatch_year +
                           cen_Date_of_day14 +
                           (1|hatch_mom_Ring) +
                           (1|rear_area) +
                           (1|rear_mom_Ring) +
                           (1|home_or_away) +
                           (1|net_rearing_manipulation),
                         data = df_blue_dNA)

check_model(blue_dNA.mixed.t1)
check_model(blue_dNA.mixed.t1,
            check = "vif")
check_model(blue_dNA.mixed.t1,
            check = "outliers")
summary(blue_dNA.mixed.t1)

#--- after discus ions we deside that his is the mainf 

#--- after discussing the vif 


# anova time
blue_dNA.mixed.t2 <- update(blue_dNA.mixed.t1,
                           ~. - (1|home_or_away))

anova(blue_dNA.mixed.t1,
      blue_dNA.mixed.t2,
      test = "Chisq")
#--- t1 is better (p-vaue = ***)

blue_dNA.mixed.t3 <- update(blue_dNA.mixed.t1,
                             ~. - (1|rear_area))

anova(blue_dNA.mixed.t3,
      blue_dNA.mixed.t1,
      test = "Chisq")
#--- t1 is sightly better

blue_dNA.mixed.t4 <- update(blue_dNA.mixed.t1,
                             ~. - (1|net_rearing_manipulation))

anova(blue_dNA.mixed.t4,
      blue_dNA.mixed.t1,
      test = "Chisq")
#--- t1 is better

blue_dNA.mixed.t5 <- update(blue_dNA.mixed.t1,
                            ~. - (1|hatch_mom_Ring))

anova(blue_dNA.mixed.t5,
      blue_dNA.mixed.t1,
      test = "Chisq")
#--- t1 is better

blue_dNA.mixed.t6 <- update(blue_dNA.mixed.t1,
                            ~. - (1|rear_mom_Ring))

anova(blue_dNA.mixed.t6,
      blue_dNA.mixed.t1,
      test = "Chisq")
#--- t1 is better
#--- we assume that by this metric (and the once before) that the t1 model is the best fit for the mixed model...

summary(blue_dNA.mixed.t1)


# Vart är vi påväg?....
# För 10 poäng...

# new data 1
df_new_blue_dNA.1 <-
  crossing(
    rear_Cs_at_start_of_rearing =
      seq(
        min(df_blue_dNA$rear_Cs_at_start_of_rearing),
        max(df_blue_dNA$rear_Cs_at_start_of_rearing),
        length = 100
      ),
    chicks_lost_percent =
      rep(
        min(df_blue_dNA$chicks_lost_percent),
        max(df_blue_dNA$chicks_lost_percent),
        times = 100
      ),
    cen_Date_of_day14 =
      rep(
        min(df_blue_dNA$cen_Date_of_day14),
        max(df_blue_dNA$cen_Date_of_day14),
        times = 100
      ),
    hatch_year = c(2001, 2002, 2003))



# new data 2 
df_new_blue_dNA.2 <-
  tidyr::expand(
    df_blue_dNA,
    rear_Cs_at_start_of_rearing =
      seq(
        min(df_blue_dNA$rear_Cs_at_start_of_rearing),
        max(df_blue_dNA$rear_Cs_at_start_of_rearing),
        length = 100
      ),
    chicks_lost_percent =
      rep(
        mean(df_blue_dNA$chicks_lost_percent),
        times = 100
      ),
    cen_Date_of_day14 =
      rep(
        mean(df_blue_dNA$cen_Date_of_day14),
        times = 100
      ),
    hatch_year = c(2001, 2002, 2003),
    nesting(
      hatch_mom_Ring,
      rear_mom_Ring,
      net_rearing_manipulation,
      rear_area,
      home_or_away
    )
  )
#--- this is our first test


# for reffrensing our old work
# df_newgrouse3 <- 
#   tidyr::expand(df_grouse, 
#                 cen_log_totalcount = seq(min(df_grouse$cen_log_totalcount),
#                                          max(df_grouse$cen_log_totalcount),
#                                          length = 100), 
#                 cen_prev = c(-1,0,1),
#                 nesting(moor, drive))

# predictInterval(g3.mixed, 
#                 newdata = df_newgrouse3, 
#                 level = 0.8,
#                 n.sims = 100)

blue_pred_dNA.1 <- predictInterval(blue_dNA.mixed.t1,
                                   newdata = df_new_blue_dNA.2,
                                   level = 0.8,
                                   n.sims = 1000)
str(df_blue_dNA)    
                            

