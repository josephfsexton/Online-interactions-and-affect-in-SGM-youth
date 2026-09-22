#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#        ONLINE INTERACTIONS AND AFFECT IN SGM YOUTH        #
#   Kirsty A. Clark, Joseph F. Sexton, Jessica L. Hamilton  #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                    Code prepared by JFS                   #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Changelog
# - 08/20/26: changed to include ICCs, taus, and sigmas for final model
# - 08/13/26: changed model table to include SEs + controls for main effects
# - 07/30/26: caught a typo in table collation for simple slopes
# - 07/16/26: cleaned up file, restricted to variables of interest
# - 07/07/26: completed most analyses and early table generations

# load libraries
library(dplyr)        # for data cleaning
library(tidyr)        # for data cleaning
library(ggplot2)      # for plotting
library(patchwork)    # for piecing together ggplots
library(lme4)         # for estimating MLMs
library(lmerTest)     # for getting significance estimates for MLMs
library(performance)  # for getting cond. and marg. R2s for MLMs
library(psych)        # for estimating alphas
library(interactions) # for inspecting interactions from MLMS
library(ICC)          # for estimating ICCs
library(openxlsx)     # for exporting results as .xlsx files

# load data - N = 50 (length(levels(as.factor(data$ID)))), 3369 surveys (n)
baseline <- read.csv("/Users/jfs/Documents/Research/SPIRiT/SPIRiT Baseline De-Identified Data/Baseline_Deidentified_withScales.csv")
data <- read.csv("/Users/jfs/Documents/Research/SPIRiT/SPIRiT EMA De-Identified Data/EMA_Deidentified.csv")

#### CLEAN DATA ####
data <- data %>% # first, create ordered versions for variables of interest
  mutate(
    # online interactions of any kind
    neg_online = max.col(data[,grepl("negmediainteraction",colnames(data))]),
    neg_online_val = max.col(data[,grepl("negmediafeelingscale",colnames(data))]) - 1,
    pos_online = max.col(data[,grepl("posmediainteraction",colnames(data))]),
    pos_online_val = max.col(data[,grepl("posmediainteractfeelingscale",colnames(data))]) - 1,
    # affect
    annoyed = max.col(data[,grepl("annoyed",colnames(data))]) - 1,
    fatigued = max.col(data[,grepl("fatigued",colnames(data))]) - 1,
    exhausted = max.col(data[,grepl("exhausted",colnames(data))]) - 1,
    discouraged = max.col(data[,grepl("discouraged",colnames(data))]) - 1,
    resentful = max.col(data[,grepl("resentful",colnames(data))]) - 1,
    uneasy = max.col(data[,grepl("uneasy",colnames(data))]) - 1,
    cheerful = max.col(data[,grepl("cheerful",colnames(data))]) - 1,
    hopeless = max.col(data[,grepl("hopeless",colnames(data))]) - 1,
    onedge = max.col(data[,grepl("onedge",colnames(data))]) - 1,
    angry = max.col(data[,grepl("angry",colnames(data))]) - 1,
    lively = max.col(data[,grepl("lively",colnames(data))]) - 1,
    anxious = max.col(data[,grepl("anxious",colnames(data))]) - 1,
    sad = max.col(data[,grepl("sad",colnames(data))]) - 1,
    wornout = max.col(data[,grepl("wornout",colnames(data))]) - 1,
    vigorous = max.col(data[,grepl("vigorous",colnames(data))]) - 1,
  ) %>% # second, restrict to variables of interest
  select(
    # identifiers
    ID, Survey_Type, Date, Trigger.Index,
    # online interactions
    neg_online,
    neg_online_val,
    pos_online,
    pos_online_val,
    # affect
    annoyed, fatigued, exhausted, discouraged, resentful,
    uneasy, cheerful, hopeless, onedge, angry,
    lively, anxious, sad, wornout, vigorous,
  ) %>% # third, create dichotomous variables where necessary
  mutate(
    neg_online_any = as.numeric(neg_online %in% 1:9),
    pos_online_any = as.numeric(pos_online %in% 1:9)
  ) %>% # fourth, change variables to character where necessary
  mutate(
    neg_online = neg_online %>%
      recode_values(
        1 ~ "1 = Phone call",
        2 ~ "2 = Text message",
        3 ~ "3 = Social media",
        4 ~ "4 = FaceTime",
        5 ~ "5 = Zoom/Skype",
        6 ~ "6 = Reddit",
        7 ~ "7 = Twitch",
        8 ~ "8 = Discord",
        9 ~ "9 = Other",
        10 ~ "None"
      ),
    pos_online = pos_online %>%
      recode_values(
        1 ~ "1 = Phone call",
        2 ~ "2 = Text message",
        3 ~ "3 = Social media",
        4 ~ "4 = FaceTime",
        5 ~ "5 = Zoom/Skype",
        6 ~ "6 = Reddit",
        7 ~ "7 = Twitch",
        8 ~ "8 = Discord",
        9 ~ "9 = Other",
        10 ~ "None"
      )
  ) %>% # fourth, create affect variables
  mutate(
    posaff = cheerful + lively + vigorous,
    negaff = annoyed + discouraged + resentful + uneasy + hopeless + onedge +
      angry + anxious + sad,
    poms_anger = resentful + angry + annoyed,
    poms_depression = hopeless + discouraged + sad,
    poms_anxiety = anxious + onedge + uneasy
  ) %>% # fifth, create person-centered versions for variables of interest
  mutate(
    posaff_pmc = posaff - ave(posaff, ID, FUN = function(x) mean(x, na.rm = TRUE)),
    negaff_pmc = negaff - ave(negaff, ID, FUN = function(x) mean(x, na.rm = TRUE)),
    poms_anger_pmc = poms_anger - ave(poms_anger, ID, FUN = function(x) mean(x, na.rm = TRUE)),
    poms_depression_pmc = poms_depression - ave(poms_depression, ID, FUN = function(x) mean(x, na.rm = TRUE)),
    poms_anxiety_pmc = poms_anxiety - ave(poms_anxiety, ID, FUN = function(x) mean(x, na.rm = TRUE)),
  ) %>% # sixth, add variable denoting observation number
  arrange(Date) %>% # instead of all AM, then afternoon, then PM
  group_by(ID) %>% mutate(Observation = row_number()) %>% ungroup()

# next, clean baseline data
baseline <- baseline %>%
  rename( # first, rename some variables
    # ID
    ID = record_id,
    # race/ethnicity
    race_AIAN  = race_ethnicity___1,
    race_Asian = race_ethnicity___2,
    race_Black = race_ethnicity___3,
    race_NHOPI = race_ethnicity___4,
    race_White = race_ethnicity___5,
    race_Multi = race_ethnicity___6,
    race_Other = race_ethnicity___7,
    # sexual orientation
    so_straight = sexual_orientation___1,
    so_lesbian = sexual_orientation___2,
    so_gay = sexual_orientation___3,
    so_bisexual = sexual_orientation___4,
    so_pansexual = sexual_orientation___5,
    so_queer = sexual_orientation___6,
    so_questioning = sexual_orientation___7,
    so_asexual = sexual_orientation___8,
    so_other = sexual_orientation___9,
    # gender identity
    gi_girl = gender_identity___1,
    gi_boy = gender_identity___2,
    gi_transgirl = gender_identity___3,
    gi_transboy = gender_identity___4,
    gi_nonbinary = gender_identity___5,
    gi_genderfluid = gender_identity___6,
    gi_gnc = gender_identity___7,
    gi_genderqueer = gender_identity___8,
    gi_twospirit = gender_identity___9,
    gi_agender = gender_identity___10,
    gi_other = gender_identity___11,
    # sitbi lifetime history
    attempt_lifetime = sitbir_1,
    ideation_lifetime = sitbir_5,
    nssi_lifetime = nssi_1
  ) %>% mutate( # second, recode as character where needed
    sex = sex %>%
      recode_values(
        1 ~ "Male",
        2 ~ "Female",
        3 ~ "Intersex"
      ),
    gender_category = gender_category %>%
      recode_values(
        1 ~ "Transgender",
        2 ~ "Cisgender",
        3 ~ "Intersex"
      ),
    binary_vs_nonbinary = binary_vs_nonbinary %>%
      recode_values(
        1 ~ "Binary",
        2 ~ "Nonbinary",
        3 ~ "Neither"
      ),
    education = education %>%
      recode_values(
        1 ~ "5th grade",
        2 ~ "6th grade",
        3 ~ "7th grade",
        4 ~ "8th grade",
        5 ~ "9th grade",
        6 ~ "10th grade",
        7 ~ "11th grade",
        8 ~ "High school diploma or GED",
        9 ~ "Some college or associate's degree",
        10 ~ "Currently enrolled in college",
        11 ~ "Bachelor's degree",
        12 ~ "Currently enrolled in graduate school",
        13 ~ "Graduate degree"
      ),
    employment = employment %>%
      recode_values(
        1 ~ "Under working age",
        2 ~ "Full time",
        3 ~ "Part time",
        4 ~ "Part time work, full time student",
        5 ~ "Disabled and not working",
        6 ~ "Disabled but working off-record",
        7 ~ "Unemployed, student",
        8 ~ "Unemployed, other",
        9 ~ "Homemaker"
      )
    ) %>% mutate( # third, create collapsed race variable (according to ANK [2025] conventions)
      race = case_when(
        race_Multi == 1 ~ "Multiracial",
        race_AIAN == 1 ~ "AIAN",
        race_Asian == 1 ~ "Asian",
        race_Black == 1 ~ "Black",
        race_NHOPI == 1 ~ "NHOPI",
        race_White == 1 ~ "White",
        race_Multi == 1 ~ "Multiracial",
        race_Other == 1 ~ "Other"
      )
    )

# merge baseline RS into data as needed for analyses
data <- data %>% left_join(select(baseline,ID,age,ders_total,phq_total),by="ID")

# three respondents endorsed more than one race, assign them to just multiracial
which(rowSums(baseline[,grepl("race_",colnames(baseline))])>1)
baseline[c(6,41,42),grepl("race_",colnames(baseline))]
summary(mutate_all(baseline[,grepl("race_",colnames(baseline))],as.factor))

# verify using individual participant
data_S529 <- filter(data,ID=="S_529")
data_S505 <- filter(data,ID=="S_505")

# create an alternative restricted to those that report media experiences
data_neg_online <- data %>% filter(neg_online_any==1) %>% # 398 observations across n = 45
  mutate(
    neg_online_val_pmc = neg_online_val - ave(neg_online_val, ID, FUN = mean),
    neg_online_val_sd = ave(neg_online_val, ID, FUN = sd),
    neg_online_val_pmc_std = neg_online_val_pmc / neg_online_val_sd
  )
round(ICCest(ID, y = neg_online_val, data = data_neg_online)$ICC,2) # ICC = 0.16
data_pos_online <- data %>% filter(pos_online_any==1) %>% # 1,427 observations across n = 50
  mutate(
    pos_online_val_pmc = pos_online_val - ave(pos_online_val, ID, FUN = mean),
    pos_online_val_sd = ave(pos_online_val, ID, FUN = sd),
    pos_online_val_pmc_std = pos_online_val_pmc / pos_online_val_sd
  )
round(ICCest(ID, y = pos_online_val, data = data_pos_online)$ICC,2) # ICC = 0.45

# explore how valence differs by kind of media
data_neg_online %>%
  group_by(neg_online) %>%
  summarize(
    n_obs = n(),
    prop = round(n_obs/nrow(data_neg_online),2),
    mean_val = mean(neg_online_val_pmc),
    sd_val = sd(neg_online_val_sd),
    mean_val_std = mean(neg_online_val_pmc_std,na.rm=TRUE),
    sd_val_std = sd(neg_online_val_pmc_std,na.rm=TRUE)
  ) %>%
  arrange(desc(n_obs))
data_pos_online %>%
  group_by(pos_online) %>%
  summarize(
    n_obs = n(),
    prop = round(n_obs/nrow(data_pos_online),2),
    mean_val = mean(pos_online_val_pmc),
    sd_val = sd(pos_online_val_sd),
    mean_val_std = mean(pos_online_val_pmc_std,na.rm=TRUE),
    sd_val_std = sd(pos_online_val_pmc_std,na.rm=TRUE)
  ) %>%
  arrange(desc(n_obs))

# on what %age of occasions with neg. ints are pos. ints also reported?
# -> 81.91% of occasions w neg. ints. also include pos. ints. (vs. 42.36% in general sample)
round(sum(data_neg_online$pos_online_any) / nrow(data_neg_online),4) * 100 # 81.91%
# and vice versa
# -> 22.85% of occasions w pos. ints. also include neg. ints. (vs. 11.81% in general sample)
round(sum(data_pos_online$neg_online_any) / nrow(data_pos_online),4) * 100 # 81.91%

# breakdown of platforms among online experiences (positive or negative)
data_any_online <- data %>%
  pivot_longer(
    cols = c(pos_online,neg_online),
    values_to = "medium"
  ) %>%
  filter(medium != "None")
data_any_online %>%
  group_by(medium) %>%
  summarize(
    n_obs = n(),
    prop = round(n_obs/nrow(data_any_online),2)
  ) %>%
  arrange(desc(n_obs))

# breakdown of platforms among all surveys (positive or negative or neither)
data %>%
  pivot_longer(
    cols = c(pos_online,neg_online),
    values_to = "medium"
  ) %>%
  group_by(medium) %>%
  summarize(
    n_obs = n(),
    prop = round(n_obs/(nrow(data)*2),2) # double nrow bc pivot_longer
  ) %>%
  arrange(desc(prop))

#### SAMPLE DEMOGRAPHICS ####
# should result in: Table 1, matching Nikolaidis-Konstas et al. (2025)
table1 <- data.frame(
  Variable = c(
    "Sociodemographic characteristics",
    "Age",
    "Race", names(table(baseline$race)),
    "Hispanic or Latinx",
    "Sex assigned at birth", names(table(baseline$sex)),
    "Binary or nonbinary gender", names(table(baseline$binary_vs_nonbinary)),
    "Gender modality", names(table(baseline$gender_category)),
    "Gender identity",
      "Girl or woman","Boy or man","Trans girl or woman","Trans boy or man",
      "Nonbinary","Genderfluid","Gender nonconforming","Genderqueer",
      "Agender","Other",
    "Sexual orientation",
      "Straight","Lesbian","Gay","Bisexual","Pansexual","Queer",
      "Questioning","Asexual","Other",
    "Clinical severity",
    "Lifetime attempt",
    "Lifetime ideation",
    "Lifetime NSSI"
  ),
  Freq = c(
    NA,paste0(mean(baseline$age)," (",round(sd(baseline$age),2),")"),
    NA, paste0(table(baseline$race)," (",sprintf("%.2f",round(table(baseline$race)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$latinx)," (",sprintf("%.2f",round(sum(baseline$latinx)/nrow(baseline),3)*100),")"),
    NA, paste0(table(baseline$sex)," (",sprintf("%.2f",round(table(baseline$sex)/nrow(baseline),3)*100),")"),
    NA, paste0(table(baseline$binary_vs_nonbinary)," (",sprintf("%.2f",round(table(baseline$binary_vs_nonbinary)/nrow(baseline),3)*100),")"),
    NA, paste0(table(baseline$gender_category)," (",sprintf("%.2f",round(table(baseline$gender_category)/nrow(baseline),3)*100),")"),
    NA,
      paste0(sum(baseline$gi_girl)," (",sprintf("%.2f",round(sum(baseline$gi_girl)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_boy)," (",sprintf("%.2f",round(sum(baseline$gi_boy)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_transgirl)," (",sprintf("%.2f",round(sum(baseline$gi_transgirl)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_transboy)," (",sprintf("%.2f",round(sum(baseline$gi_trasnboy)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_nonbinary)," (",sprintf("%.2f",round(sum(baseline$gi_nonbinary)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_genderfluid)," (",sprintf("%.2f",round(sum(baseline$gi_genderfluid)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_gnc)," (",sprintf("%.2f",round(sum(baseline$gi_gnc)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_genderqueer)," (",sprintf("%.2f",round(sum(baseline$gi_genderqueer)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_agender)," (",sprintf("%.2f",round(sum(baseline$gi_agender)/nrow(baseline),3)*100),")"),
      paste0(sum(baseline$gi_other)," (",sprintf("%.2f",round(sum(baseline$gi_other)/nrow(baseline),3)*100),")"),
    NA,
    paste0(sum(baseline$so_straight)," (",sprintf("%.2f",round(sum(baseline$so_straight)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_lesbian)," (",sprintf("%.2f",round(sum(baseline$so_lesbian)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_gay)," (",sprintf("%.2f",round(sum(baseline$so_gay)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_bisexual)," (",sprintf("%.2f",round(sum(baseline$so_bisexual)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_pansexual)," (",sprintf("%.2f",round(sum(baseline$so_pansexual)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_queer)," (",sprintf("%.2f",round(sum(baseline$so_queer)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_questioning)," (",sprintf("%.2f",round(sum(baseline$so_questioning)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_asexual)," (",sprintf("%.2f",round(sum(baseline$so_asexual)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$so_other)," (",sprintf("%.2f",round(sum(baseline$so_other)/nrow(baseline),3)*100),")"),
    NA,
    paste0(sum(baseline$attempt_lifetime)," (",sprintf("%.2f",round(sum(baseline$attempt_lifetime)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$ideation_lifetime)," (",sprintf("%.2f",round(sum(baseline$ideation_lifetime)/nrow(baseline),3)*100),")"),
    paste0(sum(baseline$nssi_lifetime)," (",sprintf("%.2f",round(sum(baseline$nssi_lifetime)/nrow(baseline),3)*100),")")
  )
)
# write.csv(table1, "20260618_spiritSmDemographics_jfs.csv", row.names = FALSE)

#### DESCRIPTIVE STATISTICS ####
# response rates
desc1 <- function(data) {
  cat("**Responses per respondent**:\n")
  print(round(c(
    Mean = mean(table(data$ID)),
    SD = sd(table(data$ID)),
    Min = min(table(data$ID)),
    Median = median(table(data$ID)),
    Max = max(table(data$ID))
  ),2))
  cat("\n**Breakdown by time of day**:\n")
  print(cbind(
    n = table(data$Survey_Type),
    percent = paste0(round(table(data$Survey_Type)/nrow(data),4)*100,"%"),
    compliance = paste0(round(table(data$Survey_Type)/(50*28),4)*100,"%")
  ))
  cat("\n**Average number of days per respondent**:\n")
  days_ID <- data %>% group_by(ID) %>% summarize(days = n_distinct(Date))
  print(round(c(
    Mean = mean(days_ID$days),
    SD = sd(days_ID$days),
    Min = min(days_ID$days),
    Median = median(days_ID$days),
    Max = max(days_ID$days)
  ),2))
}
desc1(data)
desc1(data_neg_online)
desc1(data_pos_online)

# tables including means and SDs for variables
data2 <- data %>% select(
  neg_online_any, neg_online_val,
  pos_online_any, pos_online_val,
  posaff, negaff, poms_anger, poms_depression, poms_anxiety
)
desc_tbl <- round(data.frame(
  Mean = sapply(data2, mean, na.rm = TRUE),
  SD   = sapply(data2, sd, na.rm = TRUE),
  Min  = sapply(data2, min, na.rm = TRUE),
  Max  = sapply(data2, max, na.rm = TRUE)
),2)

# pull ICC values and 95% CIs
ICC_tbl <- data.frame(
  ICC = rep(NA,nrow(desc_tbl)),
  ICC_LL95 = rep(NA,nrow(desc_tbl)),
  ICC_UL95 = rep(NA,nrow(desc_tbl)),
  sigma_wp = rep(NA,nrow(desc_tbl)),
  tau_bp = rep(NA,nrow(desc_tbl)),
  row.names = row.names(desc_tbl)
)
for(x in row.names(ICC_tbl)){
  ICC_iter = ICCest(ID, y = x, data = data)
  ICC_tbl[x,] = round(c(ICC_iter$ICC,ICC_iter$LowerCI,ICC_iter$UpperCI,ICC_iter$varw,ICC_iter$vara),2)
}
# warnings:
# - NAs removed from rows
#     - this is fine

# calculate within- and between-person correlations
multicorr <- statsBy(
  select(
    data,
    neg_online_any, neg_online_val,
    pos_online_any, pos_online_val,
    posaff, negaff, poms_anger, poms_depression, poms_anxiety,
    ID
  ), group = "ID"
)
# warnings:
# - in cor(...): the standard deviation is zero
#    - this is for the correlation between pos_online_any ~ pos_online_val,
#      ... because pos_online_any always = 1 when pos_online_val != NA
# assemble cormat w/ within corrs lower-tri, between corrs upper-tri
cor_tbl <- round(multicorr$rwg,2)
cor_tbl[upper.tri(cor_tbl)] = round(multicorr$rbg[upper.tri(cor_tbl)],2)
# inspect p-values, multiplying by number of comparisons (Bonferroni correction)
pcor_tbl <- multicorr$pwg*(2*choose(9,2))
pcor_tbl[upper.tri(pcor_tbl)] = multicorr$pbg[upper.tri(pcor_tbl)]*(2*choose(9,2))
round(pcor_tbl,4)
pcor_tbl < .05

# assemble descriptive statistics, ICCs, and correlations; export
desc_icc_cor_tbl <- cbind(desc_tbl,ICC_tbl,cor_tbl)

# write.csv(desc_icc_cor_tbl,"20260716_spiritOnlineDesc_jfs.csv")

#### DESCRIPTIVE PLOTS ####
# (code taken partly from AW workshop)
# lineplot
lineplot <- ggplot(data, aes(x=Observation, y=posaff_pmc, group=ID)) + 
  xlab("Observation") + #provide a label for the x axis
  ylab("Positive Affect (Sum)") + #provide a label for y axis
  theme_bw() + #use a black and white theme
  geom_line() + #request a line plot
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) #modify font for labels
lineplot #plot the line plot

# plot mean trajectory for visualization of mean
lineplot_m <- ggplot(data, aes(x = Observation, y = posaff_pmc, group = ID)) + 
  xlab("Observation") + #provide a label for the x axis
  ylab("Positive Affect (Sum)") + #provide a label for y axis
  theme_bw() + #use a black and white theme
  geom_line(alpha = 0.3) +                      # light individual trajectories
  stat_summary(
    aes(group = 1),                             # override grouping so we get one mean‐curve
    fun    = mean,
    geom   = "line",
    size   = 1.2,
    color  = "red"
  ) +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold")
  ) + geom_segment(aes(x = min(Observation), xend = max(Observation), y = mean(posaff_pmc), yend = ))
lineplot_m

# positive affect
plt_posaff <- ggplot(data, aes(x = Observation, y = posaff, group = ID)) + 
  scale_y_continuous(breaks=seq(0,30,10)) +
  scale_x_continuous(breaks=seq(1,84,83), expand = c(0.01,0.01)) +
  geom_line(alpha = 0.3, linewidth = .3) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(color='black', size=10, family='avenir', angle=90),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    strip.text=element_text(color='black', size=8, family='avenir', face='bold'),
    plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust=.5),
    plot.background=element_rect(color='white'),
    panel.grid = element_blank()) +
  labs(y="Positive affect") +
  geom_segment(aes(
    x = min(Observation), xend = max(Observation),
    y = mean(posaff,na.rm=TRUE), yend = mean(posaff,na.rm=TRUE))
  )

# negative affect
plt_negaff <- ggplot(data, aes(x = Observation, y = negaff, group = ID)) + 
  scale_y_continuous(breaks=seq(0,90,30)) +
  scale_x_continuous(breaks=seq(1,84,83), expand = c(0.01,0.01)) +
  geom_line(alpha = 0.3, linewidth = .3) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(color='black', size=10, family='avenir', angle=90),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    strip.text=element_text(color='black', size=8, family='avenir', face='bold'),
    plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust=.5),
    plot.background=element_rect(color='white'),
    panel.grid = element_blank()) +
  labs(y="Negative affect") +
  geom_segment(aes(
    x = min(Observation), xend = max(Observation),
    y = mean(negaff,na.rm=TRUE), yend = mean(negaff,na.rm=TRUE))
  )

# anger
plt_anger <- ggplot(data, aes(x = Observation, y = poms_anger, group = ID)) + 
  scale_y_continuous(breaks=seq(0,30,10)) +
  scale_x_continuous(breaks=seq(1,84,83), expand = c(0.01,0.01)) +
  geom_line(alpha = 0.3, linewidth = .3) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(color='black', size=10, family='avenir', angle=90),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    strip.text=element_text(color='black', size=8, family='avenir', face='bold'),
    plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust=.5),
    plot.background=element_rect(color='white'),
    panel.grid = element_blank()) +
  labs(y="Anger") +
  geom_segment(aes(
    x = min(Observation), xend = max(Observation),
    y = mean(poms_anger,na.rm=TRUE), yend = mean(poms_anger,na.rm=TRUE))
  )

# depression
plt_depression <- ggplot(data, aes(x = Observation, y = poms_depression, group = ID)) + 
  scale_y_continuous(breaks=seq(0,30,10)) +
  scale_x_continuous(breaks=seq(1,84,83), expand = c(0.01,0.01)) +
  geom_line(alpha = 0.3, linewidth = .3) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(color='black', size=10, family='avenir', angle=90),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    strip.text=element_text(color='black', size=8, family='avenir', face='bold'),
    plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust=.5),
    plot.background=element_rect(color='white'),
    panel.grid = element_blank()) +
  labs(y="Depression") +
  geom_segment(aes(
    x = min(Observation), xend = max(Observation),
    y = mean(poms_depression,na.rm=TRUE), yend = mean(poms_depression,na.rm=TRUE))
  )

# anxiety
plt_anxiety <- ggplot(data, aes(x = Observation, y = poms_anxiety, group = ID)) + 
  scale_y_continuous(breaks=seq(0,30,10)) +
  scale_x_continuous(breaks=seq(1,84,83), expand = c(0.01,0.01)) +
  geom_line(alpha = 0.3, linewidth = .3) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(color='black', size=10, family='avenir', angle=90),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    strip.text=element_text(color='black', size=8, family='avenir', face='bold'),
    plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust=.5),
    plot.background=element_rect(color='white'),
    panel.grid = element_blank()) +
  labs(x="Observation",y="Anxiety") +
  geom_segment(aes(
    x = min(Observation), xend = max(Observation),
    y = mean(poms_anxiety,na.rm=TRUE), yend = mean(poms_anxiety,na.rm=TRUE))
  )

# patch together
plt_posaff + plt_negaff + plt_anger + plt_depression + plt_anxiety +
  plot_layout(ncol=1, axes = "collect")

#### MULTILEVEL MODELS ####
# model 1: positive interaction only
mdl1_posaff     <- lmer(posaff          ~ pos_online_any + (1|ID), data = data)
mdl1_negaff     <- lmer(negaff          ~ pos_online_any + (1|ID), data = data)
mdl1_anger      <- lmer(poms_anger      ~ pos_online_any + (1|ID), data = data)
mdl1_depression <- lmer(poms_depression ~ pos_online_any + (1|ID), data = data)
mdl1_anxiety    <- lmer(poms_anxiety    ~ pos_online_any + (1|ID), data = data)
# model 2: negative interaction only
mdl2_posaff     <- lmer(posaff          ~ neg_online_any + (1|ID), data = data)
mdl2_negaff     <- lmer(negaff          ~ neg_online_any + (1|ID), data = data)
mdl2_anger      <- lmer(poms_anger      ~ neg_online_any + (1|ID), data = data)
mdl2_depression <- lmer(poms_depression ~ neg_online_any + (1|ID), data = data)
mdl2_anxiety    <- lmer(poms_anxiety    ~ neg_online_any + (1|ID), data = data)
# model 3: positive and negative interactions
mdl3_posaff     <- lmer(posaff          ~ pos_online_any + neg_online_any + (1|ID), data = data)
mdl3_negaff     <- lmer(negaff          ~ pos_online_any + neg_online_any + (1|ID), data = data)
mdl3_anger      <- lmer(poms_anger      ~ pos_online_any + neg_online_any + (1|ID), data = data)
mdl3_depression <- lmer(poms_depression ~ pos_online_any + neg_online_any + (1|ID), data = data)
mdl3_anxiety    <- lmer(poms_anxiety    ~ pos_online_any + neg_online_any + (1|ID), data = data)
# model 3b: positive and negative interactions + controls
mdl3b_posaff     <- lmer(posaff          ~ pos_online_any + neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl3b_negaff     <- lmer(negaff          ~ pos_online_any + neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl3b_anger      <- lmer(poms_anger      ~ pos_online_any + neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl3b_depression <- lmer(poms_depression ~ pos_online_any + neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl3b_anxiety    <- lmer(poms_anxiety    ~ pos_online_any + neg_online_any + ders_total + phq_total + (1|ID), data = data)
# model 4: positive and negative interaction + their interaction
mdl4_posaff     <- lmer(posaff          ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + (1|ID), data = data)
mdl4_negaff     <- lmer(negaff          ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + (1|ID), data = data)
mdl4_anger      <- lmer(poms_anger      ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + (1|ID), data = data)
mdl4_depression <- lmer(poms_depression ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + (1|ID), data = data)
mdl4_anxiety    <- lmer(poms_anxiety    ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + (1|ID), data = data)
# model 5: positive and negative interaction + their interaction + controls
mdl5_posaff     <- lmer(posaff          ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl5_negaff     <- lmer(negaff          ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl5_anger      <- lmer(poms_anger      ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl5_depression <- lmer(poms_depression ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + ders_total + phq_total + (1|ID), data = data)
mdl5_anxiety    <- lmer(poms_anxiety    ~ pos_online_any + neg_online_any + pos_online_any:neg_online_any + ders_total + phq_total + (1|ID), data = data)

#### MULTILEVEL MODELS - COLLATE RESULTS AND GENERATE PLOTS ####
tbl_mlm <- cbind(
  rbind(
    c("Positive affect (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction"),rep(NA,4)),
    round(summary(mdl1_posaff)$coefficients,3),
    c(paste0("Model 2: negative online interaction"),rep(NA,4)),
    round(summary(mdl2_posaff)$coefficients,3),
    c(paste0("Model 3: positive and negative online interactions"),rep(NA,4)),
    round(summary(mdl3_posaff)$coefficients,3),
    c(paste0("Model 3b: positive and negative online interactions + controls"),rep(NA,4)),
    round(summary(mdl3b_posaff)$coefficients,3),
    c(paste0("Model 4: positive and negative online interactions + their interaction"),rep(NA,4)),
    round(summary(mdl4_posaff)$coefficients,3),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls"),rep(NA,4)),
    round(summary(mdl5_posaff)$coefficients,3)
  ),
  rbind(
    c("Negative affect (0-90)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction"),rep(NA,4)),
    round(summary(mdl1_negaff)$coefficients,3),
    c(paste0("Model 2: negative online interaction"),rep(NA,4)),
    round(summary(mdl2_negaff)$coefficients,3),
    c(paste0("Model 3: positive and negative online interactions"),rep(NA,4)),
    round(summary(mdl3_negaff)$coefficients,3),
    c(paste0("Model 3b: positive and negative online interactions + controls"),rep(NA,4)),
    round(summary(mdl3b_negaff)$coefficients,3),
    c(paste0("Model 4: positive and negative online interactions + their interaction"),rep(NA,4)),
    round(summary(mdl4_negaff)$coefficients,3),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls"),rep(NA,4)),
    round(summary(mdl5_negaff)$coefficients,3)
  ),
  rbind(
    c("Anger (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction"),rep(NA,4)),
    round(summary(mdl1_anger)$coefficients,3),
    c(paste0("Model 2: negative online interaction"),rep(NA,4)),
    round(summary(mdl2_anger)$coefficients,3),
    c(paste0("Model 3: positive and negative online interactions"),rep(NA,4)),
    round(summary(mdl3_anger)$coefficients,3),
    c(paste0("Model 3b: positive and negative online interactions + controls"),rep(NA,4)),
    round(summary(mdl3b_anger)$coefficients,3),
    c(paste0("Model 4: positive and negative online interactions + their interaction"),rep(NA,4)),
    round(summary(mdl4_anger)$coefficients,3),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls"),rep(NA,4)),
    round(summary(mdl5_anger)$coefficients,3)
  ),
  rbind(
    c("Depression (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction"),rep(NA,4)),
    round(summary(mdl1_depression)$coefficients,3),
    c(paste0("Model 2: negative online interaction"),rep(NA,4)),
    round(summary(mdl2_depression)$coefficients,3),
    c(paste0("Model 3: positive and negative online interactions"),rep(NA,4)),
    round(summary(mdl3_depression)$coefficients,3),
    c(paste0("Model 3b: positive and negative online interactions + controls"),rep(NA,4)),
    round(summary(mdl3b_depression)$coefficients,3),
    c(paste0("Model 4: positive and negative online interactions + their interaction"),rep(NA,4)),
    round(summary(mdl4_depression)$coefficients,3),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls"),rep(NA,4)),
    round(summary(mdl5_depression)$coefficients,3)
  ),
  rbind(
    c("Anxiety (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction"),rep(NA,4)),
    round(summary(mdl1_anxiety)$coefficients,3),
    c(paste0("Model 2: negative online interaction"),rep(NA,4)),
    round(summary(mdl2_anxiety)$coefficients,3),
    c(paste0("Model 3: positive and negative online interactions"),rep(NA,4)),
    round(summary(mdl3_anxiety)$coefficients,3),
    c(paste0("Model 3b: positive and negative online interactions + controls"),rep(NA,4)),
    round(summary(mdl3b_anxiety)$coefficients,3),
    c(paste0("Model 4: positive and negative online interactions + their interaction"),rep(NA,4)),
    round(summary(mdl4_anxiety)$coefficients,3),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls"),rep(NA,4)),
    round(summary(mdl5_anxiety)$coefficients,3)
  )
)

tbl_mlm_cis <- cbind(
  rbind(
    c("Positive affect (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction (n = ",length(levels(mdl1_posaff@flist$ID)),", nobs = ",length(mdl1_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl1_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl1_posaff)[3:4,]),2),
    c(paste0("Model 2: negative online interaction (n = ",length(levels(mdl2_posaff@flist$ID)),", nobs = ",length(mdl2_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl2_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl2_posaff)[3:4,]),2),
    c(paste0("Model 3: positive and negative online interactions (n = ",length(levels(mdl3_posaff@flist$ID)),", nobs = ",length(mdl3_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3_posaff)[3:5,]),2),
    c(paste0("Model 3b: positive and negative online interactions + controls (n = ",length(levels(mdl3b_posaff@flist$ID)),", nobs = ",length(mdl3b_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3b_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3b_posaff)[3:7,]),2),
    c(paste0("Model 4: positive and negative online interactions + their interaction (n = ",length(levels(mdl4_posaff@flist$ID)),", nobs = ",length(mdl4_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl4_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl4_posaff)[3:6,]),2),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls (n = ",length(levels(mdl5_posaff@flist$ID)),", nobs = ",length(mdl5_posaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl5_posaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl5_posaff)[3:8,]),2)
  ),
  rbind(
    c("Negative affect (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction (n = ",length(levels(mdl1_negaff@flist$ID)),", nobs = ",length(mdl1_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl1_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl1_negaff)[3:4,]),2),
    c(paste0("Model 2: negative online interaction (n = ",length(levels(mdl2_negaff@flist$ID)),", nobs = ",length(mdl2_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl2_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl2_negaff)[3:4,]),2),
    c(paste0("Model 3: positive and negative online interactions (n = ",length(levels(mdl3_negaff@flist$ID)),", nobs = ",length(mdl3_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3_negaff)[3:5,]),2),
    c(paste0("Model 3b: positive and negative online interactions + controls (n = ",length(levels(mdl3b_negaff@flist$ID)),", nobs = ",length(mdl3b_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3b_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3b_negaff)[3:7,]),2),
    c(paste0("Model 4: positive and negative online interactions + their interaction (n = ",length(levels(mdl4_negaff@flist$ID)),", nobs = ",length(mdl4_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl4_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl4_negaff)[3:6,]),2),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls (n = ",length(levels(mdl5_negaff@flist$ID)),", nobs = ",length(mdl5_negaff@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl5_negaff)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl5_negaff)[3:8,]),2)
  ),
  rbind(
    c("Anger (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction (n = ",length(levels(mdl1_anger@flist$ID)),", nobs = ",length(mdl1_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl1_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl1_anger)[3:4,]),2),
    c(paste0("Model 2: negative online interaction (n = ",length(levels(mdl2_anger@flist$ID)),", nobs = ",length(mdl2_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl2_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl2_anger)[3:4,]),2),
    c(paste0("Model 3: positive and negative online interactions (n = ",length(levels(mdl3_anger@flist$ID)),", nobs = ",length(mdl3_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3_anger)[3:5,]),2),
    c(paste0("Model 3b: positive and negative online interactions + controls (n = ",length(levels(mdl3b_anger@flist$ID)),", nobs = ",length(mdl3b_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3b_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3b_anger)[3:7,]),2),
    c(paste0("Model 4: positive and negative online interactions + their interaction (n = ",length(levels(mdl4_anger@flist$ID)),", nobs = ",length(mdl4_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl4_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl4_anger)[3:6,]),2),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls (n = ",length(levels(mdl5_anger@flist$ID)),", nobs = ",length(mdl5_anger@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl5_anger)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl5_anger)[3:8,]),2)
  ),
  rbind(
    c("Depression (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction (n = ",length(levels(mdl1_depression@flist$ID)),", nobs = ",length(mdl1_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl1_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl1_depression)[3:4,]),2),
    c(paste0("Model 2: negative online interaction (n = ",length(levels(mdl2_depression@flist$ID)),", nobs = ",length(mdl2_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl2_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl2_depression)[3:4,]),2),
    c(paste0("Model 3: positive and negative online interactions (n = ",length(levels(mdl3_depression@flist$ID)),", nobs = ",length(mdl3_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3_depression)[3:5,]),2),
    c(paste0("Model 3b: positive and negative online interactions + controls (n = ",length(levels(mdl3b_depression@flist$ID)),", nobs = ",length(mdl3b_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3b_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3b_depression)[3:7,]),2),
    c(paste0("Model 4: positive and negative online interactions + their interaction (n = ",length(levels(mdl4_depression@flist$ID)),", nobs = ",length(mdl4_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl4_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl4_depression)[3:6,]),2),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls (n = ",length(levels(mdl5_depression@flist$ID)),", nobs = ",length(mdl5_depression@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl5_depression)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl5_depression)[3:8,]),2)
  ),
  rbind(
    c("Anxiety (0-30)",rep(NA,4)),
    c(paste0("Model 1: positive online interaction (n = ",length(levels(mdl1_anxiety@flist$ID)),", nobs = ",length(mdl1_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl1_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl1_anxiety)[3:4,]),2),
    c(paste0("Model 2: negative online interaction (n = ",length(levels(mdl2_anxiety@flist$ID)),", nobs = ",length(mdl2_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl2_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl2_anxiety)[3:4,]),2),
    c(paste0("Model 3: positive and negative online interactions (n = ",length(levels(mdl3_anxiety@flist$ID)),", nobs = ",length(mdl3_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3_anxiety)[3:5,]),2),
    c(paste0("Model 3b: positive and negative online interactions + controls (n = ",length(levels(mdl3b_anxiety@flist$ID)),", nobs = ",length(mdl3b_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl3b_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl3b_anxiety)[3:7,]),2),
    c(paste0("Model 4: positive and negative online interactions + their interaction (n = ",length(levels(mdl4_anxiety@flist$ID)),", nobs = ",length(mdl4_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl4_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl4_anxiety)[3:6,]),2),
    c(paste0("Model 5: positive and negative online interactions + their interaction + controls (n = ",length(levels(mdl5_anxiety@flist$ID)),", nobs = ",length(mdl5_anxiety@flist$ID),")"),rep(NA,4)),
    round(cbind(summary(mdl5_anxiety)$coefficients[,c("Estimate","Std. Error","Pr(>|t|)")],confint(mdl5_anxiety)[3:8,]),2)
  )
)

# write.csv(tbl_mlm_cis, "20260813_spiritOnlineMlmsCis_jfs.csv", row.names = TRUE)

# fetch random effect (tau) and residual (sigma) variance
tbl_randeffects <- data.frame(
  posaff = round(as.data.frame(VarCorr(mdl5_posaff))$sdcor^2,2),
  negaff = round(as.data.frame(VarCorr(mdl5_negaff))$sdcor^2,2),
  anger = round(as.data.frame(VarCorr(mdl5_anger))$sdcor^2,2),
  depression = round(as.data.frame(VarCorr(mdl5_depression))$sdcor^2,2),
  anxiety = round(as.data.frame(VarCorr(mdl5_anxiety))$sdcor^2,2)
)
row.names(tbl_randeffects) = c("intercept_var_tau","residual_var_sigma")

# fetch ICC values
tbl_icc <- data.frame(
  posaff = round(as.double(icc(mdl5_posaff)$ICC_adjusted),2),
  negaff = round(as.double(icc(mdl5_negaff)$ICC_adjusted),2),
  anger = round(as.double(icc(mdl5_anger)$ICC_adjusted),2),
  depression = round(as.double(icc(mdl5_depression)$ICC_adjusted),2),
  anxiety = round(as.double(icc(mdl5_anxiety)$ICC_adjusted),2)
)
row.names(tbl_icc) = "ICC_adjusted"

# also observe the marginal and conditional R^2 values
tbl_r2 <- data.frame(
  posaff = round(as.double(r2(mdl5_posaff)),2),
  negaff = round(as.double(r2(mdl5_negaff)),2),
  anger = round(as.double(r2(mdl5_anger)),2),
  depression = round(as.double(r2(mdl5_depression)),2),
  anxiety = round(as.double(r2(mdl5_anxiety)),2)
)
row.names(tbl_r2) = c("CondR2","MargR2")

#### PROBE INTERACTIONS ####

# simple slopes
sim_slope_posaff     <- sim_slopes(mdl5_posaff, pred = neg_online_any, modx = pos_online_any)
sim_slope_negaff     <- sim_slopes(mdl5_negaff, pred = neg_online_any, modx = pos_online_any)
sim_slope_anger      <- sim_slopes(mdl5_anger, pred = neg_online_any, modx = pos_online_any)
sim_slope_depression <- sim_slopes(mdl5_depression, pred = neg_online_any, modx = pos_online_any)
sim_slope_anxiety    <- sim_slopes(mdl5_anxiety, pred = neg_online_any, modx = pos_online_any)

# in reverse direction
sim_slope_posaff_rev     <- sim_slopes(mdl5_posaff, pred = pos_online_any, modx = neg_online_any)
sim_slope_negaff_rev     <- sim_slopes(mdl5_negaff, pred = pos_online_any, modx = neg_online_any)
sim_slope_anger_rev      <- sim_slopes(mdl5_anger, pred = pos_online_any, modx = neg_online_any)
sim_slope_depression_rev <- sim_slopes(mdl5_depression, pred = pos_online_any, modx = neg_online_any)
sim_slope_anxiety_rev    <- sim_slopes(mdl5_anxiety, pred = pos_online_any, modx = neg_online_any)

# collate simple slopes
tbl_slopes <- cbind(
  rbind(
    ### positive affect ###
    c("Positive affect", rep(NA,3)),
    c("Moderating effect of pos int on the effect of neg int", rep(NA,3)),
    c("Pos int reported?","Slope of neg int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_posaff$slopes$Est.,2),
      paste0("(",round(sim_slope_posaff$slopes$`2.5%`,2),",",round(sim_slope_posaff$slopes$`97.5%`,2),")"),
      round(sim_slope_posaff$slopes$p,2)
    ),
    c("Moderating effect of neg int on the effect of pos int", rep(NA,3)),
    c("Neg int reported?","Slope of pos int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_posaff_rev$slopes$Est.,2),
      paste0("(",round(sim_slope_posaff_rev$slopes$`2.5%`,2),",",round(sim_slope_posaff_rev$slopes$`97.5%`,2),")"),
      round(sim_slope_posaff_rev$slopes$p,2)
    )
  ),
    
    ### negative affect ###
  rbind(
    c("Negative affect", rep(NA,3)),
    c("Moderating effect of pos int on the effect of neg int", rep(NA,3)),
    c("Pos int reported?","Slope of neg int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_negaff$slopes$Est.,2),
      paste0("(",round(sim_slope_negaff$slopes$`2.5%`,2),",",round(sim_slope_negaff$slopes$`97.5%`,2),")"),
      round(sim_slope_negaff$slopes$p,2)
    ),
    c("Moderating effect of neg int on the effect of pos int", rep(NA,3)),
    c("Neg int reported?","Slope of pos int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_negaff_rev$slopes$Est.,2),
      paste0("(",round(sim_slope_negaff_rev$slopes$`2.5%`,2),",",round(sim_slope_negaff_rev$slopes$`97.5%`,2),")"),
      round(sim_slope_negaff_rev$slopes$p,2)
    )
  ),
    
  ### anger ###
  rbind(
    c("Anger", rep(NA,3)),
    c("Moderating effect of pos int on the effect of neg int", rep(NA,3)),
    c("Pos int reported?","Slope of neg int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_anger$slopes$Est.,2),
      paste0("(",round(sim_slope_anger$slopes$`2.5%`,2),",",round(sim_slope_anger$slopes$`97.5%`,2),")"),
      round(sim_slope_anger$slopes$p,2)
    ),
    c("Moderating effect of neg int on the effect of pos int", rep(NA,3)),
    c("Neg int reported?","Slope of pos int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_anger_rev$slopes$Est.,2),
      paste0("(",round(sim_slope_anger_rev$slopes$`2.5%`,2),",",round(sim_slope_anger_rev$slopes$`97.5%`,2),")"),
      round(sim_slope_anger_rev$slopes$p,2)
    )
  ),
    
  ### depression ###
  rbind(
    c("Positive affect", rep(NA,3)),
    c("Moderating effect of pos int on the effect of neg int", rep(NA,3)),
    c("Pos int reported?","Slope of neg int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_depression$slopes$Est.,2),
      paste0("(",round(sim_slope_depression$slopes$`2.5%`,2),",",round(sim_slope_depression$slopes$`97.5%`,2),")"),
      round(sim_slope_depression$slopes$p,2)
    ),
    c("Moderating effect of neg int on the effect of pos int", rep(NA,3)),
    c("Neg int reported?","Slope of pos int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_depression_rev$slopes$Est.,2),
      paste0("(",round(sim_slope_depression_rev$slopes$`2.5%`,2),",",round(sim_slope_depression_rev$slopes$`97.5%`,2),")"),
      round(sim_slope_depression_rev$slopes$p,2)
    )
  ),
    
  ### anxiety ###
  rbind(
    c("Anxiety", rep(NA,3)),
    c("Moderating effect of pos int on the effect of neg int", rep(NA,3)),
    c("Pos int reported?","Slope of neg int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_anxiety$slopes$Est.,2),
      paste0("(",round(sim_slope_anxiety$slopes$`2.5%`,2),",",round(sim_slope_anxiety$slopes$`97.5%`,2),")"),
      round(sim_slope_anxiety$slopes$p,2)
    ),
    c("Moderating effect of neg int on the effect of pos int", rep(NA,3)),
    c("Neg int reported?","Slope of pos int","95% CI","p"),
    cbind(
      c("No","Yes"),
      round(sim_slope_anxiety_rev$slopes$Est.,2),
      paste0("(",round(sim_slope_anxiety_rev$slopes$`2.5%`,2),",",round(sim_slope_anxiety_rev$slopes$`97.5%`,2),")"),
      round(sim_slope_anxiety_rev$slopes$p,2)
    )
  )
)

# write.csv(tbl_slopes, "20260730_spiritOnlineSimSlopes_jfs.csv", row.names = FALSE)

# johnson-neyman plots
jn_plt_posaff <- johnson_neyman(
  mdl5_posaff, pred = neg_online_any, modx = pos_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of negative online interaction",
  modx.label = "Positive online interaction?",
  title = "Positive affect"
)$plot
jn_plt_negaff <- johnson_neyman(
  mdl5_negaff, pred = neg_online_any, modx = pos_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of negative online interaction",
  modx.label = "Positive online interaction?",
  title = "Negative affect"
)$plot
jn_plt_anger <- johnson_neyman(
  mdl5_anger, pred = neg_online_any, modx = pos_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of negative online interaction",
  modx.label = "Positive online interaction?",
  title = "Anger"
)$plot
jn_plt_depression <- johnson_neyman(
  mdl5_depression, pred = neg_online_any, modx = pos_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of negative online interaction",
  modx.label = "Positive online interaction?",
  title = "Depression"
)$plot
jn_plt_anxiety <- johnson_neyman(
  mdl5_anxiety, pred = neg_online_any, modx = pos_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of negative online interaction",
  modx.label = "Positive online interaction?",
  title = "Anxiety"
)$plot

# modify the johnson-neyman plots
enhance_jn_plot <- function(plt){
  plt_iter <- plt + scale_x_continuous(breaks = seq(0,1,1)) + 
    theme_minimal() +
    theme(
      axis.text.x=element_text(color='black', size=10, family='avenir'),
      axis.text.y=element_text(color='black', size=10, family='avenir'), 
      axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
      axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2),
      plot.title=element_text(color='black', size=14, family='avenir', face='bold', hjust =.5),
      plot.background=element_rect(color='white'),
      legend.position = "none"
  )
  return(plt_iter)
}
jn_plt_posaff <- enhance_jn_plot(jn_plt_posaff)
jn_plt_negaff <- enhance_jn_plot(jn_plt_negaff)
jn_plt_anger <- enhance_jn_plot(jn_plt_anger)
jn_plt_depression <- enhance_jn_plot(jn_plt_depression)
jn_plt_anxiety <- enhance_jn_plot(jn_plt_anxiety)

# patch plots together
jn_plt_posaff + jn_plt_negaff + jn_plt_anger + jn_plt_depression + jn_plt_anxiety +
  plot_layout(ncol=5, axes = "collect", guides = "collect")

# interaction plots
int_plt_posaff <- interact_plot(
  mdl5_posaff, pred = neg_online_any, modx = pos_online_any, interval = TRUE,
  x.label = "Negative online interaction?",
  y.label = "Positive affect",
  legend.main = "Positive online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
)
int_plt_negaff <- interact_plot(
  mdl5_negaff, pred = neg_online_any, modx = pos_online_any, interval = TRUE,
  x.label = "Negative online interaction?",
  y.label = "Negative affect",
  legend.main = "Positive online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
)
int_plt_anger <- interact_plot(
  mdl5_anger, pred = neg_online_any, modx = pos_online_any, interval = TRUE,
  x.label = "Negative online interaction?",
  y.label = "Anger",
  legend.main = "Positive online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
)
int_plt_depression <- interact_plot(
  mdl5_depression, pred = neg_online_any, modx = pos_online_any, interval = TRUE,
  x.label = "Negative online interaction?",
  y.label = "Depression",
  legend.main = "Positive online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
)
int_plt_anxiety <- interact_plot(
  mdl5_anxiety, pred = neg_online_any, modx = pos_online_any, interval = TRUE,
  x.label = "Negative online interaction?",
  y.label = "Anxiety",
  legend.main = "Positive online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
)

# modify the simple slopes plots
enhance_int_plot <- function(plt){
  plt_iter <- plt + theme_minimal() + theme(
    axis.text.x=element_text(color='black', size=10, family='avenir'),
    axis.text.y=element_text(color='black', size=10, family='avenir'), 
    axis.title.x=element_text(color='black', size=12, family='avenir', face='bold', vjust=-1), 
    axis.title.y=element_text(color='black', size=12, family='avenir', face='bold', vjust=2), 
    plot.background=element_rect(color='white')
  )
  return(plt_iter)
}
int_plt_posaff = enhance_int_plot(int_plt_posaff)
int_plt_negaff = enhance_int_plot(int_plt_negaff)
int_plt_anger = enhance_int_plot(int_plt_anger)
int_plt_depression = enhance_int_plot(int_plt_depression)
int_plt_anxiety = enhance_int_plot(int_plt_anxiety)

# patch plots together
int_plt_posaff + int_plt_negaff + int_plt_anger + int_plt_depression + int_plt_anxiety +
  plot_layout(ncol=5, axes = "collect", guides = "collect")
ggsave("20260821_spiritOnlineInteractionPlots_jfs.jpg",width=13,height=5,dpi=400)

#### PROBE INTERACTIONS IN OTHER DIRECTION ####
# johnson-neyman plots
jn_plt_posaff_rev <- johnson_neyman(
  mdl5_posaff, pred = pos_online_any, modx = neg_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of positive online interaction",
  modx.label = "Negative online interaction?",
  title = "Positive affect"
)$plot %>% enhance_jn_plot()
jn_plt_negaff_rev <- johnson_neyman(
  mdl5_negaff, pred = pos_online_any, modx = neg_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of positive online interaction",
  modx.label = "Negative online interaction?",
  title = "Negative affect"
)$plot %>% enhance_jn_plot()
jn_plt_anger_rev <- johnson_neyman(
  mdl5_anger, pred = pos_online_any, modx = neg_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of positive online interaction",
  modx.label = "Negative online interaction?",
  title = "Anger"
)$plot %>% enhance_jn_plot()
jn_plt_depression_rev <- johnson_neyman(
  mdl5_depression, pred = pos_online_any, modx = neg_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of positive online interaction",
  modx.label = "Negative online interaction?",
  title = "Depression"
)$plot %>% enhance_jn_plot()
jn_plt_anxiety_rev <- johnson_neyman(
  mdl5_anxiety, pred = pos_online_any, modx = neg_online_any,
  sig.color = "coral1", insig.color = "grey",
  mod.range = c(0,1),
  y.label = "Slope of positive online interaction",
  modx.label = "Negative online interaction?",
  title = "Anxiety"
)$plot %>% enhance_jn_plot()

# patch plots together
jn_plt_posaff_rev + jn_plt_negaff_rev + jn_plt_anger_rev + jn_plt_depression_rev + jn_plt_anxiety_rev +
  plot_layout(ncol=5, axes = "collect", guides = "collect")

# interaction plots
int_plt_posaff_rev <- interact_plot(
  mdl5_posaff, pred = pos_online_any, modx = neg_online_any, interval = TRUE,
  x.label = "Positive online interaction?",
  y.label = "Positive affect",
  legend.main = "Negative online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
) %>% enhance_int_plot()
int_plt_negaff_rev <- interact_plot(
  mdl5_negaff, pred = pos_online_any, modx = neg_online_any, interval = TRUE,
  x.label = "Positive online interaction?",
  y.label = "Negative affect",
  legend.main = "Negative online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
) %>% enhance_int_plot()
int_plt_anger_rev <- interact_plot(
  mdl5_anger, pred = pos_online_any, modx = neg_online_any, interval = TRUE,
  x.label = "Positive online interaction?",
  y.label = "Anger",
  legend.main = "Negative online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
) %>% enhance_int_plot()
int_plt_depression_rev <- interact_plot(
  mdl5_depression, pred = pos_online_any, modx = neg_online_any, interval = TRUE,
  x.label = "Positive online interaction?",
  y.label = "Depression",
  legend.main = "Negative online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
) %>% enhance_int_plot()
int_plt_anxiety_rev <- interact_plot(
  mdl5_anxiety, pred = pos_online_any, modx = neg_online_any, interval = TRUE,
  x.label = "Positive online interaction?",
  y.label = "Anxiety",
  legend.main = "Negative online interaction?",
  pred.labels = c("No","Yes"),
  modx.labels = c("No","Yes"),
  colors = c("lightsalmon","dodgerblue1")
) %>% enhance_int_plot()

# patch plots together
int_plt_posaff_rev + int_plt_negaff_rev + int_plt_anger_rev + int_plt_depression_rev + int_plt_anxiety_rev +
  plot_layout(ncol=5, axes = "collect", guides = "collect")
