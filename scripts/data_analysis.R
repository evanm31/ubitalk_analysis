require(googlesheets)
require(tidyverse)
require(ggpubr)
require(lubridate)

theme_set(theme_bw())

p1 <- "Sea turtles are large, air-breathing reptiles that inhabit tropical and subtropical seas throughout the world. Their shells consist of an upper part (carapace) and a lower section (plastron). Hard scales (or scutes) cover all but the leatherback, and the number and arrangement of these scutes can be used to determine the species.
Sea turtles come in many different sizes, shapes and colors. The olive ridley is usually less than 100 pounds, while the leatherback typically ranges from 650 to 1,300 pounds! The upper shell, or carapace, of each sea turtle species ranges in length, color, shape and arrangement of scales.
Sea turtles do not have teeth, but their jaws have modified “beaks” suited to their particular diet. They do not have visible ears but have eardrums covered by skin. They hear best at low frequencies, and their sense of smell is excellent. Their vision underwater is good, but they are nearsighted out of water. Their streamlined bodies and large flippers make them remarkably adapted to life at sea. However, sea turtles maintain close ties to land."

p2 <- "Machines powered by artificial intelligence increasingly mediate our social, cultural, economic and political interactions. 
Understanding the behaviour of artificial intelligence systems is essential
to our ability to control their actions, reap their benefits and minimize their harms."

len_p1 <- str_count(p1, '\\w+') 
len_p2 <- str_count(p2, '\\w+')

ubitalk <- gs_title("UbiTalk User Study")
ubitalk <- ubitalk %>%
  gs_read() 

<<<<<<< HEAD:data_analysis.R

user_data <- ubitalk[2:17, 1:15] %>% 
=======
user_data <- ubitalk %>% 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
        mutate(control_time = ifelse(control_min == 1, 60 + control_sec, control_sec),
        treatment_time = ifelse(treatment_min == 1, 60 + treatment_sec, treatment_sec),
        wpm_S = (60/S_sec)*len_p2,
        wpm_N = (60/N_sec)*len_p2,
        wpm_F = (60/F_sec)*len_p2,
<<<<<<< HEAD:data_analysis.R
        wpm_with_app = ifelse(treatment_time < 60, (treatment_time/60)*len_p1, (60/treatment_time)*len_p1),
        wpm_without_app = ifelse(control_time < 60, (control_time/60)*len_p1, (60/control_time)*len_p1),
        treat_control_diff = (control_time - treatment_time),
        p_chnge = (treat_control_diff / control_time) * 100.0
  ) %>% 
  select("Fast" = wpm_F, "Normal" = wpm_N, "Slow" = wpm_S, everything(), -c(treatment_min, treatment_sec, control_min, control_sec))


user_data <- user_data %>% drop_na() # Drop NA values

=======
        wpm_treatment = ifelse(treatment_time < 60, (treatment_time/60)*len_p1, (60/treatment_time)*len_p1),
        wpm_control = ifelse(control_time < 60, (control_time/60)*len_p1, (60/control_time)*len_p1)
  ) %>% 
  select("Fast" = wpm_F, "Normal" = wpm_N, "Slow" = wpm_S, everything(), -c(treatment_min, treatment_sec, control_min, control_sec))

write.csv(user_data, file = "data/user_data.csv") #for reproducible results without google sheets access 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
app_data <- read_csv("data/final_data.csv")

#VISUALIZATIONS 

#USER DATA 
#speaking speed
p <- user_data %>% 
  gather("speed", "wpm", 1:3) %>% 
  ggboxplot(x = "speed", y = "wpm", 
            add = "jitter",
            color = "speed", palette = "jco") +
  stat_compare_means(method = "anova")
ggpar(p, 
      legend = "none",
      main = "WPM by Speech Time",
      xlab = "Speech Speed",
      ylab = "Words Per Minute")
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
<<<<<<< HEAD:data_analysis.R
=======
ggexport(p, filename = "figures/p2_speed_differences.png")
>>>>>>> 2e57b05a6c5237c57bec8b0adb22d6527da2273a:scripts/data_analysis.R

=======
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R
=======
>>>>>>> parent of 95bd184... add figures

#treatment vs.control WPM
p <- user_data %>% 
<<<<<<< HEAD:data_analysis.R
  gather("group", "wpm", 17:18) %>% 
=======
  gather("group", "wpm", 15:16) %>% 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
  ggboxplot(x = "group", y = "wpm",
            add = "jitter",
            color = "group", palette = "jco") +
  stat_compare_means()
ggpar(p, 
      legend = "none",
      main = "WPM by Group",
      xlab = "Group",
      ylab = "Words Per Minute")
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
<<<<<<< HEAD:data_analysis.R
=======
ggexport(p, filename = "figures/p1_speed_differences.png")
>>>>>>> 2e57b05a6c5237c57bec8b0adb22d6527da2273a:scripts/data_analysis.R

## ~~~~ KATIE'S PLOTS ~~~~~~~~~~~~~~~~~
#treatment vs.control WPM 
user_data$fast_threshold <- factor(user_data$fast_threshold)
p2 <- user_data %>% 
  gather("group", "wpm", 17:18) 
df <- data.frame(f1=user_data$fast_threshold, label=c("low tresh","high thresh"), 
                 f2= p2, label=c("treatment","control"),
                 stringsAsFactors = FALSE)
df$f1f2 <- interaction(df $f1, df$f2.group)
ggplot(aes(y = f2.wpm, x = f2.group, fill = f1), 
          data = df) +
          labs(fill='Fast Thresh') +
          geom_boxplot() + 
          xlab(' Treatment vs. Control') + 
          ylab('Words per Minute') +
          ggtitle('Comparing Fast Thresholds affect on WPM')


# Compare WPM for first read app or No app
user_data$fast_threshold <- factor(user_data$fast_threshold)
p2 <- user_data %>% 
  gather("group", "wpm", 17:18) 
df <- data.frame(f1=user_data$first_read,
                 f2= p2, label=c("treatment","control"),
                 stringsAsFactors = FALSE)
df$f1f2 <- interaction(df $f1, df$f2.group)
ggplot(aes(y = f2.wpm, x = f2.group, fill = f1), 
       data = df) +
  labs(fill='Fast Thresh') +
  geom_boxplot() + 
  xlab(' Treatment vs. Control') + 
  ylab('Words per Minute') +
  ggtitle('Comparing Fast Thresholds affect on WPM')

#treatment vs.control WPM -- Pct Change
ggplot(aes(y = p_chnge, 
           x = reorder(name,-p_chnge)),
          data = user_data) +
          xlab('Name') + 
          ylab('WPM % Change') +
          ggtitle('Percent Change in WPM') +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_bar(stat = "identity",fill = "#FF6666")
## ~~~~~~~~~~~~~~~~~~~~~~~~~
=======
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R
=======
>>>>>>> parent of 95bd184... add figures

#gender differences 
p <- user_data %>% 
<<<<<<< HEAD:data_analysis.R
  gather("group", "wpm", 17:18) %>% 
=======
  gather("group", "wpm", 15:16) %>% 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
  ggboxplot(x = "gender", y = "wpm",
            color = "gender", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
ggpar(p)

#native speaker differences 
p <- user_data %>% 
<<<<<<< HEAD:data_analysis.R
  gather("group", "wpm", 17:18) %>% 
=======
  gather("group", "wpm", 15:16) %>% 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
  ggboxplot(x = "native_speaker", y = "wpm",
            color = "native_speaker", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
ggpar(p)

#speed differences 
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
user_data %>% 
=======
p <- user_data %>% 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
=======
user_data %>% 
>>>>>>> parent of 95bd184... add figures
  group_by(name) %>% 
  summarise(high_med = Fast - Normal,
            med_low = Normal - Slow,
            high_low = Fast - Slow) %>% 
  gather("difference", "wpm", 2:4) %>%
  ggplot(aes(x = name, y = wpm, fill = difference)) + 
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
  geom_bar(stat = "identity", position = "dodge") +
<<<<<<< HEAD:data_analysis.R
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
=======
  geom_bar(stat = "identity", position = "dodge")
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R

#by app first 
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggboxplot(x = "first_read", y = "wpm",
            color = "first_read", palette = "jco",
<<<<<<< HEAD:data_analysis.R
            add = "jitter") 
ggpar(p)
=======
  labs(x = "Participant", y = "WPM difference", title = "Differences in WPM by Speaking Speed")
ggsave(p, filename = "figures/wpm_difference_by_speaking_speed.png", device = "png") 
=======
  geom_bar(stat = "identity", position = "dodge")
>>>>>>> parent of 95bd184... add figures

#by app first 
p <- user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggboxplot(x = "first_read", y = "wpm",
            color = "first_read", palette = "jco",
            add = "jitter") + 
stat_compare_means()
<<<<<<< HEAD
p <- ggpar(p,
           legend = "none",
           main = "WPM by First Read/Group",
           xlab = "First Read",
           ylab = "Words Per Minute")
ggexport(p, filename = "figures/first_read_speed_differences.png")
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R

#+ 
#stat_compare_means()

=======
            add = "jitter") + 
stat_compare_means()
ggpar(p)
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R
=======
ggpar(p)
>>>>>>> parent of 95bd184... add figures

#plot of WPM by speed
#arrange each in descending order? 
user_data %>% 
  gather("speed", "wpm", 1:3) %>% 
  group_by(speed) %>% 
  ggplot(aes(x = reorder(name, -wpm), y = wpm, fill = speed)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~speed) + 
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
  labs() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
=======
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Participant", y = "Words per Minute", title = "WPM by Speaking Speed")
ggsave(p, filename = "figures/wpm_by_speaking_speed.png", device = "png") 
>>>>>>> 95bd18454bfd81c16cd5e1efc348ae0a1a15c9d1:scripts/data_analysis.R
=======
  labs()
>>>>>>> parent of 95bd184... add figures

#speaking speeds by age
#no signal 
user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggplot(aes(x = age, y = wpm)) + 
  geom_point() +
  facet_grid(~group)
  
#APP DATA 
app_data <- app_data %>%
  group_by(participant) %>%
<<<<<<< HEAD:data_analysis.R
  mutate(time = dplyr::row_number())
=======
  labs()

#speaking speeds by age
#no signal 
user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggplot(aes(x = age, y = wpm)) + 
  geom_point() +
  facet_grid(~group)
  
#APP DATA 
app_data <- app_data %>%
  group_by(participant) %>%
  mutate(time = dplyr::row_number()) %>% 
  filter(!(time %in% 1:2), participant != "Quentin") #remove faulty first two rows
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R
=======
  mutate(time = dplyr::row_number()) %>% 
<<<<<<< HEAD
  filter(!(time %in% 1:2), participant != "Quentin") #remove faulty first two rows and outliers
>>>>>>> 2e57b05a6c5237c57bec8b0adb22d6527da2273a:scripts/data_analysis.R
=======
  filter(!(time %in% 1:2), participant != "Quentin") #remove faulty first two rows
>>>>>>> parent of 95bd184... add figures

#arrange by percentage on pace
app_data %>% 
  group_by(participant) %>% 
<<<<<<< HEAD
<<<<<<< HEAD:data_analysis.R
  summarize(pct.slow = mean(speed == "Slow"),
            pct.on_pace = mean(speed == "On_pace"),
            pct.fast = mean(speed == "Fast")) %>% 
<<<<<<< HEAD:data_analysis.R
=======
  arrange(pct.on_pace) %>% 
>>>>>>> parent of 95bd184... add figures:scripts/data_analysis.R
=======
  summarize(`Slow` = mean(speed == "Slow"),
            `On pace` = mean(speed == "On_pace"),
            `Fast` = mean(speed == "Fast")) %>% 
>>>>>>> 2e57b05a6c5237c57bec8b0adb22d6527da2273a:scripts/data_analysis.R
=======
  summarize(pct.slow = mean(speed == "Slow"),
            pct.on_pace = mean(speed == "On_pace"),
            pct.fast = mean(speed == "Fast")) %>% 
  arrange(pct.on_pace) %>% 
>>>>>>> parent of 95bd184... add figures
  gather("speed", "percent", 2:4) %>%
  arrange(percent) %>%
  ggplot(aes(x = participant, y = percent, fill = speed)) + 
  geom_bar(stat = "identity")

#words over time - not interesting
app_data %>% 
  ggplot(aes(x = time, y = n_words_total)) + 
  geom_line(group = 1) + 
  facet_grid(participant~.)

#states over time
app_data %>% 
  group_by(time, participant) %>% 
  mutate(speed = case_when(
    speed == "Slow" ~ -1,
    speed == "On_pace" ~ 0,
    TRUE ~ 1
  )) %>% 
  ggplot(aes(x = time, y = speed)) + 
  geom_line() + 
  facet_grid(participant~.)

#compare observed to actual WPM 
totals <- app_data %>% 
  group_by(participant) %>% 
  select(n_words_total) %>% 
  slice(n())

observed_avg <- mean(totals[1:13,"n_words_total"] %>% select(n_words_total) %>% .$n_words_total)
observed_avg / len_p1 #about 3x WPM in app 

#plot of actual vs. observed
totals %>% 
  ggplot(aes(x = reorder(participant, -n_words_total), y = n_words_total)) + 
  geom_bar(stat = "identity", alpha = 0.5) + 
  geom_hline(aes(yintercept = len_p1)) + 
  annotate("text", x = 4, y = len_p1 + 15, label = "Actual word count") + 
  labs(x = "Participant", y = "Observed Words (by app)", title = "Observed Word Count by Participant") + 
  scale_y_continuous(breaks = seq(0, 600, 50))
