require(googlesheets)
require(tidyverse)
require(ggpubr)
require(lubridate)

theme_set(theme_bw())

p1 <- "Sea turtles are large, air-breathing reptiles that inhabit tropical and subtropical seas throughout the world. Their shells consist of an upper part (carapace) and a lower section (plastron). Hard scales (or scutes) cover all but the leatherback, and the number and arrangement of these scutes can be used to determine the species.
Sea turtles come in many different sizes, shapes and colors. The olive ridley is usually less than 100 pounds, while the leatherback typically ranges from 650 to 1,300 pounds! The upper shell, or carapace, of each sea turtle species ranges in length, color, shape and arrangement of scales.
Sea turtles do not have teeth, but their jaws have modified beaks suited to their particular diet. They do not have visible ears but have eardrums covered by skin. They hear best at low frequencies, and their sense of smell is excellent. Their vision underwater is good, but they are nearsighted out of water. Their streamlined bodies and large flippers make them remarkably adapted to life at sea. However, sea turtles maintain close ties to land."

p2 <- "Machines powered by artificial intelligence increasingly mediate our social, cultural, economic and political interactions. 
Understanding the behaviour of artificial intelligence systems is essential
to our ability to control their actions, reap their benefits and minimize their harms."

len_p1 <- str_count(p1, '\\w+') 
len_p2 <- str_count(p2, '\\w+')

ubitalk <- gs_title("UbiTalk User Study")
ubitalk <- ubitalk %>%
  gs_read() 

user_data <- ubitalk %>% mutate(
        control_time = control_min*60 + control_sec,
        treatment_time = treatment_min*60 + treatment_sec,
        wpm_S = (60/S_sec)*len_p2,
        wpm_N = (60/N_sec)*len_p2,
        wpm_F = (60/F_sec)*len_p2,
        Treatment = len_p1/treatment_time*60,
        Control = len_p1/control_time*60,
        treat_control_diff = (control_time - treatment_time),
        p_chnge = (treat_control_diff / control_time) * 100.0
        ) %>% 
  select("Fast" = wpm_F, "Normal" = wpm_N, "Slow" = wpm_S, everything(), -c(treatment_min, treatment_sec, control_min, control_sec))

#user_data <- user_data %>% drop_na() # Drop NA values
write.csv(user_data, file = "data/user_data.csv") #for reproducible results without google sheets access 
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
ggexport(p, filename = "figures/p2_speed_differences.png")

#treatment vs.control WPM
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggboxplot(x = "group", y = "wpm",
            add = "jitter",
            color = "group", palette = "jco") +
  stat_compare_means()
ggpar(p, 
      legend = "none",
      main = "WPM by Group",
      xlab = "Group",
      ylab = "Words Per Minute")
ggexport(p, filename = "figures/p1_group_speed_differences.png")

## ~~~~ KATIE'S PLOTS ~~~~~~~~~~~~~~~~~
#treatment vs.control WPM 
user_data$fast_threshold <- factor(user_data$fast_threshold)
p2 <- user_data %>% 
  gather("group", "wpm", 17:18) 
df <- data.frame(f1=user_data$fast_threshold, label=c("low tresh","high thresh"), 
                 f2= p2, label=c("treatment","control"),
                 stringsAsFactors = FALSE)
df$f1f2 <- interaction(df $f1, df$f2.group)
p <- ggplot(aes(y = f2.wpm, x = f2.group, fill = f1), 
          data = df) +
          labs(fill='Fast Thresh') +
          geom_boxplot() + 
          xlab(' Treatment vs. Control') + 
          ylab('Words per Minute') +
          ggtitle('Comparing Fast Thresholds Effect on WPM')
ggsave(p, filename = "figures/threshold_wpm.png", device = "png") 

# Compare WPM for first read app or No app
user_data$fast_threshold <- factor(user_data$fast_threshold)
p2 <- user_data %>% 
  gather("group", "wpm", 17:18) 
df <- data.frame(f1=user_data$first_read,
                 f2= p2, label=c("treatment","control"),
                 stringsAsFactors = FALSE)
df$f1f2 <- interaction(df $f1, df$f2.group)
p <- ggplot(aes(y = f2.wpm, x = f2.group, fill = f1), 
       data = df) +
  labs(fill='Fast Thresh') +
  geom_boxplot() + 
  xlab(' Treatment vs. Control') + 
  ylab('Words per Minute') +
  ggtitle('Comparing First Read App Effect on WPM')
ggsave(p, filename = "figures/first_read_wpm.png", device = "png") 

#treatment vs.control WPM -- Pct Change
p <- ggplot(aes(y = p_chnge, 
           x = reorder(name,-p_chnge)),
          data = user_data) +
          xlab('Name') + 
          ylab('WPM % Change') +
          ggtitle('Percent Change in WPM') +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_bar(stat = "identity",fill = "#FF6666")
ggsave(p, filename = "figures/wpm_pct_change_by_group.png", device = "png") 
## ~~~~~~~~~~~~~~~~~~~~~~~~~

#gender differences 
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggboxplot(x = "gender", y = "wpm",
            color = "gender", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
p <- ggpar(p,
           legend = "none",
           main = "WPM by Gender/Group",
           xlab = "Gender",
           ylab = "Words Per Minute")
ggexport(p, filename = "figures/gender_speed_differences.png")

#native speaker differences 
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggboxplot(x = "native_speaker", y = "wpm",
            color = "native_speaker", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
p <- ggpar(p,
           legend = "none",
           main = "WPM by Speaker Status/Group",
           xlab = "Native Speaker?",
           ylab = "Words Per Minute")
ggexport(p, filename = "figures/native_speaker_speed_differences.png")

#speed differences 
p <- user_data %>% 
  group_by(name) %>% 
  summarise(high_med = Fast - Normal,
            med_low = Normal - Slow,
            high_low = Fast - Slow) %>% 
  gather("difference", "wpm", 2:4) %>%
  ggplot(aes(x = name, y = wpm, fill = difference)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Participant", y = "WPM difference", title = "Differences in WPM by Speaking Speed")
ggsave(p, filename = "figures/wpm_difference_by_speaking_speed.png", device = "png") 

#by app first 
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggboxplot(x = "first_read", y = "wpm",
            color = "first_read", palette = "jco",
            add = "jitter") + 
  stat_compare_means()
p <- ggpar(p,
           legend = "none",
           main = "WPM by First Read/Group",
           xlab = "First Read",
           ylab = "Words Per Minute")
ggexport(p, filename = "figures/first_read_speed_differences.png")

#plot of WPM by speed
p <- user_data %>% 
  gather("speed", "wpm", 1:3) %>% 
  group_by(speed) %>% 
  ggplot(aes(x = reorder(name, -wpm), y = wpm, fill = speed)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~speed) + 
  theme(axis.text.x = element_text(angle = 90 , hjust = 1)) + 
  labs(x = "Participant", y = "Words per Minute", title = "WPM by Speaking Speed", fill = "Speed")
ggsave(p, filename = "figures/wpm_by_speaking_speed.png", device = "png") 

#speaking speeds by age - no signal 
p <- user_data %>% 
  gather("group", "wpm", 17:18) %>% 
  ggplot(aes(x = age, y = wpm)) + 
  geom_point() +
  facet_grid(~group) + 
  labs(x = "Age", y = "Words per Minute", title = "Speaking Speed by Age")
ggsave(p, filename = "figures/wpm_by_age.png", device = "png") 

#APP DATA 
app_data <- app_data %>%
  group_by(participant) %>%
  mutate(time = dplyr::row_number()) %>% 
  filter(!(time %in% 1:2), participant != "Quentin") #remove faulty first two rows and outliers

#arrange by percentage on pace
p <- app_data %>% 
  group_by(participant) %>% 
  summarize(`Slow` = mean(speed == "Slow"),
            `On pace` = mean(speed == "On_pace"),
            `Fast` = mean(speed == "Fast")) %>% 
  gather("speed", "percent", 2:4) %>%
  arrange(percent) %>%
  ggplot(aes(x = participant, y = 100*percent, fill = speed)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Participant", y = "Percent", title = "Percentage Speeds by Participant", fill = "Speed")
ggsave(p, filename = "figures/percentage_speed.png", device = "png") 

#states over time
p <- app_data %>% 
  group_by(time, participant) %>% 
  mutate(speed = case_when(
    speed == "Slow" ~ -1,
    speed == "On_pace" ~ 0,
    TRUE ~ 1
  )) %>% 
  ggplot(aes(x = time, y = speed)) + 
  geom_line() + 
  facet_grid(participant~.) + 
  labs(x = "Time", y = "Speed", title = "Speed Over Time by Participant") + 
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c("Slow", "On pace", "Fast"))
ggsave(p, filename = "figures/speed_over_time.png", device = "png") 

#compare observed to actual WPM 
totals <- app_data %>% 
  group_by(participant) %>% 
  select(n_words_total) %>% 
  slice(n())

observed_avg <- mean(totals[1:13,"n_words_total"] %>% select(n_words_total) %>% .$n_words_total)
observed_avg / len_p1 #about 3x WPM in app 

#plot of actual vs. observed
p <- totals %>% 
  ggplot(aes(x = reorder(participant, -n_words_total), y = n_words_total)) + 
  geom_bar(stat = "identity", alpha = 0.5) + 
  geom_hline(aes(yintercept = len_p1)) + 
  annotate("text", x = 4, y = len_p1 + 15, label = "Actual word count (178)") + 
  labs(x = "Participant", y = "Observed Words (by app)", title = "Observed Word Count per Participant") + 
  scale_y_continuous(breaks = seq(0, 600, 50)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(p, filename = "figures/observed_vs_actual_wpm.png", device = "png") 


p <- user_data %>% 
  gather("group", "wpm", 17:18)
#p$group <- factor(p$group, levels = c('wpm_without_app','wpm_with_app'))
ggplot(p, aes(fill = group, x = reorder(name,-p_chnge), y = wpm)) +
  geom_ribbon(aes(ymin=100,ymax=150), fill="blue", alpha="0.5") +
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  geom_hline(yintercept=150) +
  geom_hline(yintercept=100) +
  xlab('Name') + 
  ylab('WPM')
