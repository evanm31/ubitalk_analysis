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

user_data <- ubitalk %>% 
        mutate(control_time = ifelse(control_min == 1, 60 + control_sec, control_sec),
        treatment_time = ifelse(treatment_min == 1, 60 + treatment_sec, treatment_sec),
        wpm_S = (60/S_sec)*len_p2,
        wpm_N = (60/N_sec)*len_p2,
        wpm_F = (60/F_sec)*len_p2,
        wpm_treatment = ifelse(treatment_time < 60, (treatment_time/60)*len_p1, (60/treatment_time)*len_p1),
        wpm_control = ifelse(control_time < 60, (control_time/60)*len_p1, (60/control_time)*len_p1)
  ) %>% 
  select("Fast" = wpm_F, "Normal" = wpm_N, "Slow" = wpm_S, everything(), -c(treatment_min, treatment_sec, control_min, control_sec))

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

#treatment vs.control WPM
p <- user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggboxplot(x = "group", y = "wpm",
            add = "jitter",
            color = "group", palette = "jco") +
  stat_compare_means()
ggpar(p, 
      legend = "none",
      main = "WPM by Group",
      xlab = "Group",
      ylab = "Words Per Minute")

#gender differences 
p <- user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggboxplot(x = "gender", y = "wpm",
            color = "gender", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
ggpar(p)

#native speaker differences 
p <- user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggboxplot(x = "native_speaker", y = "wpm",
            color = "native_speaker", palette = "jco",
            add = "jitter",
            facet.by = "group", short.panel.labs = FALSE) +
  stat_compare_means()
ggpar(p)

#speed differences 
user_data %>% 
  group_by(name) %>% 
  summarise(high_med = Fast - Normal,
            med_low = Normal - Slow,
            high_low = Fast - Slow) %>% 
  gather("difference", "wpm", 2:4) %>%
  ggplot(aes(x = name, y = wpm, fill = difference)) + 
  geom_bar(stat = "identity", position = "dodge")

#by app first 
p <- user_data %>% 
  gather("group", "wpm", 15:16) %>% 
  ggboxplot(x = "first_read", y = "wpm",
            color = "first_read", palette = "jco",
            add = "jitter") + 
stat_compare_means()
ggpar(p)

#plot of WPM by speed
#arrange each in descending order? 
user_data %>% 
  gather("speed", "wpm", 1:3) %>% 
  group_by(speed) %>% 
  ggplot(aes(x = reorder(name, -wpm), y = wpm, fill = speed)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~speed) + 
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

#arrange by percentage on pace
app_data %>% 
  group_by(participant) %>% 
  summarize(pct.slow = mean(speed == "Slow"),
            pct.on_pace = mean(speed == "On_pace"),
            pct.fast = mean(speed == "Fast")) %>% 
  arrange(pct.on_pace) %>% 
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
