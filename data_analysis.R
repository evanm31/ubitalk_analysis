require(googlesheets)
require(tidyverse)
require(ggpubr)

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

user_data <- ubitalk[1:7, 1:10] %>% 
  filter(Name != "Katie House") %>% 
  mutate(control_time = ifelse(control_min == 1, 60 + control_sec, control_sec),
        treatment_time = ifelse(treatment_min == 1, 60 + treatment_sec, treatment_sec),
        wpm_S = (60/S_sec)*len_p2,
        wpm_N = (60/N_sec)*len_p2,
        wpm_F = (60/F_sec)*len_p2,
        wpm_treatment = ifelse(treatment_time <= 60, (treatment_time/60)*len_p1, (60/treatment_time)*len_p1),
        wpm_control = ifelse(control_time <= 60, (control_time/60)*len_p1, (60/control_time)*len_p1)
  ) %>% 
  select("Fast" = wpm_F, "Normal" = wpm_N, "Slow" = wpm_S, everything(), -c(treatment_min, treatment_sec, control_min, control_sec))

app_data <- read_csv("data/final_data.csv")

#VISUALIZATIONS 

#USER DATA 
#speaking speed
#fix anova location
p <- user_data %>% 
  gather("speed", "wpm", 1:3) %>% 
  ggboxplot(x = "speed", y = "wpm",
            color = "speed", palette = "jco") +
  stat_compare_means(method = "anova")
ggpar(p, 
      legend = "none",
      main = "WPM by Speech Time",
      xlab = "Speech Speed",
      ylab = "Words Per Minute")

#treatment vs.control WPM
p <- user_data %>% 
  gather("group", "wpm", 12:13) %>% 
  ggboxplot(x = "group", y = "wpm",
            color = "group", palette = "jco") +
  stat_compare_means(method = "t.test")
ggpar(p, 
      legend = "none",
      main = "WPM by Group",
      xlab = "Group",
      ylab = "Words Per Minute")

#APP DATA 
  