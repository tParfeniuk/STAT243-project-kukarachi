library(tidyverse)

df_raw <- read_csv("student-por.csv")
df_clean <- df_raw %>%
  filter(G3 > 0 & G1 > 0)

theme_kse <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1B365D"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    legend.position = "top"
  )

### PLOT 1 : Temperature map, visualising average grades in different study time - mother education groups ###
#filter positive Medu values
plot_t1 <- df_clean %>% 
  filter(Medu > 0) %>%
  #give understandable labels to the numeric data 
  mutate(studytime = factor(studytime, levels = c(1,2,3,4), labels = c('<2 hrs','2-5 hrs','5-10 hrs','>10 hrs')),
         Medu = factor(Medu, levels = c(1,2,3,4), labels = c('Primary', 'Secondary', 'Upper-secondary', 'Higher')) 
  ) %>%
  #create column for average grade per Medu-studytime combination 
  group_by(Medu,studytime) %>% 
  summarise(meanG3 = mean(G3), .groups = 'drop') %>% 
  #create the temperature map plot 
  ggplot(aes(studytime,Medu, fill = meanG3))+
  geom_tile(color = 'white',linewidth = 1) + 
  #set the labels, gradient colors and theme
  geom_text(
    aes(label = paste0(round(meanG3,2))),
    color = 'white',fontface = 'bold',size = 4
  )+
  scale_fill_gradient2(
    low = "#D9534F", mid = "#8D99AE", high = "#2A9D8F", midpoint = 12.2, name = "Average G3"
  )+
  labs(x = 'Time of student individual study',
       y = 'Mother education',
       title = 'Discipline vs crouch start')+
  theme_kse
#save the plot as png
ggsave("t1_social_lift.png", plot = plot_t1, width = 10, height = 6, dpi = 300)

### PLOT 2 : Alcohol affect on male VS female###
plot_t2 <- df_clean %>% 
  #create columns for average grade and amount of students per gender-alcohol consumption combination
  group_by(sex, Dalc) %>% 
  summarise(
    mean_G3 = mean(G3),
    n = n(),
    .groups = 'drop'
  ) %>% 
  #create line graph plot with points on refractions
  ggplot(aes(factor(Dalc),mean_G3,color = sex, group = sex))+
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  #set the labels, scale, colors and theme
  geom_text(aes(label = paste0(round(mean_G3, 2), "\n", n), vjust = ifelse(sex == "M", 1.5, -1)), size = 3.5, fontface = "bold", show.legend = FALSE)+
  scale_color_manual(
    values = c("M" = "#1B365D", "F" = "#D9534F"),
    labels = c("M" = "Male", "F" = "Female"),
    name = "Sex:"
  )+ 
  scale_y_continuous(limits = c(9, 14.5))+
  labs(
    title = "Alcohol affect on performance",
    subtitle = "Male vs Female",
    x = "Workday alcohol consumption",
    y = "Average G3"
  )+
  theme_kse
#save the plot as png
ggsave("t2_alcohol affect.png", plot = plot_t2, width = 10, height = 6, dpi = 300)

### PLOT 3 : average grade per term for student attending VS not attending extra support classes ###
plot_t3 <- df_clean %>% 
  #create columns for term average grades per schoolsup option (yes or no)
  group_by(schoolsup) %>% 
  summarise(
    mean_G1 = mean(G1),
    mean_G2 = mean(G2),
    mean_G3 = mean(G3),
    .groups = "drop"
  ) %>% 
  #convert to long format
  pivot_longer(
    cols = c(mean_G1,mean_G2,mean_G3),
    names_to = 'term',
    values_to = 'grade'
  ) %>% 
  #label the column names for clarity
  mutate(term = factor(term,levels = c("mean_G1", "mean_G2", "mean_G3"), labels = c('1st term','2nd term','3rd term'))) %>% 
  #create line graph with points on refractions
  ggplot(aes(term,grade,color = schoolsup, group = schoolsup))+
  geom_line(linewidth = 1.2)+
  geom_point(size = 4)+
  #set the labels, scale, colors and theme
  geom_text(aes(label = round(grade, 2)), vjust = -1.0, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    values = c("yes" = "#1B365D", "no" = "#D9534F"),
    name = "Additional school support:"
  )+
  scale_y_continuous(limits = c(11, 13))+
  labs(
    title = "Average grade per term",
    subtitle = "Support vs no support",
    x = "Term",
    y = "Grade"
  )+
  theme_kse
#save the plot as png
ggsave("t3_schoolsupport.png", plot = plot_t3, width = 10, height = 6, dpi = 300) 

### PLOT 4 : complex facet_grid plot of 4, featuring term 3 to 1 grade regression, with dots based on absence ###
plot_t4 <- df_clean %>% 
  mutate(
    school_label = ifelse(school == "GP", "Urban school", "Rural school"),
    address_label = ifelse(address == "U", "Urban living", "Rural living")
  ) %>% 
  #create vertical and horizontal lines on grades to pass, jitter plot (dot width based on absence), and 
  ggplot(aes(x = G1, y = G3, color = higher)) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#8D99AE") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "#8D99AE") +
  geom_jitter(aes(size = absences), alpha = 0.6, width = 0.25, height = 0.25) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  facet_grid(school_label ~ address_label)+
  #set the labels, scale, colors and theme
  labs(
    title = "Motivation to study higher education",
    subtitle = "Rural vs Urban living address/school",
    x = 'Term 1 Grade',
    y = 'term 3 Grade'
  )+
  scale_color_manual(
    values = c("yes" = "#1B365D", "no" = "#D9534F"),
    name = "Planning higher education:"
    )+
  theme_kse
#save the plot as png
ggsave("t4_motivation.png", plot = plot_t4, width = 10, height = 6, dpi = 300)

