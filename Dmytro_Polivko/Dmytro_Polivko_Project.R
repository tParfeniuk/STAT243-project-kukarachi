library(tidyverse)
library(Metrics)

df_raw <- read_csv("student-por.csv")
nrow(df_raw)
ncol(df_raw)

### PART 1: Hidden dropouts is zero grades that are not real grades

#Count all empty N/A cells in the whole table
sum(is.na(df_raw))

#Show all suspicious students who have a zero in G1 or G3
df_raw %>% filter(G1 == 0 | G3 == 0) %>% select(school, sex, age, address, absences, G1, G2, G3)

#Count in which school the 15 students with G3 = 0 
df_raw %>% filter(G3 == 0) %>% count(school)

#Check absences of these 15 students
df_raw %>% filter(G3 == 0) %>% pull(absences) %>% summary()

#Create the clean dataset: keep only students with both G1 and G3 above zero
df_clean <- df_raw %>% filter(G1 > 0 & G3 > 0)
#check the size of the clean data (633 rows, 16 removed)
nrow(df_clean)
nrow(df_raw) - nrow(df_clean)

#Compare mean, sd and median of the final grade before and after cleaning
df_raw %>% summarize(mean_G3 = mean(G3), sd_G3 = sd(G3), median_G3 = median(G3))
df_clean %>% summarize(mean_G3 = mean(G3), sd_G3 = sd(G3), median_G3 = median(G3))


theme_kse <- theme_minimal(base_size = 13) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1B365D"),
    plot.subtitle   = element_text(size = 11, color = "#555555"),
    legend.position = "top"
  )


### PART 2: Absences - asymmetry, IQR outliers and log10

#Descriptive statistics of absences on the clean data
df_clean %>% summarise(
  mean_absences = mean(absences),
  median_absences = median(absences),
  sd_absences = sd(absences),
  q1_absences = quantile(absences, 0.25),
  q3_absences = quantile(absences, 0.75),
  iqr_absences = IQR(absences),
  #Outlier threshold by the IQR rule: Q3 + 1.5 * IQR = 15
  upper_absences = q3_absences + 1.5 * iqr_absences,
  max_absences = max(absences)
) %>% as.data.frame()

#Count students with zero absences, with at least one absence and outliers above 15
df_clean %>% filter(absences == 0) %>% nrow()
df_clean %>% filter(absences > 0) %>% nrow()
df_clean %>% filter(absences > 15) %>% nrow()


### PLOT1 : Histogram of raw absences with median, mean and outlier threshold

p_abs_raw <- ggplot(df_clean, aes(x = absences)) +
  geom_histogram(binwidth = 2, fill = "#1B365D", color = "white") +
  #Dashed vertical lines: median teal, mean grey and outlier threshold red
  geom_vline(xintercept = 2,    linetype = "dashed", color = "#2A9D8F", linewidth = 1) +
  geom_vline(xintercept = 3.75, linetype = "dashed", color = "#8D99AE", linewidth = 1) +
  geom_vline(xintercept = 15,   linetype = "dashed", color = "#D9534F", linewidth = 1) +
  #Set the labels and theme
  theme_kse + labs(
    title = "Histogram of absences",
    x = "Number of missed classes per year",
    y = "Number of students"
)
p_abs_raw
#Save the plot as png
ggsave("d1_absences_raw.png", plot = p_abs_raw, width = 10, height = 6, dpi = 300)

#Keep only students with at least one absence and create log column
df_abs_pos <- df_clean %>% filter(absences > 0) %>%
  mutate(log_absences = log10(absences))
nrow(df_abs_pos)

#Statistics of log absences, mean and medium are almost equal, the asymmetry disappeared.
df_abs_pos %>% summarise(
  mean_log_abs = mean(log_absences),
  median_log_abs = median(log_absences),
  sd_log_abs = sd(log_absences)
) %>% as.data.frame()


### PLOT 2: Histogram of log10(absences)

p_abs_log <- ggplot(df_abs_pos, aes(x = log_absences)) +
  geom_histogram(bins = 12, fill = "#1B365D", color = "white") +
  #Set the labels and theme
  theme_kse +
  labs(
    title    = "Histogram of log10(absences)",
    x        = "log10(number of missed classes)",
    y        = "Number of students"
    )
p_abs_log
#Save the plot as png
ggsave("d2_absences_log.png", plot = p_abs_log, width = 10, height = 6, dpi = 300)


### PART 3: Age outliers by the IQR rule and the 3 sigma rule

#Descriptive statistics of age with two upper outlier thresholds
df_clean %>%
  summarise(
    mean_age     = mean(age),
    sd_age       = sd(age),
    median_age   = median(age),
    q1_age       = quantile(age, 0.25),
    q3_age       = quantile(age, 0.75),
    iqr_age      = IQR(age),
    #IQR rule: 18 + 1.5 * 2 = 21
    upper_iqr    = q3_age + 1.5 * iqr_age,
    #3 sigma rule: 16.72 + 3 * 1.21 = 20.36
    upper_3sigma = mean_age + 3 * sd_age
) %>%  as.data.frame()

#Students older than the 3 sigma threshold   
df_clean %>%
  filter(age > mean(age) + 3 * sd(age)) %>%
  select(school, sex, age, failures, Dalc, Walc, absences, G1, G2, G3)  

#Students older than the IQR threshold
df_clean %>%
  filter(age > quantile(age, 0.75) + 1.5 * IQR(age)) %>%
  select(school, sex, age, failures, Dalc, Walc, absences, G1, G2, G3)


### PLOT 3: Boxplot of age by gender

p_age <- ggplot(df_clean, aes(x = sex, y = age)) +
  #Semi-transparent boxes, outliers shown as big red dots
  geom_boxplot(fill = "#1B365D", alpha = 0.3, outlier.color = "#D9534F", outlier.size = 3, outlier.alpha = 1) +
  #Set the labels and theme
  theme_kse +
  labs(
    title    = "Boxplot age of students by gender",
    x        = "Sex (F = female, M = male)",
    y        = "Age, years",
   )
p_age
#Save the plot as png
ggsave("d3_age_boxplot.png", plot = p_age, width = 10, height = 6, dpi = 300)


### PART 4: Structural gap between schools GP and MS

#Create profile of each school: grades, share of rural students, no internet, parents' education, ambitions 
school_profile <- df_clean %>%
  group_by(school) %>% summarise(
    n = n(),
    mean_G3 = mean(G3),
    median_G3 = median(G3),
    rural_perc  = mean(address == "R") * 100,
    no_internet_perc = mean(internet == "no") * 100,
    mean_Medu = mean(Medu),
    mean_Fedu = mean(Fedu),
    higher_perc = mean(higher == "yes") * 100
  )
as.data.frame(school_profile)


### PLOT 4: Boxplot of final grade by school

p_school_g3 <- ggplot(df_clean, aes(x = school, y = G3, fill = school))+
  geom_boxplot(alpha = 0.7)+
  #Set school colors
  scale_fill_manual(values = c(GP = "#1B365D", MS = "#D9534F")) +
  theme_kse + 
  #Remove the legend
  theme(legend.position = "none") + 
  #Set the labels
  labs(
    title = "Final grade by school",
    x = "School",
    y = "Final grade G3"
  )
p_school_g3
#Save the plot as png
ggsave("d4_school_g3.png", plot = p_school_g3, width = 10, height = 6, dpi = 300) 


### PLOT 5: Social profile of students in GP vs MS

p_school_demo <- school_profile %>%
  #Keep only the school and three percentage indicators
  select(school, rural_perc, no_internet_perc, higher_perc) %>%
  #Convert to long format
  pivot_longer(cols = c(rural_perc, no_internet_perc, higher_perc), names_to = "indicator", values_to = "percent") %>%
  #create bar plot
  ggplot(aes(x = indicator, y = percent, fill = school)) + geom_col(position = "dodge") +
  #Set the colors, readable axis labels and theme
  scale_fill_manual(values = c(GP = "#1B365D", MS = "#D9534F"))+
  scale_x_discrete(labels = c(higher_perc = "Want higher education",
                              no_internet_perc = "No internet at home",
                              rural_perc = "Live in rural area"))+
  theme_kse + labs(
    title = "Social profile of students by school",
    x = NULL, 
    y = "Share of student, %",
    fill = "School"
  )
p_school_demo
#Save the plot as png
ggsave("d5_school_profile.png", plot = p_school_demo, width = 10, height = 6, dpi = 300)



