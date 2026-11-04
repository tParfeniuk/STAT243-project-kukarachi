library(tidyverse)
library(Metrics)
df_raw <- read_csv("C:/Users/user/Downloads/student-por.csv", show_col_types = FALSE)
#we filter out missing students
df_clean<-df_raw %>%  
  mutate(raw_id=row_number()) %>%
  filter(G3>0&G1>0)
#color palette
palette_kse <- c(
  navy= "#1B365D",coral= "#D9534F",teal= "#2A9D8F",grey= "#8D99AE"  
)
theme_kse <- theme_minimal(base_size = 13) + 
  theme(
    plot.title= element_text(face ="bold",size =15,color= "#1B365D"),
    plot.subtitle= element_text(size = 11,color="#555555"),
    plot.caption= element_text(size = 9,color="#8D99AE", hjust = 0),
    legend.position= "top",
    legend.title= element_text(face="bold")
  )
# 3. linear regression
lm_traj<-lm(G3~G1+G2,data= df_clean)
names(lm_traj)
threshold_3s<-3*sd(lm_traj$residuals)# 3-sigma threshold
#Save fitted values and residuals, and determine outlier status
df_clean <- df_clean %>%
mutate(
  fitted= lm_traj$fitted.values,
  resid = lm_traj$residuals,
  outlier_status = case_when(
  resid > threshold_3s~ "Progress",
  resid < -threshold_3s~ "Regression",
  TRUE~ "Standard"
)
)
#outlier check
#outlier_5 <- df_clean %>%
  #filter(abs(resid) > threshold_3s) %>%
  #select(raw_id,school,sex,age,absences,failures,Dalc,G1,G2,G3,fitted,resid) %>%
  #mutate(
   #fitted= round(fitted, 2), 
   #resid= round(resid, 2)   
  #)%>%
  #arrange(desc(resid))         
#print(outlier_5)
#-----------------------------------------------------------------------------------------
# graph Fitted vs Actual
p_slide10 <- ggplot(df_clean, aes(x = fitted, y = G3, color = outlier_status)) +
  #Plotting data points, the ideal prediction line, and limit corridors
  geom_point(aes(size = abs(resid) > threshold_3s), alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "#1B365D", linewidth = 1) +
  geom_abline(slope = 1, intercept = threshold_3s, color = "#D9534F", linetype= "dashed", linewidth= 0.8) +
  geom_abline(slope = 1, intercept = -threshold_3s, color = "#D9534F", linetype= "dashed", linewidth= 0.8) +
 #labels anomalous points
geom_text(
  data = df_clean %>% filter(abs(resid) > threshold_3s),
  aes(label = paste0("#", raw_id, " (", G1, "->", G2, "->", G3, ")")),
  vjust = -0.9, size = 3.3, fontface = "bold", show.legend = FALSE#configuring anomalies on the chart
) +
#coloring points and configuring the legend
scale_color_manual(
name = NULL,
values = c(
  "Standard"="#8D99AE",
  "Progress"="#2A9D8F",
  "Regression"="#D9534F"
)
)+
#formatting the first graph
scale_size_manual(values= c("FALSE"= 2,"TRUE" =4),guide ="none")+
labs(
  title= "Trajectory Model G1 + G2",
  subtitle= "Fitted vs Actual",
  x= "Predicted annual score",
  y= "Actual annual score",
)+
theme_kse
print(p_slide10)
ggsave("trajectory_fitted_vs_actual.png",plot = p_slide10,width = 10,height = 6, dpi = 300)
#----------------------------------------------------------------------------------
#Graph Density of the distribution of residuals(Diagnostics of residues)
p_slide11_density<- ggplot(df_clean, aes(x = resid)) +
#the construction of the probability density curve
geom_density(fill= "#1B365D",color = "#1B365D",alpha = 0.35,linewidth = 1) +
#Gaussian curve
stat_function(
fun= dnorm, 
args= list(mean=mean(df_clean$resid),sd=sd(df_clean$resid)), 
color="#2A9D8F",linewidth=1.2
)+
geom_vline(xintercept= 0, color= "#1B365D",linewidth = 1) +
geom_vline(xintercept= c(-threshold_3s,threshold_3s),color = "#D9534F",linetype = "dashed",linewidth = 1) +
#construction of limit values
annotate("text",x =threshold_3s+0.3,y =0.35,label="+2.79",color= "#D9534F",fontface= "bold",hjust= 0) +
annotate("text",x =-threshold_3s-0.3,y =0.35,label="-2.79",color= "#D9534F",fontface= "bold",hjust= 1) +
labs(
title= "Diagnostics of residues",
subtitle= "Residuals =AG3-FG3",
x="Residual",
y="Density",
)+
theme_kse
print(p_slide11_density)
ggsave("residuals_density_3sigma.png",plot= p_slide11_density,width= 10,height= 6,dpi= 300)