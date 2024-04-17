library(tidyverse)
library(ggplot2)
library(plotly)



orientation <- readxl::read_xlsx("datas/Fusion_orientations.xlsx")
p_charge <- readxl::read_xlsx("datas/FUSION_P_CHARGE.xlsx")


orientation %>% filter(REGION != "") %>%
  group_by(ANNEE) %>%
  summarise(n = n()) %>%
  write_rds(file = "datas/df_orientation.rds")


p_charge %>%
  mutate(REGION = coalesce(REGION, ""),
         MOTIF_FIN_ACC = coalesce(MOTIF_FIN_ACC, "")) %>%
  filter(REGION != "" &
           (MOTIF_FIN_ACC == "Accompagnement terminé" |
              MOTIF_FIN_ACC == "")) %>%
  group_by(ANNEE) %>%
  summarise(n = n()) %>%
  write_rds(file = "datas/df_p_charge.rds")


r <- orientation %>% mutate(MOIS_ANNEE = paste(ANNEE,(sprintf("%02d",MOIS)))) %>% mutate(date = as.Date(paste(ANNEE,sprintf("%02d",MOIS),"01", sep = "-")))
t <- r %>% group_by(ANNEE,MOIS,LIB_MOIS,date) %>% arrange (ANNEE, MOIS) %>% summarise(Volume=n())
t2 <- t
t2$Volume <- as.numeric(t2$Volume)


ggplot(t, aes(x = factor(MOIS, levels = 1:12), y = Volume, fill = factor(ANNEE))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Mois", y = "Volume", fill = "Année") +
  scale_fill_manual(values = c("#FFFF66", "#66CCFF", "#FF6666")) +  # Couleurs par année
  scale_x_discrete(labels = t$LIB_MOIS[1:12]) +  # Noms des mois
  theme_minimal()


fig <- plot_ly(t, x = ~MOIS, y = ~Volume, color = ~factor(ANNEE), type = 'scatter', mode = 'lines') %>%
  layout(title = "Volumes par Mois et Année",
         xaxis = list(title = "Mois"),
         yaxis = list(title = "Volume"),
         hovermode = "closest")

fig
