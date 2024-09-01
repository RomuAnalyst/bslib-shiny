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


o1 <- orientation %>% filter(REGION != "") %>% mutate(MOIS_ANNEE = paste(ANNEE,(sprintf("%02d",MOIS)))) %>% mutate(date = as.Date(paste(ANNEE,sprintf("%02d",MOIS),"01", sep = "-")))
table_orient <- o1 %>% group_by(ANNEE,MOIS,LIB_MOIS,date) %>% arrange (ANNEE, MOIS) %>% summarise(Orientés=n())
a1 <- p_charge %>% mutate(REGION = coalesce(REGION, ""),
                         MOTIF_FIN_ACC = coalesce(MOTIF_FIN_ACC, "")) %>%
  filter(REGION != "" &
           (MOTIF_FIN_ACC == "Accompagnement terminé" |
              MOTIF_FIN_ACC == "")) %>% mutate(MOIS_ANNEE = paste(ANNEE,(sprintf("%02d",MOIS)))) %>% mutate(date = as.Date(paste(ANNEE,sprintf("%02d",MOIS),"01", sep = "-")))
table_acc <- a1 %>% group_by(ANNEE,MOIS,LIB_MOIS,date) %>% arrange (ANNEE, MOIS) %>% summarise(Accompagnés=n())
table_join <- table_orient %>% left_join(table_acc)


copie_table <- table_join
t3 <- table_join %>% mutate(V.acc = sum(Volume_orient)) %>% select(ANNEE, V.acc) %>% group_by(ANNEE, V.acc)

t2$Volume <- as.numeric(t2$Volume)


ggplot(t, aes(x = factor(MOIS, levels = 1:12), y = Volume, fill = factor(ANNEE))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Mois", y = "Volume", fill = "Année") +
  scale_fill_manual(values = c("#FFFF66", "#66CCFF", "#FF6666")) +  # Couleurs par année
  scale_x_discrete(labels = t$LIB_MOIS[1:12]) +  # Noms des mois en fonction des n° de mois
  theme_classic()


fig <- plot_ly(t, x = ~MOIS, y = ~Volume, color = ~factor(ANNEE), type = 'scatter', mode = 'lines') %>%
  layout(title = "Volumes par Mois et Année",
         xaxis = list(title = "Mois"),
         yaxis = list(title = "Volume"),
         hovermode = "closest")

fig

library(plotly)

plot_ly(t, x = ~factor(MOIS, levels = 1:12), y = ~Volume, color = ~factor(ANNEE)) %>%
  add_bars() %>%
  layout(xaxis = list(title = "Mois", ticktext = t$LIB_MOIS[1:12], tickvals = 1:12),
         yaxis = list(title = "Volume"),
         legend = list(title = "Année")) %>%
  layout(barmode = "group")


library(plotly)

plot_ly(table_join, x = ~factor(MOIS, levels = 1:12), y = ~Accompagnés, color = ~factor(ANNEE)) %>%
  add_lines() %>%
  layout(xaxis = list(title = "Mois", ticktext = t$LIB_MOIS[1:12], tickvals = 1:12),
         yaxis = list(title = "Volume"),
         legend = list(title = "Année"))

library(plotly)

# Création de données aléatoires pour l'exemple
data <- data.frame(x = 1:10, y = rnorm(10))

# Création du graphique interactif avec une courbe
plot_ly(t2022, x = ~LIB_MOIS, y = ~Orientés, type = 'scatter', mode = 'lines', color = ~factor(ANNEE))
t2022 <- table_join %>% filter(ANNEE == "2022")


plot_ly(table_join, x = ~MOIS, y = ~Orientés, color = ~factor(ANNEE), type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Mois", ticktext = t$LIB_MOIS[1:12], tickvals = 1:12),
         yaxis = list(title = "Volume"),
         legend = list(title = "Année"))


plot_ly(table_acc, x = ~MOIS, y = ~Accompagnés) %>% add_lines(y = ~Accompagnés)

p <- plot_ly(palmerpenguins::penguins, x = ~bill_length_mm, y = ~body_mass_g)
add_markers(p, color = ~bill_depth_mm, size = ~bill_depth_mm)
add_markers(p, color = ~species)
add_markers(p, color = ~species, colors = "Set1")
add_markers(p, symbol = ~species)
add_paths(p, linetype = ~species)

p
