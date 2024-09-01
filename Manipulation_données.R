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

t2$Volume <- as.numeric(t2$Volume)
