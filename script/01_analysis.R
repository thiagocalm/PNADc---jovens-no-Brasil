##### ANALISES

# Bibliotecas -------------------------------------------------------------

library(patchwork)
library(survey)
library(srvyr)
library(convey)
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))
ifelse(!require(reactable),install.packages("reactable"),require(reactable))
library(ggrepel)


# Importação das bases ----------------------------------------------------

pnad_15 <- readRDS("./data/pnadc_15.rds")
pnad_16 <- readRDS("./data/pnadc_16.rds")
pnad_17 <- readRDS("./data/pnadc_17.rds")
pnad_18 <- readRDS("./data/pnadc_18.rds")
pnad_19 <- readRDS("./data/pnadc_19.rds")
pnad_20 <- readRDS("./data/pnadc_20.rds")
pnad_21 <- readRDS("./data/pnadc_21.rds")

# Aplicacao de plano amostral complexo  -------------------------------------

# 2015
pnadc_15_plan <- pnad_15 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2016
pnadc_16_plan <- pnad_16 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2017
pnadc_17_plan <- pnad_17 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2018
pnadc_18_plan <- pnad_18 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2019
pnadc_19_plan <- pnad_19 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2020
pnadc_20_plan <- pnad_20 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

# 2021
pnadc_21_plan <- pnad_21 %>%
  mutate(
    fl_jovem = ifelse(V2009 >= 14 & V2009 <= 29, 1, 0),
    fl_14a17 = ifelse(V2009 >= 14 & V2009 <= 17, 1, 0),
    fl_18a24 = ifelse(V2009 >= 18 & V2009 <= 24, 1, 0),
    fl_25a29 = ifelse(V2009 >= 25 & V2009 <= 29, 1, 0),
    fl_ocupado = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 1, 1, 0)),
    fl_desocup = ifelse(is.na(VD4002), 0, ifelse(VD4002 == 2, 1, 0)),
    fl_inativo = ifelse(is.na(VD4001), 0, ifelse(VD4001 == 2, 1, 0)),
    fl_estudando = ifelse(is.na(V3002), 0, ifelse(V3002 == 1, 1, 0)),
    fl_informal = ifelse( (VD4009 == 2 | VD4009 == 4 | VD4009 == 10) |
                            ((VD4009 == 8 | VD4009 == 9) &
                               V4019 == 2), 1, 0),
    fl_chefe = ifelse((VD2002 == 1 | VD2002 == 2), 1, 0)
  ) %>% as_survey_design(ids = id_pes,
                         weights = V1028)

rm("pnad_15","pnad_16","pnad_17","pnad_18","pnad_19","pnad_20","pnad_21")

# confer?ncia

pnadc_15_plan %>%
  group_by(fl_14a17) %>%
  summarise(n = survey_total()/1000)

pnadc_21_plan %>%
  group_by(fl_informal) %>%
  summarise(n = survey_total())


# 1 - Jovens por situacao ocupacional (total e por grupo et?rio) ---------------

pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    situacao_ocupacional = case_when(
      fl_ocupado == 1 ~ 'Ocupado',
      fl_desocup == 1 ~ 'Desocupado',
      TRUE ~ 'Inativo')
  ) %>%
  group_by(situacao_ocupacional) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          TRUE ~ 'Inativo')
      ) %>%
      group_by(situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, situacao_ocupacional, prop) %>%
  arrange(ordem, situacao_ocupacional) %>%
  ggplot() +
  aes(x=ordem, y=prop, linetype = situacao_ocupacional, shape = situacao_ocupacional,
      group = interaction(situacao_ocupacional, situacao_ocupacional)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = 1.1) +
  geom_point(size = 4, alpha = 1) +
  #scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  scale_y_continuous(breaks = c(seq(0,60,10))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 12,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 11,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 12,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey"))




  #reactable(columns = list(
  #  ordem = colDef(name = "Ano"),
  #  idade_jovem = colDef(name = "Idade do jovem")
  #  ),
  #columnGroups = list(
  #  colGroup(
  #    name = "Status Ocupacional",
  #    columns = c(
  #      "Desocupado",
  #      "Inativo",
  #      "Ocupado")
  #  )
  #),
  #striped = TRUE,
  #highlight = TRUE,
  #compact = TRUE
  #)



# 2 - Jovens por situacao de estudo (por grupo et?rio) -----------------------------

pnadc_15_plan %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      fl_25a29 == 1 ~ '25a29',
      fl_jovem == 1 ~ 'Total',
      TRUE ~ 'Total'),
    situacao_estudo = case_when(
      fl_estudando == 1 ~ 'Estudando',
      TRUE ~ 'Nao_estudando')
  ) %>%
  group_by(idade_jovem, situacao_estudo) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  spread(situacao_estudo,prop) %>%
  bind_rows(
    pnadc_16_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao_estudando')
      ) %>%
      group_by(idade_jovem, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont) %>%
      spread(situacao_estudo,prop)
  ) %>% View()
  #select(ordem, idade_jovem, situacao_estudo,prop) %>%
  #arrange(ordem, idade_jovem, situacao_estudo)



# 3 - Jovens por status ocupacional e de estudo (total e grupo etario) --------

pnadc_15_plan %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      fl_25a29 == 1 ~ '25a29',
      fl_jovem == 1 ~ 'Total',
      TRUE ~ 'Total'),
    situacao_jovem = case_when(
      fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
      fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
      fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
      fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
      VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
      VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
  ) %>%
  group_by(idade_jovem, situacao_jovem) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          fl_25a29 == 1 ~ '25a29',
          fl_jovem == 1 ~ 'Total',
          TRUE ~ 'Total'),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
      ) %>%
      group_by(idade_jovem, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, situacao_jovem, prop) %>%
  arrange(ordem, idade_jovem, situacao_jovem) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_jovem, shape = situacao_jovem,
      group = interaction(situacao_jovem, situacao_jovem)) +
  geom_vline(xintercept = c("2019","2021"), size = .8, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "B",end = .9) +
  facet_grid(.~idade_jovem) +
  theme_minimal() +
  theme(axis.text.x = element_text(colour = 'black', angle = 90,
                                   size = 9, hjust = 0.5, vjust =
                                     0.5),
        axis.title.x=element_blank(),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))

# DEBUG: T? dando uma parcela que n?o t? se encaixando em nenhuma categoria para todos os anos.
# acho que ? problema no "case_when()"


# 4.1 - Jovens por tipo de domicilio (total) -----------------

(plot4.1 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto"))
  ) %>%
  group_by(tipo_dom) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, tipo_dom, prop) %>%
  arrange(ordem, tipo_dom) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=tipo_dom,
      group = interaction(tipo_dom, tipo_dom)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2, alpha = 1) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom"))


# 4.2 - Jovens por tipo de domicilio (grupo etario) -----------------

(plot4.2 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto"))
  ) %>%
  group_by(idade_jovem, tipo_dom) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto"))
      ) %>%
      group_by(idade_jovem, tipo_dom) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, tipo_dom, prop) %>%
  arrange(ordem, idade_jovem, tipo_dom) %>%
  ggplot() +
  aes(x=ordem, y=prop, linetype = tipo_dom, shape = tipo_dom,
      group = interaction(tipo_dom, tipo_dom)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = 1) +
  geom_point(size = 4, alpha = 1) +
  #scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(.~idade_jovem) +
  scale_y_continuous(breaks = c(seq(0,65,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 12,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 11,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 12,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey")))


# 5.1 - Jovens por tipo de dom. e situacao ocupacional (total) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot5.1 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_ocupacional = case_when(
      fl_ocupado == 1 ~ 'Ocupado',
      fl_desocup == 1 ~ 'Desocupado',
      fl_inativo == 1 ~ 'Inativo',
      TRUE ~ 'Total')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal") %>%
  group_by(tipo_dom, situacao_ocupacional) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, tipo_dom, situacao_ocupacional, prop) %>%
  arrange(ordem, tipo_dom, situacao_ocupacional) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_ocupacional,
      group = interaction(situacao_ocupacional, situacao_ocupacional)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2, alpha = 1) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(. ~ tipo_dom) +
  scale_y_continuous(breaks = c(seq(0,80,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        strip.background = element_blank()))

# 5.2 - Jovens por tipo de dom. e situacao ocupacional (grupo etario) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot5.2 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_ocupacional = case_when(
      fl_ocupado == 1 ~ 'Ocupado',
      fl_desocup == 1 ~ 'Desocupado',
      fl_inativo == 1 ~ 'Inativo',
      TRUE ~ 'Total')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal") %>%
  group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_ocupacional = case_when(
          fl_ocupado == 1 ~ 'Ocupado',
          fl_desocup == 1 ~ 'Desocupado',
          fl_inativo == 1 ~ 'Inativo',
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_ocupacional) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, tipo_dom, situacao_ocupacional, prop) %>%
  arrange(ordem, idade_jovem,tipo_dom, situacao_ocupacional) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_ocupacional,
      group = interaction(situacao_ocupacional, situacao_ocupacional)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2, alpha = 1) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(idade_jovem ~ tipo_dom) +
  scale_y_continuous(breaks = c(seq(0,80,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold'),
        strip.text.y = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 8,
                                   hjust = 0.5,
                                   vjust = 0.5, colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        strip.background = element_blank()))

plot5.2/plot5.1

# 6.1 - Jovens por tipo de dom. e situacao de estudo (total) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot6.1 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("Unipessoal",
                        "Casal sem filho",
                        "Casal 1+ filhos",
                        "Monoparental 1+ filhos",
                        "Estendida","Composto")),
    situacao_estudo = case_when(
      fl_estudando == 1 ~ 'Estudando',
      TRUE ~ 'Nao estudando')
  ) %>%
  filter(tipo_dom != "Composto" &
           tipo_dom != "Unipessoal") %>%
  group_by(tipo_dom, situacao_estudo) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, tipo_dom, situacao_estudo, prop) %>%
  arrange(ordem, tipo_dom, situacao_estudo) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_estudo,
      group = interaction(situacao_estudo, situacao_estudo)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2, alpha = 1) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(. ~ tipo_dom) +
  scale_y_continuous(breaks = c(seq(0,90,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        strip.background = element_blank()))

# 6.2 - Jovens por tipo de dom. e situacao de estudo (grupo etario) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot6.2 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("Unipessoal",
                        "Casal sem filho",
                        "Casal 1+ filhos",
                        "Monoparental 1+ filhos",
                        "Estendida","Composto")),
    situacao_estudo = case_when(
      fl_estudando == 1 ~ 'Estudando',
      TRUE ~ 'Nao estudando')
  ) %>%
  filter(tipo_dom != "Composto" &
           tipo_dom != "Unipessoal") %>%
  group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("Unipessoal",
                            "Casal sem filho",
                            "Casal 1+ filhos",
                            "Monoparental 1+ filhos",
                            "Estendida","Composto")),
        situacao_estudo = case_when(
          fl_estudando == 1 ~ 'Estudando',
          TRUE ~ 'Nao estudando')
      ) %>%
      filter(tipo_dom != "Composto" &
               tipo_dom != "Unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_estudo) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem,  tipo_dom, situacao_estudo, prop) %>%
  arrange(ordem, idade_jovem, tipo_dom, situacao_estudo) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_estudo, shape = situacao_estudo,
      group = interaction(situacao_estudo, situacao_estudo)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 1, alpha = .7) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(idade_jovem ~ tipo_dom) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom"))

plot6.2/plot6.1

# 7.1 - Jovens por tipo de dom. e situacao ocup. e estudo (total) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot7.1 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_jovem = case_when(
      fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
      fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
      fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
      fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
      VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
      VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
      TRUE ~ 'Total')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal") %>%
  group_by(tipo_dom, situacao_jovem) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, tipo_dom, situacao_jovem, prop) %>%
  arrange(ordem, tipo_dom, situacao_jovem) %>%
  ggplot() +
  aes(x=ordem, y=prop, linetype = situacao_jovem, shape = situacao_jovem,
      group = interaction(situacao_jovem, situacao_jovem)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = 1) +
  geom_point(size = 4, alpha = 1) +
  #scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(. ~ tipo_dom) +
  scale_y_continuous(breaks = c(seq(0,75,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 12,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 11,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 12,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey")))

# 7.2 - Jovens por tipo de dom. e situacao ocup. e estudo (grupo etario) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

(plot7.2 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_jovem = case_when(
      fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
      fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
      fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
      fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
      VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
      VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
      TRUE ~ 'Total')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal") %>%
  group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal") %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, tipo_dom, situacao_jovem, prop) %>%
  arrange(ordem, idade_jovem, tipo_dom, situacao_jovem) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_jovem, shape = situacao_jovem,
      group = interaction(situacao_jovem, situacao_jovem)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 1, alpha = .7) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(idade_jovem ~ tipo_dom) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom"))

# 7.3 - Jovens (exceto 14-17anos) por tipo de dom. e situacao ocup. e estudo (grupo etario) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.
# Filtro 2 realizado: retirei o grupo et?rio 14-17anos.
# Motivo: para facilitar a an?lise.

(plot7.3 <- pnadc_15_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_jovem = case_when(
      fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
      fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
      fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
      fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
      VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
      VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
      TRUE ~ 'Total')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal" &
           idade_jovem != '14a17') %>%
  group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2015") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_16_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2016") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_jovem = case_when(
          fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
          fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
          fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
          fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
          VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
          VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando",
          TRUE ~ 'Total')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               idade_jovem != '14a17') %>%
      group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, tipo_dom, situacao_jovem, prop) %>%
  arrange(ordem, idade_jovem, tipo_dom, situacao_jovem) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_jovem, shape = situacao_jovem,
      group = interaction(situacao_jovem, situacao_jovem)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 1, alpha = .7) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(idade_jovem ~ tipo_dom) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom"))

plot7.3/plot7.1

# Rodar o gr?fico "7" pra 2o tri.

# 7.4 - Variacao no per?odo da pandemia por tipo de dom. e situacao ocup. e estudo (total) --------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.
# Filtro 2 realizado: retirei o grupo et?rio 14-17anos.
# Motivo: para facilitar a an?lise.

f_graf_7.2 <- function(df, ano) {
  df %>%
    filter(fl_jovem == 1) %>%
    mutate(
      idade_jovem = case_when(
        fl_14a17 == 1 ~ '14a17',
        fl_18a24 == 1 ~ '18a24',
        fl_25a29 == 1 ~ '25a29'),
      tipo_dom =
        factor(tipo_dom,
               levels = c(1,2,3,4,5,6),
               labels = c("unipessoal",
                          "casal sem filho",
                          "casal 1+ filhos",
                          "monoparental 1+ filhos",
                          "estendida","composto")),
      situacao_jovem = case_when(
        fl_ocupado == 1 & fl_estudando == 1 ~ "ocupado estudando",
        fl_ocupado == 1 & fl_estudando == 0 ~ "ocupado n?o estudando",
        fl_desocup == 1 & fl_estudando == 1 ~ "desocupado estudando",
        fl_desocup == 1 & fl_estudando == 0 ~ "desocupado n?o estudando",
        VD4001 == 2 & fl_estudando == 1 ~ "inativo estudando",
        VD4001 == 2 & fl_estudando == 0 ~ "inativo n?o estudando")
    ) %>%
    filter(idade_jovem != "14a17") %>%
    filter(tipo_dom != "composto" &
             tipo_dom != "unipessoal") %>%
    group_by(idade_jovem, tipo_dom, situacao_jovem) %>%
    summarise(cont = n()) %>%
    mutate(prop = round(cont/sum(cont),2)*100) %>%
    mutate(ordem = ano) %>%
    select(-cont)
}

map2(.x = list(pnadc_19_plan,
               pnadc_20_plan, pnadc_21_plan),
     .y = list("2019", "2020", "2021"),
     .f = function(x, y) f_graf_7.2(x, y)) %>%
  bind_rows() %>%
  select(ordem, idade_jovem, tipo_dom, situacao_jovem, prop) %>%
  arrange(ordem, idade_jovem, tipo_dom, situacao_jovem) %>%
  spread(ordem, prop) %>%
  mutate(var_20_19 = `2020` - `2019`,
         var_21_20 = `2021` - `2020`) %>%
  ggplot() +
  aes(x=var_20_19, y=var_21_20, colour=situacao_jovem, shape = situacao_jovem) +
  geom_abline(slope = -1, intercept = 0, size = .6, colour = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0, size = .6, colour = "red", linetype = "dashed") +
  #geom_hline(yintercept = 0, size = .6, colour = "red", linetype = "dashed") +
  geom_point(size = 2, alpha = 1) +
  #geom_text_repel(aes(label = `2020`)) +
  scale_color_viridis_d(option = "B", end = .8) +
  labs(
    x = "Varia??o 2020/2019 p.p.",
    y = "Varia??o 2021/2020 p.p."
  ) +
  scale_x_continuous(limits = c(-15,15), n.breaks = 10) +
  scale_y_continuous(limits = c(-15,15), n.breaks = 10) +
  facet_grid(idade_jovem ~ tipo_dom) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom")

# Rodar o gr?fico "7" pra 2o tri.

# 8.1 - Jovens por tipo de dom. e situacao ocupacional informal (total) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

# An?lise de 2016 a 2021.
# Motivo: disponibilidade de dados para o 3o trimestre sobre informalidade somente a partir do 40 trimestre.

pnadc_16_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_informal = case_when(
      fl_informal == 1 ~ 'Informal',
      fl_informal == 0 ~ 'Formal',
      TRUE ~ 'Nao_ocupado')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal" &
           situacao_informal != 'Nao_ocupado') %>%
  group_by(tipo_dom, situacao_informal) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2016") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, tipo_dom, situacao_informal, prop) %>%
  arrange(ordem, tipo_dom, situacao_informal) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_informal, shape = situacao_informal,
      group = interaction(situacao_informal, situacao_informal)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 1, alpha = .7) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(. ~ tipo_dom) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 9,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold',
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 9,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom")

# 8.2 - Jovens por tipo de dom. e situacao ocupacional informal (grupo etario) -----------------

# Filtros realizados: tipo de domic?lio somente de fam?lia domiciliar (sem unipessoal e composto)
# Motivo: ser uma parcela pequena.

pnadc_16_plan %>%
  filter(fl_jovem == 1) %>%
  mutate(
    idade_jovem = case_when(
      fl_14a17 == 1 ~ '14a17',
      fl_18a24 == 1 ~ '18a24',
      TRUE ~ '25a29'),
    tipo_dom =
      factor(tipo_dom,
             levels = c(1,2,3,4,5,6),
             labels = c("unipessoal",
                        "casal sem filho",
                        "casal 1+ filhos",
                        "monoparental 1+ filhos",
                        "estendida","composto")),
    situacao_informal = case_when(
      fl_informal == 1 ~ 'Informal',
      fl_informal == 0 ~ 'Formal',
      TRUE ~ 'Nao_ocupado')
  ) %>%
  filter(tipo_dom != "composto" &
           tipo_dom != "unipessoal" &
           situacao_informal != 'Nao_ocupado') %>%
  group_by(idade_jovem, tipo_dom, situacao_informal) %>%
  summarise(cont = n()) %>%
  mutate(prop = round(cont/sum(cont),2)*100) %>%
  mutate(ordem = "2016") %>%
  select(-cont) %>%
  bind_rows(
    pnadc_17_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(idade_jovem, tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2017") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_18_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(idade_jovem, tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2018") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_19_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(idade_jovem, tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2019") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_20_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(idade_jovem, tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2020") %>%
      select(-cont)
  ) %>%
  bind_rows(
    pnadc_21_plan %>%
      filter(fl_jovem == 1) %>%
      mutate(
        idade_jovem = case_when(
          fl_14a17 == 1 ~ '14a17',
          fl_18a24 == 1 ~ '18a24',
          TRUE ~ '25a29'),
        tipo_dom =
          factor(tipo_dom,
                 levels = c(1,2,3,4,5,6),
                 labels = c("unipessoal",
                            "casal sem filho",
                            "casal 1+ filhos",
                            "monoparental 1+ filhos",
                            "estendida","composto")),
        situacao_informal = case_when(
          fl_informal == 1 ~ 'Informal',
          fl_informal == 0 ~ 'Formal',
          TRUE ~ 'Nao_ocupado')
      ) %>%
      filter(tipo_dom != "composto" &
               tipo_dom != "unipessoal" &
               situacao_informal != 'Nao_ocupado') %>%
      group_by(idade_jovem, tipo_dom, situacao_informal) %>%
      summarise(cont = n()) %>%
      mutate(prop = round(cont/sum(cont),2)*100) %>%
      mutate(ordem = "2021") %>%
      select(-cont)
  ) %>%
  select(ordem, idade_jovem, tipo_dom, situacao_informal, prop) %>%
  arrange(ordem, idade_jovem,tipo_dom, situacao_informal) %>%
  ggplot() +
  aes(x=ordem, y=prop, colour=situacao_informal,
      group = interaction(situacao_informal, situacao_informal)) +
  geom_vline(xintercept = c("2019","2021"), size = .6, colour = "grey", linetype = "dashed") +
  geom_line(size = .8) +
  geom_point(size = 2, alpha = 1) +
  scale_color_viridis_d(option = "B",end = .8) +
  labs(
    x = "Per?odo",
    y = "%"
  ) +
  facet_grid(idade_jovem ~ tipo_dom) +
  scale_y_continuous(breaks = c(seq(0,80,10))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold'),
        strip.text.y = element_text(size = 10, hjust = 0.5,
                                    vjust = 0.5, face = 'bold')) +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 90,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   colour = 'black'),
        axis.title.x= element_text(size = 10,
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   face = 'bold', colour = 'black'),
        axis.text.y = element_text(size = 8,
                                   hjust = 0.5,
                                   vjust = 0.5, colour = 'black'),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.5,
                                    vjust = 0.5,
                                    face = 'bold', colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        strip.background = element_blank())


# 9 - Jovens por condi??o no domic?lio ("chefia"), grupo et?rio e tipo de domic?lio ----

# Agregado (14 a 29 anos):

pnadc_21_plan %>%
  filter(fl_chefe == 1) %>%
  filter(fl_jovem == 1) %>%
  group_by(tipo_dom) %>%
  summarise(n = survey_mean())

# 14 a 17 anos:

pnadc_21_plan %>%
  filter(fl_chefe == 1) %>%
  filter(fl_14a17 == 1) %>%
  group_by(tipo_dom) %>%
  summarise(n = survey_mean())

# 18 a 24 anos:

pnadc_21_plan %>%
  filter(fl_chefe == 1) %>%
  filter(fl_18a24 == 1) %>%
  group_by(tipo_dom) %>%
  summarise(n = survey_mean())

# 25 a 29 anos:

pnadc_21_plan %>%
  filter(fl_chefe == 1) %>%
  filter(fl_25a29 == 1) %>%
  group_by(tipo_dom) %>%
  summarise(n = survey_mean())

# grafico

(plot9 <- pnadc_15_plan %>%
    filter(fl_jovem == 1) %>%
    mutate(
      tipo_dom =
        factor(tipo_dom,
               levels = c(1,2,3,4,5,6),
               labels = c("unipessoal",
                          "casal sem filho",
                          "casal 1+ filhos",
                          "monoparental 1+ filhos",
                          "estendida","composto")),
      fl_chefe =
        factor(fl_chefe,
               levels = c(1,0),
               labels = c("Respons?vel ou c?njuge",
                          "N?o respons?vel"))
    ) %>%
    filter(tipo_dom != "composto" &
             tipo_dom != "unipessoal") %>%
    group_by(tipo_dom, fl_chefe) %>%
    summarise(cont = n()) %>%
    mutate(prop = round(cont/sum(cont),2)*100) %>%
    mutate(ordem = "2015") %>%
    select(-cont) %>%
    bind_rows(
      pnadc_16_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2016") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_17_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2017") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_18_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2018") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_19_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2019") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_20_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2020") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_21_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2021") %>%
        select(-cont)
    ) %>%
    select(ordem, tipo_dom, fl_chefe, prop) %>%
    arrange(ordem, tipo_dom, fl_chefe) %>%
    ggplot() +
    aes(x= forcats::fct_rev(factor(ordem)), y=prop, fill = fl_chefe,
        group = interaction(fl_chefe, fl_chefe)) +
    geom_col(aes(fill = fl_chefe)) +
    coord_flip() +
    scale_fill_grey(start = .3, end = .8) +
    labs(
      x = "Per?odo",
      y = "%"
    ) +
    facet_grid(. ~ tipo_dom) +
    scale_y_continuous(breaks = c(seq(0,100,10))) +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, hjust = 0.5,
                                      vjust = 0.5, face = 'bold')) +
    theme(axis.text.x = element_text(size = 10,
                                     angle = 90,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = 'bold',
                                     colour = 'black'),
          axis.title.x= element_text(size = 11,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = 'bold', colour = 'black'),
          axis.text.y = element_text(size = 10,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = 'bold', colour = 'black'),
          axis.title.y = element_text(size = 11,
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      face = 'bold', colour = 'black'),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          strip.background = element_blank(),
          panel.border = element_rect(colour = "grey")))

# grafico por grupo etario

(plot9.1 <- pnadc_15_plan %>%
    filter(fl_jovem == 1) %>%
    mutate(
      idade_jovem = case_when(
        fl_14a17 == 1 ~ '14 a 17',
        fl_18a24 == 1 ~ '18 a 24',
        TRUE ~ '25 a 29'),
      tipo_dom =
        factor(tipo_dom,
               levels = c(1,2,3,4,5,6),
               labels = c("unipessoal",
                          "casal sem filho",
                          "casal 1+ filhos",
                          "monoparental 1+ filhos",
                          "estendida","composto")),
      fl_chefe =
        factor(fl_chefe,
               levels = c(1,0),
               labels = c("Respons?vel ou c?njuge",
                          "N?o respons?vel"))
    ) %>%
    filter(tipo_dom != "composto" &
             tipo_dom != "unipessoal") %>%
    group_by(idade_jovem, tipo_dom, fl_chefe) %>%
    summarise(cont = n()) %>%
    mutate(prop = round(cont/sum(cont),2)*100) %>%
    mutate(ordem = "2015") %>%
    select(-cont) %>%
    bind_rows(
      pnadc_16_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2016") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_17_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2017") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_18_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2018") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_19_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2019") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_20_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2020") %>%
        select(-cont)
    ) %>%
    bind_rows(
      pnadc_21_plan %>%
        filter(fl_jovem == 1) %>%
        mutate(
          idade_jovem = case_when(
            fl_14a17 == 1 ~ '14 a 17',
            fl_18a24 == 1 ~ '18 a 24',
            TRUE ~ '25 a 29'),
          tipo_dom =
            factor(tipo_dom,
                   levels = c(1,2,3,4,5,6),
                   labels = c("unipessoal",
                              "casal sem filho",
                              "casal 1+ filhos",
                              "monoparental 1+ filhos",
                              "estendida","composto")),
          fl_chefe =
            factor(fl_chefe,
                   levels = c(1,0),
                   labels = c("Respons?vel ou c?njuge",
                              "N?o respons?vel"))
        ) %>%
        filter(tipo_dom != "composto" &
                 tipo_dom != "unipessoal") %>%
        group_by(idade_jovem, tipo_dom, fl_chefe) %>%
        summarise(cont = n()) %>%
        mutate(prop = round(cont/sum(cont),2)*100) %>%
        mutate(ordem = "2021") %>%
        select(-cont)
    ) %>%
    select(ordem, idade_jovem, tipo_dom, fl_chefe,  prop) %>%
    arrange(ordem, idade_jovem, tipo_dom, fl_chefe) %>%
    ggplot() +
    aes(x= forcats::fct_rev(factor(ordem)), y=prop, fill = fl_chefe,
        group = interaction(fl_chefe, fl_chefe)) +
    geom_col(aes(fill = fl_chefe)) +
    coord_flip() +
    scale_fill_grey(start = .3, end = .8) +
    labs(
      x = "Per?odo",
      y = "%"
    ) +
    facet_grid(idade_jovem ~ tipo_dom) +
    scale_y_continuous(breaks = c(seq(0,100,10))) +
    theme_bw() +
    theme(strip.text = element_text(size = 11, hjust = 0.5,
                                      vjust = 0.5, face = 'bold')) +
    theme(axis.text.x = element_text(size = 10,
                                     angle = 90,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     colour = 'black'),
          axis.title.x= element_text(size = 11,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = 'bold',
                                     colour = 'black'),
          axis.text.y = element_text(size = 10,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     colour = 'black'),
          axis.title.y = element_text(size = 11,
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      face = 'bold', colour = 'black'),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          strip.background = element_blank(),
          panel.border = element_rect(colour = "grey")))
