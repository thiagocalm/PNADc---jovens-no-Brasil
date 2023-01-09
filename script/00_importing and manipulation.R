
options(scipen = 999) #retirando notacao cientifica

# Bibliotecas -------------------------------------------------------------

ifelse(!require(PNADcIBGE),install.packages("PNADcIBGE"),require(PNADcIBGE))
library(survey)
library(srvyr)
library(convey)
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))


## 2015 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2015, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao


## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_15.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_15.rds", compress = TRUE)



## 2016 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2016, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_16.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_16.rds", compress = TRUE)



## 2017 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2017, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_17.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_17.rds", compress = TRUE)



## 2018 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2018, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_18.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_18.rds", compress = TRUE)




## 2019 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2019, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_19.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_19.rds", compress = TRUE)



## 2020 - Replicando o processo de construcao da tipologia -----------------

# Importa??o dos dados ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2020, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))



# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_20.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_20.rds", compress = TRUE)



# Dados 2021 ----------------------------------------------------

dados_pnadc <- get_pnadc(year = 2021, # ano selecionado
                         quarter = 3, #trimestre do ano selecionado
                         vars = c("Ano",
                                  "Trimestre",
                                  "UF",
                                  "UPA", # Unidade Primaria de Amostragem
                                  "Estrato",
                                  "V1008", # Controle do domicilio
                                  "V1022", # situacao do domicilio
                                  "V1027", #peso do domicilio e das pessoas sem calibracao
                                  "V1028", # peso do domicilio e das pessoas
                                  "V1014", # numero do painel
                                  "V1016", # numero da entrevista
                                  "V2001", # controle pessoa no domicilio
                                  "V2003", # numero de ordem no domicilio
                                  "V2005", # condicao no domicilio
                                  "V2007", # sexo
                                  "V2009", # idade em anos completos
                                  "V2010", # cor ou raca
                                  "V3002", # frequenta escola
                                  "VD4009", # posicao ocupacional
                                  "V4019", # possui cnpj
                                  "VD2002", # condicao no domicilio agregada
                                  "VD2004", # tipo de domicilio
                                  "VD3004", # nivel de instrucao mais elevado alcancado
                                  "VD4001", # condicao em relacao a forca de trabalho
                                  "VD4002", # condicao de ocupacao
                                  "VD4004", # subocupacao
                                  "VD4030"), # motivo pelo qual nao procurou trabalho
                         design = FALSE, # desativando aplica??o de plano de amostra complexa
                         labels = FALSE # desativando aplicacao automatica de labels
)

vars <- c("Ano",
          "Trimestre",
          "UF",
          "UPA", # Unidade Primaria de Amostragem
          "Estrato",
          "V1008", # Controle do domicilio
          "V1022", # situacao do domicilio
          "V1027", #peso do domicilio e das pessoas sem calibracao
          "V1028", # peso do domicilio e das pessoas
          "V1014", # numero do painel
          "V1016", # numero da entrevista
          "V2001", # controle pessoa no domicilio
          "V2003", # numero de ordem no domicilio
          "V2005", # condicao no domicilio
          "V2007", # sexo
          "V2009", # idade em anos completos
          "V2010", # cor ou raca
          "V3002", # frequenta escola
          "VD4009", # posicao ocupacional
          "V4019", # possui cnpj
          "VD2002", # condicao no domicilio agregada
          "VD2004", # tipo de domicilio
          "VD3004", # nivel de instrucao mais elevado alcancado
          "VD4001", # condicao em relacao a forca de trabalho
          "VD4002", # condicao de ocupacao
          "VD4004", # subocupacao
          "VD4030") # motivo pelo qual nao procurou trabalho

## verificando classe dos dados

class(dados_pnadc) # n?o est? com aplicacao do plano amostral

# Tipo de domicilio para amostra completa (Brasil) ------------------------


dados_pnadc_dom <- dados_pnadc %>%
  select(vars) %>%
  mutate(
    Ano = as.numeric(Ano),
    Trimestre = as.numeric(Trimestre),
    UF = as.numeric(UF),
    UPA = as.numeric(UPA),
    Estrato = as.numeric(Estrato),
    V1008 = as.numeric(V1008),
    V1022 = as.numeric(V1022),
    V1014 = as.numeric(V1014),
    V1016 = as.numeric(V1016),
    V2003 = as.numeric(V2003),
    V2005 = as.numeric(V2005),
    V2007 = as.numeric(V2007),
    V2010 = as.numeric(V2010),
    V3002 = as.numeric(V3002),
    VD4009 = as.numeric(VD4009),
    V4019 = as.numeric(V4019),
    VD2002 = as.numeric(VD2002),
    VD2004 = as.numeric(VD2004),
    VD3004 = as.numeric(VD3004),
    VD4001 = as.numeric(VD4001),
    VD4002 = as.numeric(VD4002),
    VD4004 = as.numeric(VD4004),
    VD4030 = as.numeric(VD4030),
    # construindo variaveis de controle
    id_dom = as.numeric(paste0(UPA,V1008,V1014)),
    id_pes = as.numeric(paste0(UPA, V1008, V1014, V2003))
  ) %>%
  select(id_dom,id_pes, everything())

glimpse(dados_pnadc_dom) #ok, deu certo


df_tipo_domicilio_br <- dados_pnadc_dom %>%
  select(id_dom, id_pes, V2001, VD2002) %>%
  mutate(valor = 1,
         relacao = paste0("relacao", VD2002)) %>%
  select(-VD2002) %>%
  spread(key = relacao, value = valor, fill = 0) %>%
  group_by(id_dom, V2001) %>%
  summarise_all(.funs = sum)

# Constru??o da tipologia de domic?lios mais desagregada
df_tipo_domicilio_br <- df_tipo_domicilio_br %>%
  mutate(
    tipo_dom = case_when(

      # domicilio unipessoal
      V2001 == 1 ~ 1, #unipessoal, ok!

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo pai/mae
      V2001 == 2 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 ~ 2, #casal sem filho, ok!
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 >= 1 &
        relacao4 >= 0 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 >= 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, #monoparental com um ou mais filhos
      V2001 >= 3 &
        relacao2 == 1 &
        relacao3 < 1 &
        relacao4 >= 1 & # enteados do responsavei. Pressuposto: enteado tem lacos de parentesco
        relacao5 < 1 &
        relacao6 < 1 &
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 < 1 &
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, #casal com um ou mais filhos (filhos sendo enteados do responsavel)

      # domicilio nuclear - relcoes derivadas do responsavel da familia sendo filho/filha
      V2001 >= 3 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 2 & # dois pais
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 3, # casais com um ou mais filhos
      V2001 >= 2 &
        relacao2 < 1 &
        relacao3 < 1 &
        relacao4 < 1 &
        relacao5 < 1 &
        relacao6 == 1 & #somente um pai/mae
        relacao7 < 1 &
        relacao8 < 1 &
        relacao9 < 1 &
        relacao10 >= 0 & # pode ter irmaos
        relacao11 < 1 &
        relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        relacao16 < 1 ~ 4, # monoparental com um ou mais filhos

      # domicilio com familia estendida - relacoes derivadas por residuo das demais
      V2001 >= 2 &
        #relacao2 >= 0 &
        #relacao3 >= 0 &
        #relacao6 >= 0 &
        #relacao8 >= 0 &
        #relacao9 >= 0 &
        #relacao10 >= 0 &
        #relacao11 >= 0 &
        #relacao12 < 1 &
        relacao13 < 1 &
        relacao14 < 1 &
        relacao15 < 1 &
        #relacao17 < 1 & ~ 5
        relacao16 < 1 ~ 5, #domicilio com familia estendida
      # domicilio composto - relacoes derivadas por residuo das demais
      TRUE ~ 6))


# verificando andamento

dados_pnadc_dom <- dados_pnadc_dom %>%
  left_join(df_tipo_domicilio_br %>%
              select(id_dom, tipo_dom), by = "id_dom")

table(dados_pnadc_dom$VD2004, dados_pnadc_dom$tipo_dom) #verificacao

## Salvando as bases de dados

#write_rds(dados_pnadc_dom, "pnadc_21.dta")

saveRDS(dados_pnadc_dom, "./data/pnadc_21.rds", compress = TRUE)
