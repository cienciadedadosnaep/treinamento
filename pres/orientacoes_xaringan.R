
devtools::install_github("yihui/xaringan")

########################################################################################
#1-Carregamento de dados
#1.1-Dados do Covid19  
# referencia(22-06-2020) - (https://data.brasil.io/dataset/covid19/_meta/list.html)

library(readr)

caso <- read_csv("pres/data/caso.csv")


#2 - Analise exploratoria

library("tibble")
library("tidyverse")
library("tidyr")

caso_day_Br <- NULL
caso_day_Br <- caso  %>%  group_by(date) %>% summarise()

caso_day_Br_s <- NULL
caso_day_Br_s <- caso %>%  group_by(date) %>% summarise(deaths = sum(deaths,na.rm = "TRUE"))

plot(caso_day_Br_s)

# Outro banco de dados https://covid.saude.gov.br/
#caso_MS <- read_csv("pres/data/HIST_PAINEL_COVIDBR_21jun2020.csv")

caso_MS <- read_delim("pres/data/HIST_PAINEL_COVIDBR_21jun2020.csv",";", escape_double = FALSE, trim_ws = TRUE)


caso_MS$data<-as.Date(caso_MS$data,"%m/%d/%Y")



caso_MS_BA<-NULL
caso_MS_BA <- caso_MS %>% filter(estado=="BA") %>% group_by(data) %>% summarise(quantidade = sum(obitosNovos,na.rm = "TRUE"))
plot(caso_MS_BA$data,caso_MS_BA$quantidade)
caso_MS_BR<-NULL
caso_MS_BR <- caso_MS %>% filter(regiao=="Brasil") %>% group_by(data) %>% summarise(quantidade = sum(obitosNovos,na.rm = "TRUE"))
plot(caso_MS_BR$data,caso_MS_BR$quantidade)

caso_MS_BR$dayweek<-NULL
caso_MS_BR$dayweek<- weekdays(caso_MS_BR$data)

ggplot(caso_MS_BR, aes(x = data, y = quantidade,fill = dayweek)) +
  #  geom_col(position = "dodge")+
  geom_col()+      xlab("Data") + ylab("Nº") +  labs(fill = "dayweek")+
  theme(axis.title.x = element_text(color = "gray",size = 20)) +
  theme(axis.text.x=element_text(color = "black", size=16, angle=30, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "gray",size = 20))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) 

