# Analise PEUC

install.packages("gridExtra")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("Hmisc")
install.packages ("dplyr")
install.packages ("ggplot2")
install.packages ("tidyr")
# Pacotes

library(dplyr)
library(ggplot2)
library(tidyr)
library(rgdal)
library (geojsonio)
library(stringr)
library(gridExtra)
library(RColorBrewer)
library(lubridate)
library(Hmisc)


# MAc

setwd("~/OneDrive/APA Bonfim-Guaraira/NUC/PEUC/Analises")

# Windows

setwd("C:/Users/Daniel Magalhaes/OneDrive/APA Bonfim-Guaraira/NUC/PEUC/Analises")

ucrn = tbl_df( read.csv ("Dados/Tabelas/Informa??es_UCRN_2018_v0.csv", header = T, fileEncoding="UTF-8", sep =  ";"))





ucrn%>%
  filter (Status == 'Criada', Categoria != 'Outro')%>%
  group_by(ano_criacao, Ente.federativo)%>%
  summarise(n = n())%>%
  ggplot (aes(ano_criacao, y=n, fill= Ente.federativo))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(1950, 2017, by = 5))+
  xlab("Anos")+
  ylab("N?mero UC Criadas")


# Evolu??o numero UC (Estado, federa, municipios, particulares)

ucrn%>%
  filter (Status == 'Criada', Categoria != 'Outro') %>%
  group_by(ano_criacao)%>%
  summarise(n = n())%>%
  mutate(total = cumsum(n), ano = as.numeric(ano_criacao))%>%
  ggplot (aes(ano_criacao, y=total))+
  geom_line()+
  scale_x_continuous(breaks=seq(1950, 2018, by = 5))+
  scale_y_continuous(breaks=seq(0, 30, by = 5))+
  xlab("Anos")+
  ylab("N?mero UC Criadas no RN")


# Evolu??o area UC (Estado, federa, municipios, particulares)
ucrn%>%
  filter (Status == 'Criada', Categoria != 'Outro') %>%
  group_by(ano_criacao)%>%
  summarise(n = sum(A_UC_km))%>%
  mutate(total = cumsum(n), ano = as.numeric(ano_criacao))%>%
  ggplot (aes(ano_criacao, y=total))+
  geom_line()+
  scale_x_continuous(breaks=seq(1950, 2018, by = 5))+
  xlab("Anos")+
  ylab("?rea (km2) de UC Criadas no RN")


# Evolu??o N?mero UC estado
ucrn%>%
  filter (Ente.federativo == 'Estadual', Status == 'Criada', Categoria != 'Outro') %>%
  group_by(ano_criacao)%>%
  summarise(n = n())%>%
  mutate(total = cumsum(n), ano = as.numeric(ano_criacao))%>%
  ggplot (aes(ano_criacao, y=total))+
  geom_line()+
  scale_x_continuous(breaks=seq(1975, 2018, by = 4))+
  scale_y_continuous(breaks=seq(0, 10, by = 1))+
  xlab("Anos")+
  ylab("N?mero de UC Criadas pelo Estado do RN")


# Evolu??o area UC estado
ucrn%>%
  filter (Ente.federativo == 'Estadual', Status == 'Criada', Categoria != 'Outro') %>%
  group_by(ano_criacao)%>%
  summarise(n = sum(A_UC_km))%>%
  mutate(total = cumsum(n), ano = as.numeric(ano_criacao))%>%
  ggplot (aes(ano_criacao, y=total))+
  geom_line()+
  scale_x_continuous(breaks=seq(1950, 2018, by = 5))+
  xlab("Anos")+
  ylab("?rea UC (km2) criadas pelo Estado do RN")



# Evolu??o area por esfera de gest?o - ERROR na hora de agrupar

ucrn%>%
  filter (Status == 'Criada', Categoria != 'Outro') %>%
  group_by(ano_criacao, Ente.federativo)%>%
  summarise(n = sum(A_UC_km))%>%
  spread(ano_criacao, n)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.),0,.)))%>%
  gather (ano_criacao,n, `1950`:`2018`)%>%
  mutate(total = cumsum(n))%>%
  ggplot (aes(ano, y=total, col=Ente.federativo))+
  geom_line()+
  xlab("Anos")+
  ylab("?rea UC (km)")

