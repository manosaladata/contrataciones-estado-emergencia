library(readxl)
library(correlation)
library(tidyverse)
# setwd("D:/GITHUB-PROYECTOS BEST/contrataciones-estado-emergencia/Data")
setwd("D:/ABCN/Github/contrataciones-estado-emergencia/data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
options(scipen=999)
sapply(contr_direc, class)

boxplot(contr_direc$MONTOADJUDICADOSOLES)


contr_direc %>% 
  ggplot(aes(x=RUBROS,y=MONTOADJUDICADOSOLES/1000000, fill=RUBROS)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) + theme(axis.text.x = element_text(angle = 90))

contr_direc %>% 
  ggplot(aes(x=RUBROS,y=MONTOADJUDICADOSOLES/1000000, fill=RUBROS)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))+ 
  facet_wrap(~TIPOPROVEEDOR)

contr_direc_natural <- contr_direc %>% 
  filter(TIPOPROVEEDOR %in% ("Persona Natural"))

contr_direc_natural %>% 
  ggplot(aes(x=RUBROS,y=MONTOADJUDICADOSOLES/1000000, fill=RUBROS)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) + theme(axis.text.x = element_text(angle = 90))

contr_direc_natural %>% 
  group_by(PROVEEDOR)%>% 
  summarize(MONTOADJUDICADOSOLES = sum(MONTOADJUDICADOSOLES/1000000), veces=n()) %>% 
  arrange(desc(MONTOADJUDICADOSOLES))%>%
  View()



