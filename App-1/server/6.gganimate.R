library(tidyverse)
library(janitor)
library(gganimate)

setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")


#setwd("D:/ABCN/Github/contrataciones-estado-emergencia/data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#contr_direc<-na.omit(contr_direc)                       No usar, sino se irÃ¡ casi toda la base se elimina.
names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)
options(scipen=999)                                  .
sapply(contr_direc, class)  



######ANÁLISIS DE RUBROS###########
library(lubridate)
func_mes<-function(x){month(as.POSIXlt(x, format="%d/%m/%Y"))}

select<- as.data.frame(select(contr_direc, "RUBROS","FECHACONVOCATORIA", "MONTO_SOLES_EN_MILLONES"))
select[,2]<-sapply(select[,2], func_mes)
names(select)[3]<-"value"
names(select)
#names(select)[3]<-"value"
#class(select[2,2])


# month()
# dfMon <- select%>% 
#   mutate(date = parse_date_time(month, "my"),
#          year = year(date),
#          month = month(date)
#   ) %>% 
#   arrange(year, month) %>% 
#   select(date, year, month, result)

select_format<- select %>%
  group_by(FECHACONVOCATORIA) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value)* 1,
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9)))%>%
  group_by(RUBROS) %>% 
  filter(rank <=10) %>%
  ungroup()

#View(select_format)


# p <- ggplot(select_format, aes(rank, group = RUBROS, 
#                      fill = as.factor(RUBROS), color = as.factor(RUBROS))) +
#   geom_tile(aes(y = MONTO_SOLES_EN_MILLONES/2,
#                 height = MONTO_SOLES_EN_MILLONES,
#                 width = 0.9), alpha = 0.8, color = NA) +
#   geom_text(aes(y = 0, label = paste(RUBROS, " ")), vjust = 0.2, hjust = 1)+ 
#   
#   coord_flip(clip = "off", expand = FALSE) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_reverse() +
#   guides(color = FALSE, fill = FALSE) +
#   
#   labs(title='{closest_state}', x = "", y = "GFP per capita") +
#   theme(plot.title = element_text(hjust = 0, size = 22),
#         axis.ticks.y = element_blank(),  # These relate to the axes post-flip
#         axis.text.y  = element_blank(),  # These relate to the axes post-flip
#         plot.margin = margin(1,1,1,4, "cm")) +
#   transition_states(FECHACONVOCATORIA, transition_length = 4, state_length = 1) +
#   ease_aes('cubic-in-out')


# animate(p, fps = 25, duration = 20, width = 800, height = 600)




# 
# 
# #filter only country rows
# gdp <- gdp[1:217,]
# gdp_tidy <- gdp %>% 
#   mutate_at(vars(contains("YR")),as.numeric) %>% 
#   gather(year,value,3:13) %>% 
#   janitor::clean_names() %>% 
#   mutate(year = as.numeric(stringr::str_sub(year,1,4)))
# write_csv(gdp_tidy,"./data/gdp_tidy.csv")
# 
# 
# ################################
#gdp_tidy <- read_csv("./data/gdp_tidy.csv")

# gdp_formatted <- gdp_tidy %>%
#   group_by(year) %>%
#   # The * 1 makes it possible to have non-integer ranks while sliding
#   mutate(rank = rank(-value),
#          Value_rel = value/value[rank==1],
#          Value_lbl = paste0(" ",round(value/1e9))) %>%
#   group_by(country_name) %>%
#   filter(rank <=10) %>%
#   ungroup()


####static plot##########
rubrosplot = ggplot(select_format, aes(rank, group = RUBROS,
                                       fill = as.factor(RUBROS), color = as.factor(RUBROS)))+
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(RUBROS, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))+
  transition_states(FECHACONVOCATORIA, transition_length = 4, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'EVOLUCIÓN DE RUBROS POR MES : {closest_state}',  
       subtitle  =  "Top de RUBROS",
       caption  = "Montos en millones de soles | Fuente: OSCE")

#gif
animate(rubrosplot, 200, fps = 25, duration = 20, width = 1000, height = 800,
        renderer = gifski_renderer("gganim3.gif"), end_pause = 15, start_pause =  15) 
