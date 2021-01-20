
library(tidyverse)  #Usaremos en tibble
library(readxl)
library(xml2)  #Para leer html.
library(rvest)
library(RSelenium)  #Pra trabajar con páginas dinámicas
library(wdman)    #Navegador fantasma, permite usar rsDriver
library(robotstxt)
#library(strip)
library(gdata)


###########################################
####################DATA INSUMO#############
###########################################
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
options(scipen=999)                     
proveedores<- select(contr_direc, "PROVEEDOR","ENTIDAD", "RUCPROV", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
proveedores2_num<-group_by(proveedores,PROVEEDOR,RUCPROV)
proveedores2_num<-summarize(proveedores2_num,MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())
proveedores2_num<-arrange(proveedores2_num,desc(Contratos))
df<-as.data.frame(proveedores2_num[2])

#data0<-as.data.frame(proveedores2_num[c(1:100),2])
 data1<-as.data.frame(proveedores2_num[c(1:40),2])
 data2<-as.data.frame(proveedores2_num[c(41:80),2])
 data3<-as.data.frame(proveedores2_num[c(81:120),2])
 data4<-as.data.frame(proveedores2_num[c(121:160),2])
 data5<-as.data.frame(proveedores2_num[c(161:200),2])
# data6<-as.data.frame(proveedores2_num[c(201:240),2])
# data7<-as.data.frame(proveedores2_num[c(241:280),2])
# data8<-as.data.frame(proveedores2_num[c(281:320),2])
# data9<-as.data.frame(proveedores2_num[c(321:360),2])
# data10<-as.data.frame(proveedores2_num[c(361:400),2])
# data11<-as.data.frame(proveedores2_num[c(401:440),2])
# data12<-as.data.frame(proveedores2_num[c(441:480),2])
# data13<-as.data.frame(proveedores2_num[c(481:520),2])
# data14<-as.data.frame(proveedores2_num[c(521:560),2])
# data15<-as.data.frame(proveedores2_num[c(561:600),2])
# data16<-as.data.frame(proveedores2_num[c(601:640),2])
# data17<-as.data.frame(proveedores2_num[c(641:680),2])
# data18<-as.data.frame(proveedores2_num[c(681:720),2])
# data19<-as.data.frame(proveedores2_num[c(721:760),2])
# data20<-as.data.frame(proveedores2_num[c(761:800),2])
# data21<-as.data.frame(proveedores2_num[c(801:840),2])
# data22<-as.data.frame(proveedores2_num[c(841:880),2])
# data23<-as.data.frame(proveedores2_num[c(881:920),2])
# data24<-as.data.frame(proveedores2_num[c(921:960),2])
# data25<-as.data.frame(proveedores2_num[c(961:1000),2])
# data26<-as.data.frame(proveedores2_num[c(1001:1040),2])
# data27<-as.data.frame(proveedores2_num[c(1041:1080),2])
# data28<-as.data.frame(proveedores2_num[c(1081:1120),2])
# data29<-as.data.frame(proveedores2_num[c(1121:1160),2])
# data30<-as.data.frame(proveedores2_num[c(1161:1200),2])
# data31<-as.data.frame(proveedores2_num[c(1201:1240),2])
# data32<-as.data.frame(proveedores2_num[c(1241:1280),2])
# data33<-as.data.frame(proveedores2_num[c(1281:1320),2])
# data34<-as.data.frame(proveedores2_num[c(1321:1360),2])
# data35<-as.data.frame(proveedores2_num[c(1361:1400),2])
# data36<-as.data.frame(proveedores2_num[c(1401:1440),2])
# data37<-as.data.frame(proveedores2_num[c(1441:1480),2])
# data38<-as.data.frame(proveedores2_num[c(1481:1520),2])
# data39<-as.data.frame(proveedores2_num[c(1521:1560),2])
# data40<-as.data.frame(proveedores2_num[c(1561:1600),2])
# data41<-as.data.frame(proveedores2_num[c(1601:1640),2])
# data42<-as.data.frame(proveedores2_num[c(1641:1680),2])
# data43<-as.data.frame(proveedores2_num[c(1681:1720),2])
# data44<-as.data.frame(proveedores2_num[c(1721:1760),2])
# data45<-as.data.frame(proveedores2_num[c(1761:1800),2])
# data46<-as.data.frame(proveedores2_num[c(1801:1840),2])
# data47<-as.data.frame(proveedores2_num[c(1841:1880),2])
# data48<-as.data.frame(proveedores2_num[c(1881:1920),2])
# data49<-as.data.frame(proveedores2_num[c(1921:1960),2])
# data50<-as.data.frame(proveedores2_num[c(1961:2000),2])
# data51<-as.data.frame(proveedores2_num[c(2001:2040),2])
# data52<-as.data.frame(proveedores2_num[c(2041:2080),2])
# data53<-as.data.frame(proveedores2_num[c(2081:2120),2])
# data54<-as.data.frame(proveedores2_num[c(2121:2160),2])
# data55<-as.data.frame(proveedores2_num[c(2161:2200),2])
# data56<-as.data.frame(proveedores2_num[c(2201:2240),2])
# data57<-as.data.frame(proveedores2_num[c(2241:2280),2])
# data58<-as.data.frame(proveedores2_num[c(2281:2320),2])
# data59<-as.data.frame(proveedores2_num[c(2321:2360),2])
# data60<-as.data.frame(proveedores2_num[c(2361:2400),2])
# data61<-as.data.frame(proveedores2_num[c(2401:2440),2])
# data62<-as.data.frame(proveedores2_num[c(2441:2480),2])
# data63<-as.data.frame(proveedores2_num[c(2481:2520),2])
# data64<-as.data.frame(proveedores2_num[c(2521:2560),2])
# data65<-as.data.frame(proveedores2_num[c(2561:2600),2])
# data66<-as.data.frame(proveedores2_num[c(2601:2640),2])
# data67<-as.data.frame(proveedores2_num[c(2641:2680),2])
# data68<-as.data.frame(proveedores2_num[c(2681:2720),2])
# data69<-as.data.frame(proveedores2_num[c(2721:2760),2])
# data70<-as.data.frame(proveedores2_num[c(2761:2769),2])



vector<-c("20523717759","20555589574","20419385442","20338570041","20293847038","20501262260",
        "20501262260", "20107914995","20523717759")
# df<-data.frame(vector)
# names(df)<-c("name")

#WEB SCRAPING
server<-phantomjs(port=5015L)
#Sys.sleep(8)
scrap<-function(ruc) {
  if(nchar(ruc)!=11){a<-NA}
  else{ 
  UrlMadre<-paste0("https://apps.osce.gob.pe/perfilprov-ui/estadistica/",ruc,"#inhabMJ")
  paths_allowed(paths = c(UrlMadre)) # TRUE
  options(encoding = "utf-8")
  Browser <- remoteDriver(browserName = "phantomjs", port=5015L) #browserName = "edge"
  Sys.sleep(5)
  Browser$open()
  Sys.sleep(5)
  Browser$navigate(paste(UrlMadre))#,RUCs[1],"#sanciones")) #AQUÍ SALE EL ERROR
  Sys.sleep(5)
  Browser$screenshot(display=TRUE)
  Sys.sleep(5)
  Pagina_actual<-Browser$getPageSource()
  Pagina<-read_html(Pagina_actual[[1]])  #Por el "xml2"
  PaginaRUC<-read_html(Pagina_actual[[1]])
  pag_text<-PaginaRUC%>%
    html_nodes(css= ".data-container")%>% 
    html_text()
  
  pag_text}
}

#DEFINIMOS FUNCIONES
# sanciones<- function(ruc){
#   text<-scrap(ruc)
#   if (is.na(text)){a<-NA} else{
#   sanciones<-str_match(text, "Sanciones del TCE (\\s*(.*?)\\s*)El proveedor") #tidyverse
#   sanciones<-strsplit(sanciones, "") 
#   sanciones <-as.numeric(sanciones[[2]][2])
#   sanciones}}

penalidades<- function(ruc){
  text<-scrap(ruc)
  penalidades<-str_match(text, "Penalidades (\\s*(.*?)\\s*)El proveedor") #tidyverse
  penalidades<-strsplit(penalidades, "") 
  penalidades<-as.numeric(penalidades[[2]][2])
  penalidades}

inah_admi<-function(ruc){
  text<-scrap(ruc)
  ij<-str_match(text, "Inhabilitación administrativa (\\s*(.*?)\\s*)El proveedor") #tidyverse
  ij<-strsplit(ij, "") 
  ij<-as.numeric(ij[[2]][2])
  ij}


####################################
#############SANCIONES################
###################################
#Hasta sanciones13, en total 520 datos (13*40).

# sanciones1<-sapply(data1$RUCPROV,sanciones)
# sanciones2<-sapply(data2$RUCPROV,sanciones)
# sanciones3<-sapply(data3$RUCPROV,sanciones)
# sanciones4<-sapply(data4$RUCPROV,sanciones)
# sanciones5<-sapply(data5$RUCPROV,sanciones)
# sanciones6<-sapply(data6$RUCPROV,sanciones)
# sanciones7<-sapply(data7$RUCPROV,sanciones)
# sanciones8<-sapply(data8$RUCPROV,sanciones)
# sanciones9<-sapply(data9$RUCPROV,sanciones)
# sanciones10<-sapply(data10$RUCPROV,sanciones)
# sanciones11<-sapply(data11$RUCPROV,sanciones)
# sanciones12<-sapply(data12$RUCPROV,sanciones)
# sanciones13<-sapply(data13$RUCPROV,sanciones)
# sanciones14<-sapply(data14$RUCPROV,sanciones)
# sanciones15<-sapply(data15$RUCPROV,sanciones)
# sanciones16<-sapply(data16$RUCPROV,sanciones)
# sanciones17<-sapply(data17$RUCPROV,sanciones)
# sanciones18<-sapply(data18$RUCPROV,sanciones)
# sanciones19<-sapply(data19$RUCPROV,sanciones)
# sanciones20<-sapply(data20$RUCPROV,sanciones)
# sanciones21<-sapply(data21$RUCPROV,sanciones)
# sanciones22<-sapply(data22$RUCPROV,sanciones)
# sanciones23<-sapply(data23$RUCPROV,sanciones)
# sanciones24<-sapply(data24$RUCPROV,sanciones)
# sanciones25<-sapply(data25$RUCPROV,sanciones)
# sanciones26<-sapply(data26$RUCPROV,sanciones)
# sanciones27<-sapply(data27$RUCPROV,sanciones)
# sanciones28<-sapply(data28$RUCPROV,sanciones)
# sanciones29<-sapply(data29$RUCPROV,sanciones)
# sanciones30<-sapply(data30$RUCPROV,sanciones)
# sanciones31<-sapply(data31$RUCPROV,sanciones)
# sanciones32<-sapply(data32$RUCPROV,sanciones)
# sanciones33<-sapply(data33$RUCPROV,sanciones)
# sanciones34<-sapply(data34$RUCPROV,sanciones)
# sanciones35<-sapply(data35$RUCPROV,sanciones)
# sanciones36<-sapply(data36$RUCPROV,sanciones)
# sanciones37<-sapply(data37$RUCPROV,sanciones)
# sanciones38<-sapply(data38$RUCPROV,sanciones)
# sanciones39<-sapply(data39$RUCPROV,sanciones)
# sanciones40<-sapply(data40$RUCPROV,sanciones)
# sanciones41<-sapply(data41RUCPROV,sanciones)
# sanciones42<-sapply(data42$RUCPROV,sanciones)
# sanciones43<-sapply(data43$RUCPROV,sanciones)
# sanciones44<-sapply(data44$RUCPROV,sanciones)
# sanciones45<-sapply(data45$RUCPROV,sanciones)
# sanciones46<-sapply(data46$RUCPROV,sanciones)
# sanciones47<-sapply(data47$RUCPROV,sanciones)
# sanciones48<-sapply(data48$RUCPROV,sanciones)
# sanciones49<-sapply(data49$RUCPROV,sanciones)
# sanciones50<-sapply(data50$RUCPROV,sanciones)
# sanciones51<-sapply(data51$RUCPROV,sanciones)
# sanciones52<-sapply(data52$RUCPROV,sanciones)
# sanciones53<-sapply(data53$RUCPROV,sanciones)
# sanciones54<-sapply(data54$RUCPROV,sanciones)
# sanciones55<-sapply(data55$RUCPROV,sanciones)
# sanciones56<-sapply(data56$RUCPROV,sanciones)
# sanciones57<-sapply(data57$RUCPROV,sanciones)
# sanciones58<-sapply(data58$RUCPROV,sanciones)
# sanciones59<-sapply(data59$RUCPROV,sanciones)
# sanciones60<-sapply(data60$RUCPROV,sanciones)
# sanciones61<-sapply(data61$RUCPROV,sanciones)
# sanciones62<-sapply(data62$RUCPROV,sanciones)
# sanciones63<-sapply(data63$RUCPROV,sanciones)
# sanciones64<-sapply(data64$RUCPROV,sanciones)
# sanciones65<-sapply(data65$RUCPROV,sanciones)
# sanciones66<-sapply(data66$RUCPROV,sanciones)
# sanciones67<-sapply(data67$RUCPROV,sanciones)
# sanciones68<-sapply(data68$RUCPROV,sanciones)
# sanciones69<-sapply(data69$RUCPROV,sanciones)
# sanciones70<-sapply(data70$RUCPROV,sanciones)
sanciones("20502503180")
sanciones1
# for (i in 1:nrow(df)){ if (i>=41 & i <=80){sanciones2<-lapply(df$RUCPROV,sanciones)}
#   else if (i==81){Sys.sleep(20)}
#   else if (i>=81 & i <=120){sanciones3<-lapply(df$RUCPROV,sanciones)}
#  }

#traceback()
#debug()

##################################################
######################PENALIDADES#################
penalidades1<-sapply(data1$RUCPROV,penalidades)
penalidades2<-sapply(data2$RUCPROV,penalidades)
penalidades3<-sapply(data3$RUCPROV,penalidades)

# penalidades4<-sapply(data4$RUCPROV,penalidades)
# penalidades5<-sapply(data5$RUCPROV,penalidades)
# penalidades6<-sapply(data6$RUCPROV,penalidades)
# penalidades7<-sapply(data7$RUCPROV,penalidades)
# penalidades8<-sapply(data8$RUCPROV,penalidades)
# penalidades9<-sapply(data9$RUCPROV,penalidades)
# penalidades10<-sapply(data10$RUCPROV,penalidades)
# penalidades11<-sapply(data11$RUCPROV,penalidades)
# penalidades12<-sapply(data12$RUCPROV,penalidades)
# penalidades13<-sapply(data13$RUCPROV,penalidades)
##################################################
######################Inah_jud#################
ij1<-sapply(data1$RUCPROV,inah_admi)
ij2<-sapply(data2$RUCPROV,inah_admi)
ij3<-sapply(data3$RUCPROV,inah_admi)

##################################################
######################UNIÓN Y LIMPIEZA#################
################################################
class(data1)

data_120<-as.data.frame(proveedores2_num[c(1:120),])

sanciones_num<-data.frame(c(sanciones1,sanciones2,sanciones3))
  #                           ,sanciones4,sanciones5,sanciones6,sanciones7,
  # sanciones8,sanciones9,sanciones10,sanciones11,sanciones12,sanciones13))

penas_num<-data.frame(c(penalidades1,penalidades2,penalidades3))

                        # ,penalidades4,penalidades5,penalidades6,penalidades7,
                        #     penalidades8,penalidades9,penalidades10,penalidades11,penalidades12,penalidades13))


df_sanc_pena<-cbind(data_120,sanciones_num,penas_num)
#View(df_sanc_pena)
names(df_sanc_pena)

#df_sanc_pena<-df_sanc_pena[,c(1,2,3,4,5,6)] 
names(df_sanc_pena)<-c("PROVEEDOR","RUCPROV","MONTO TOTAL ADJUDICADO (millones) ","NUMERO DE CONTRATOS",
                       "SANCIONES DEL TCE","PENALIDAD POR INCUMPLIMIENTO")
class(df_sanc_pena)
View(df_sanc_pena)
View(df_sanc_pena[sample(nrow(df_sanc_pena), 10),]) #Una muestra de 10 datos para revisar                    

#GUARDAMOS NUEVA DATA:
write.table(df_sanc_pena, "DATOS_Webscr", sep= ";",row.names=FALSE)                       
#Añadir ".csv" al documento en caso no sea leíble.
