## code to prepare `DATASET` dataset goes here
#En dataraw, como el nombre indica generamos la data
data120<-read.csv("DATA_CONSOLIDADA_120.csv", sep= ";")
# save(data120, file = "data120.RData")
load("mapamonto.Rdata")
usethis::use_data(data120, overwrite = TRUE)  #creará una carpeta llamada "data" y guardará el .Rdata
usethis::use_data(map_mon, overwrite = TRUE)

