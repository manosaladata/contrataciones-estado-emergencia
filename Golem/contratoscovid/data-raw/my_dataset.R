## code to prepare `my_dataset` dataset goes here


contr_direc <- read_excel("Data/CONOSCE_CONTRATACIONDIRECTA.xlsx")
usethis::use_data(contr_direc, overwrite = TRUE)
departamentos<-st_read("DEPARTAMENTOS.shp")  