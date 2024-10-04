#rm(list = ls())
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("stringr")
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Carregando arquivo dataframe.
custo_base <- read_excel("C:\\tmp\\custo_import.xlsx")

# Criando dataframe para converter bandeira para org e cod.
bandeiras <- c(custo_base$bandeira)
bandeiras1 <- c(custo_base$bandeira)
dadosMon <- tibble(org = bandeiras)
dadosMon1 <- tibble(code = bandeiras1)

dadosMon$org [dadosMon$org == "AA"] <- "0000"
dadosMon1$code [dadosMon1$code %in% c("AA", "BB", "PP")] <-"BR01"
dadosMon$org [dadosMon$org == "MM"] <- "1111"
dadosMon1$code [dadosMon1$code == "MM"] <- "BR02"
dadosMon$org [dadosMon$org == "BB"] <- "2222"
dadosMon$org [dadosMon$org == "PP"] <- "3333"
dadosMon$org [dadosMon$org %in% c("RR", "BR bandeira")] <- "4444"
dadosMon1$code [dadosMon1$code %in% c("RR", "BR bandeira")] <- "BR02"

# Convers�o data.
date <- as.Date(custo_base$date,format="%d.%m.%Y")
dataconv <- format(date, "%Y%m%d")

# Select utilizando paiper e mutate para criar nova coluna com c�lculo.
relatorio <- select(custo_base,codsap,loja,custoatual,custototal) %>%
    mutate(dif_custo = custototal-custoatual)
    
# Criando dataframe para valores statics.
static <- c(dadosMon$org)
dadosStatic1 <- tibble(DISTR_CHAN = static)
dadosStatic2 <- tibble(BILL_TYPE = static)
dadosStatic3 <- tibble(CPCREENTNI = static)
dadosStatic4 <- tibble(CPCREENTBU = static)
dadosStatic5 <- tibble(BASE_UOM = static)
dadosStatic6 <- tibble(SALES_UNIT = static)
dadosStatic7 <- tibble(LOC_CURRCY = static)
dadosStatic8 <- tibble(CPCREENTSU = static)
dadosStatic9 <- tibble(CPCREENTPV = static)
dadosStatic10 <- tibble(RTCREENTSV = static)
dadosStatic11 <- tibble(RTCREENTST = static)
dadosStatic12 <- tibble(RTSALDIFST = static)
dadosStatic13 <- tibble(CPSAEXCUNI = static)
dadosStatic14 <- tibble(CPSAEXCUBU = static)
dadosStatic15 <- tibble(RTSAEXCUSV = static)
dadosStatic16 <- tibble(CPSAEXCUSU = static)
dadosStatic17 <- tibble(RTSAEXCUST = static)
dadosStatic18 <- tibble(RT_PROMO = static)
dadosStatic19 <- tibble(MARTEON = static)
dadosStatic20 <- tibble(MARTEOD = static)

dadosStatic1$DISTR_CHAN [dadosStatic1$DISTR_CHAN %in% c("0000", "1111", "2222", "3333", "4444")] <-"2"
dadosStatic2$BILL_TYPE [dadosStatic2$BILL_TYPE %in% c("0000", "1111", "2222", "3333", "4444")] <-"FP"
dadosStatic3$CPCREENTNI [dadosStatic3$CPCREENTNI %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic4$CPCREENTBU [dadosStatic4$CPCREENTBU %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic5$BASE_UOM [dadosStatic5$BASE_UOM %in% c("0000", "1111", "2222", "3333", "4444")] <-""
dadosStatic6$SALES_UNIT [dadosStatic6$SALES_UNIT %in% c("0000", "1111", "2222", "3333", "4444")] <-""
dadosStatic7$LOC_CURRCY [dadosStatic7$LOC_CURRCY %in% c("0000", "1111", "2222", "3333", "4444")] <-"BRL"
dadosStatic8$CPCREENTSU [dadosStatic8$CPCREENTSU %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic9$CPCREENTPV [dadosStatic9$CPCREENTPV %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic10$RTCREENTSV [dadosStatic10$RTCREENTSV %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic11$RTCREENTST [dadosStatic11$RTCREENTST %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic12$RTSALDIFST [dadosStatic12$RTSALDIFST %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic13$CPSAEXCUNI [dadosStatic13$CPSAEXCUNI %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic14$CPSAEXCUBU [dadosStatic14$CPSAEXCUBU %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic15$RTSAEXCUSV [dadosStatic15$RTSAEXCUSV %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic16$CPSAEXCUSU [dadosStatic16$CPSAEXCUSU %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic17$RTSAEXCUST [dadosStatic17$RTSAEXCUST %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic18$RT_PROMO [dadosStatic18$RT_PROMO %in% c("0000", "1111", "2222", "3333", "4444")] <-""
dadosStatic19$MARTEON [dadosStatic19$MARTEON %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"
dadosStatic20$MARTEOD [dadosStatic20$MARTEOD %in% c("0000", "1111", "2222", "3333", "4444")] <-"0"

# Consolidando variaveis em um dataframe.
data_frm <- cbind(MATERIAL = custo_base$codsap, PLANT = custo_base$loja, 
                  DISTR_CHAN = dadosStatic1$DISTR_CHAN, BILL_TYPE = dadosStatic2$BILL_TYPE, 
                  '/BIC/ZDAYCOM' = dataconv, MAT_PLANT = custo_base$codsap, CALDAY = dataconv, 
                  CPCREENTNI = dadosStatic3$CPCREENTNI, CPCREENTBU = dadosStatic4$CPCREENTBU, 
                  BASE_UOM = dadosStatic5$BASE_UOM, SALES_UNIT = dadosStatic6$SALES_UNIT, 
                  LOC_CURRCY = dadosStatic7$LOC_CURRCY, CPCREENTSU = dadosStatic8$CPCREENTSU, 
                  CPCREENTPV = dadosStatic9$CPCREENTPV, RTCREENTSV = dadosStatic10$RTCREENTSV, 
                  RTCREENTST = dadosStatic11$RTCREENTST, RTSALDIFST = dadosStatic12$RTSALDIFST, 
                  CPSAEXCUNI = dadosStatic13$CPSAEXCUNI, CPSAEXCUBU = dadosStatic14$CPSAEXCUBU, 
                  CPSAEXCUPV = round (relatorio$dif_custo, 3),  RTSAEXCUSV = dadosStatic15$RTSAEXCUSV, 
                  CPSAEXCUSU = dadosStatic16$CPSAEXCUSU, RTSAEXCUST = dadosStatic17$RTSAEXCUST, 
                  SALESORG = dadosMon$org, COMP_CODE = dadosMon1$code, RT_PROMO = dadosStatic18$RT_PROMO, 
                  MARTEON = dadosStatic19$MARTEON, MARTEOD = dadosStatic20$MARTEOD)

# Exportar conte�do para .csv.
write.table(data_frm, "import_rpa.csv", row.names = FALSE, sep = ";", dec = ".")
