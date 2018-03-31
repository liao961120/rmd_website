library(readr)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(heatmaply)

psy_test <- read_rds("./data/psy_test_analy.rds")
psy_test_f <- read_rds("./data/psy_test_filtered.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")


## correlation matrix-------------------
AT_item <- var_info$vars_en[var_info$construct_en=="AT"]
    AT_item <- AT_item[!is.na(AT_item)]
CF_item <- var_info$vars_en[var_info$construct_en=="CF"]
    CF_item <- CF_item[!is.na(CF_item)]
HD_item <- var_info$vars_en[var_info$construct_en=="HD"]
    HD_item <- HD_item[!is.na(HD_item)]
RF_item <- var_info$vars_en[var_info$construct_en=="RF"]
    RF_item <- RF_item[!is.na(RF_item)]
Fk_item <- var_info$vars_en[var_info$construct_en=="F"]
    Fk_item <- Fk_item[!is.na(Fk_item)]

All_data <- psy_test_f %>%
    select(AT_item,CF_item,Fk_item,HD_item,RF_item)

colnames(All_data) <- var_info$con_num[1:62]
# which(colnames(All_data)=="F-01")  # finding index
# which(colnames(All_data)=="F-06")  # finding index
All_data <- All_data[,-(27:32)] # without Fk
cor_mt <- round(cor(All_data), 2) # rounded corr matrix


col3 <- colorRampPalette(c("purple","blue","white", "red", "orange"))
# corpl <- corrplot(cor_mt, method="color",  
#          type="full", order="hclust",
#          sig.level = 0.05, insig = "blank", col=col3(200))

### heatmaply------------------
# BrBG <- colorRampPalette(brewer.pal(11, "BrBG"))
BrBG <- colorRampPalette(c("purple","blue","white", "red", "orange"))
hm <- heatmaply(cor_mt, margins = c(20,50,0,0),
          k_col = 2, k_row = 2,
          colors = BrBG,
          dendrogram = "none",
          label_names=c("row", "column", "Corr."),
          column_text_angle = 60,
          fontsize_row=5, fontsize_col=5,
          limits = c(-1,1))
