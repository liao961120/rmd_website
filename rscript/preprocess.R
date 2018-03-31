library(readr)
library(dplyr)
library(tidyr)
library(stringr)
var_info <- read_rds("./data/item_construct.rds") %>% arrange(construct_en)
psy_test <- read_rds("./data/psy_test_parsed.rds")


# reverse scoring
rs <- var_info$vars_en[var_info$type=="R"] #items needed reverse scoring
rs <- rs[!is.na(rs)]
for(i in rs) {
    psy_test[,i] <- 6 - psy_test[,i]
}


# construct & total score
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
    
# add new variable to var_info: construct-number 
num <- c(paste("0",1:9,sep=""), "10", 11:20)
con_num <- c(num[1:length(AT_item)], 
             num[1:length(CF_item)],
             num[1:length(Fk_item)],
             num[1:length(HD_item)],
             num[1:length(RF_item)], 
             rep(NA, nrow(var_info)-62))
 
item_code <- var_info[,2:3]
item_code <- cbind(con_num,item_code) %>%
    unite(con_num, construct_en, con_num, sep = "-") %>%
    mutate(con_num = ifelse(con_num == "NA-NA", str_replace(con_num, "NA-NA", "NA"), con_num))
    
var_info <- left_join(var_info, item_code, by="vars_en") %>%
        select(vars_en, construct_en, con_num, question, everything())
    
# add new variables: construct total scores
psy_test <- psy_test %>%
    mutate(AT=rowSums(psy_test[,AT_item])) %>%
    mutate(CF=rowSums(psy_test[,CF_item])) %>%
    mutate(HD=rowSums(psy_test[,HD_item])) %>%
    mutate(RF=rowSums(psy_test[,RF_item])) %>%
    mutate(Fk=rowSums(psy_test[,Fk_item])) %>%
    mutate(total=rowSums(psy_test[,c(AT_item,CF_item,HD_item,RF_item)]))

# wirte data
write_rds(psy_test, "./data/psy_test_analy.rds")
write_rds(var_info, "./data/item_construct_conNum.rds")