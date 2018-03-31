library(readr)
library(dplyr)
library(psych)
library(e1071) # for skewness calculation
library(ggplot2)

psy_test <- read_rds("./data/psy_test_analy.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")

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

# Filtering
## Grade: first -> Fk: gradient
psy_test_f <- psy_test %>%
    filter(grade != "first") %>%
    filter(Fk <= 18) 

# Cronbach's alpha--------------
All_data <- psy_test_f %>%
    select(AT_item,CF_item,HD_item,RF_item)
AT_data <- psy_test_f %>%
    select(AT_item)
CF_data <- psy_test_f %>%
    select(CF_item)
HD_data <- psy_test_f %>%
    select(HD_item)
RF_data <- psy_test_f %>%
    select(RF_item)
Fk_data <- psy_test_f %>%
    select(Fk_item)

all_alpha <- psych::alpha(All_data)$total$std.alpha
AT_alpha <- psych::alpha(AT_data)$total$std.alpha
CF_alpha <- psych::alpha(CF_data)$total$std.alpha
HD_alpha <- psych::alpha(HD_data)$total$std.alpha
RF_alpha <- psych::alpha(RF_data)$total$std.alpha
Fk_alpha <- psych::alpha(Fk_data)$total$std.alpha

## construct corr--------------------
construct_score <- psy_test_f %>%
    select(AT, CF, HD, RF, total)
construct_cor_mt <- round(cor(construct_score), 2) # rounded corr matrix


## delete-self alpha--------------
item_stats <- var_info[1:62, -c(4,5,6)] %>% filter(construct_en != "F")
item_stats <- cbind(item_stats,c(rep(0, nrow(item_stats))),c(rep(0, nrow(item_stats))) )
colnames(item_stats) <- c("item","construct","code","alpha_ori","alpha")

for (i in item_stats$item){
    if (item_stats[item_stats$item==i, "construct"] == "AT"){
        index <- which(colnames(AT_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- AT_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(AT_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "CF"){
        index <- which(colnames(CF_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- CF_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(CF_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "HD"){
        index <- which(colnames(HD_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- HD_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(HD_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "RF"){
        index <- which(colnames(RF_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- RF_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(RF_data[,-index])$total$std.alpha
    }
}

## stats: mean, sd, skewness, H-L, corr --------------
item_data <- psy_test_f %>%
    select(AT_item,CF_item,HD_item,RF_item,AT,CF,HD,RF,total)

item_mean <- sapply(item_data, mean)
item_sd <- sapply(item_data, sd)
item_skew <- sapply(item_data, skewness)

### H-L------------------------
item_data_H <- item_data %>%
    filter(total >= quantile(item_data$total, p=.725))
item_data_L <- item_data %>%
    filter(total <= quantile(item_data$total, p=.275))
item_mean_H <- sapply(item_data_H, mean)
item_mean_L <- sapply(item_data_L, mean)


### corr with construct--------------------
item_stats <- item_stats %>% mutate(corr=1)
for (i in item_stats$item){
    if (item_stats[item_stats$item==i, "construct"] == "AT"){
        item_stats[item_stats$item==i, "corr"] <- 
            cor(psy_test_f[,i], psy_test_f$AT)
    }
    else if (item_stats[item_stats$item==i, "construct"] == "CF"){
        item_stats[item_stats$item==i, "corr"] <- 
            cor(psy_test_f[,i], psy_test_f$CF)
    }
    else if (item_stats[item_stats$item==i, "construct"] == "HD"){
        item_stats[item_stats$item==i, "corr"] <- 
            cor(psy_test_f[,i], psy_test_f$HD)
    }
    else if (item_stats[item_stats$item==i, "construct"] == "RF"){
        item_stats[item_stats$item==i, "corr"] <- 
            cor(psy_test_f[,i], psy_test_f$RF)
    }
}

### combine----------------
row_name <- names(item_data)

stats <- as.data.frame(cbind(item_mean, item_sd, item_skew, item_mean_H, item_mean_L)) %>%
    rename(mean=item_mean, sd=item_sd, skew=item_skew, H=item_mean_H, L=item_mean_L) %>%
    mutate(Diff = H - L) %>%
    mutate(var=row_name) %>%
    left_join(item_stats, by=c("var"="item")) %>%
    select(var, construct, code,everything(),-H,-L)




write_rds(psy_test_f, "./data/psy_test_filtered.rds")