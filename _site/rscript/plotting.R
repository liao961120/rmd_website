library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cowplot)
library(scales) # for ggplot color
library(corrplot)
library(RColorBrewer)
# library(heatmaply)

psy_test <- read_rds("./data/psy_test_analy.rds")
psy_test_f <- read_rds("./data/psy_test_filtered.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")

## plotting theme ----------------
theme <- theme(
    plot.subtitle = element_text(size = 9, face = "bold"),
    plot.caption = element_text(size = 7, face = "plain", vjust = 0),
    axis.text.x = element_text(size = 7, face = "plain"),
    axis.text.y = element_text(size = 7, face = "plain"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0, size = 14, face = "bold"))

## Fk: gradient-------------------------
Fk_cutoff <- tibble(i=15:30,
                    AT=rep(0,length(15:30)),
                    CF=rep(0,length(15:30)),
                    HD=rep(0,length(15:30)),
                    RF=rep(0,length(15:30)),
                    total=rep(0,length(15:30))
)
for (i in 15:30) {
    psy_test1 <- psy_test %>%
        filter(grade != "first") %>%
        filter(Fk <= i)
    Fk_cutoff$AT[i-14] <- cor(psy_test1$Fk,psy_test1$AT)
    Fk_cutoff$CF[i-14] <- cor(psy_test1$Fk,psy_test1$CF)
    Fk_cutoff$HD[i-14] <- cor(psy_test1$Fk,psy_test1$HD)
    Fk_cutoff$RF[i-14] <- cor(psy_test1$Fk,psy_test1$RF)
    Fk_cutoff$total[i-14] <- cor(psy_test1$Fk,psy_test1$total)
}

pl_cutoff <- ggplot(Fk_cutoff)+
    geom_line(mapping = aes(x=i,y=AT,color="AT"),size=1)+
    geom_line(mapping = aes(x=i,y=CF,color="CF"),size=1)+
    geom_line(mapping = aes(x=i,y=HD,color="HD"),size=1)+
    geom_line(mapping = aes(x=i,y=RF,color="RF"),size=1)+
    geom_line(mapping = aes(x=i,y=total,color="total"),size=1)+
    scale_y_continuous(breaks = seq(0.05,0.4,by=0.05))+
    theme +
    scale_x_reverse(breaks=15:30)+
    labs(x="Cutoff", y="Corr. with Fake-Good", color="Construct")
# ggplotly(pl_cutoff)


## Distributions-------------------
plot_distr <- function(item, color="#F8766D"){
    x_axis <- as.character(unlist((psy_test_f[,item])))
    ggplot()+
        geom_bar(aes(x=x_axis, 
                     y=(..count..)/sum(..count..)),
                     fill=color)+
        scale_y_continuous(labels=scales::percent,
                           limits = c(0, 0.85))+
        scale_x_discrete(limits=c("1","2","3","4","5"))+
        theme +
        labs(x="", y="")
}

cl <- hue_pal()(4) ## ggplot default color generator 

### p_AT----------------------
AT_item <- unname(unlist(var_info[var_info$construct_en=="AT" & !is.na(var_info$construct_en),"vars_en"]))
p_AT <- plot_grid(plot_distr(AT_item[1]), plot_distr(AT_item[2]),
                  plot_distr(AT_item[3]), plot_distr(AT_item[4]),
                  plot_distr(AT_item[5]), plot_distr(AT_item[6]),
                  plot_distr(AT_item[7]), plot_distr(AT_item[8]),
                  plot_distr(AT_item[9]),
                  labels = unlist(var_info[var_info$construct_en=="AT","con_num"]), vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### p_CF1 p_CF2----------------------
CF_item <- unname(unlist(var_info[var_info$construct_en=="CF" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="CF" & !is.na(var_info$construct_en),"con_num"])
    
p_CF1 <- plot_grid(plot_distr(CF_item[1],cl[2]), plot_distr(CF_item[2],cl[2]),
                  plot_distr(CF_item[3],cl[2]), plot_distr(CF_item[4],cl[2]),
                  plot_distr(CF_item[5],cl[2]), plot_distr(CF_item[6],cl[2]),
                  plot_distr(CF_item[7],cl[2]), plot_distr(CF_item[8],cl[2]),
                  plot_distr(CF_item[9],cl[2]), plot_distr(CF_item[10],cl[2]),
                  plot_distr(CF_item[11],cl[2]), plot_distr(CF_item[12],cl[2]),
                  labels = label[1:12], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

p_CF2 <- plot_grid( plot_distr(CF_item[13],cl[2]), 
                   plot_distr(CF_item[14],cl[2]), plot_distr(CF_item[15],cl[2]), 
                   plot_distr(CF_item[16],cl[2]), plot_distr(CF_item[17],cl[2]),
                   labels = label[13:17], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### p_HD1 p_HD_2----------------------
item <- unname(unlist(var_info[var_info$construct_en=="HD" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="HD" & !is.na(var_info$construct_en),"con_num"])

p_HD1 <- plot_grid(plot_distr(item[1],cl[3]), plot_distr(item[2],cl[3]),
                   plot_distr(item[3],cl[3]), plot_distr(item[4],cl[3]),
                   plot_distr(item[5],cl[3]), plot_distr(item[6],cl[3]),
                   plot_distr(item[7],cl[3]), plot_distr(item[8],cl[3]),
                   plot_distr(item[9],cl[3]), plot_distr(item[10],cl[3]),
                   plot_distr(item[11],cl[3]), plot_distr(item[12],cl[3]),
                   labels = label[1:12], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

p_HD2 <- plot_grid(plot_distr(item[13],cl[3]), plot_distr(item[14],cl[3]),
                   plot_distr(item[15],cl[3]), plot_distr(item[16],cl[3]),
                   plot_distr(item[17],cl[3]), plot_distr(item[18],cl[3]),
                   plot_distr(item[19],cl[3]), plot_distr(item[20],cl[3]),
                   labels = label[13:20], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### p_RF----------------------
item <- unname(unlist(var_info[var_info$construct_en=="RF" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="RF" & !is.na(var_info$construct_en),"con_num"])

p_RF <- plot_grid(plot_distr(item[1],cl[4]), plot_distr(item[2],cl[4]),
                   plot_distr(item[3],cl[4]), plot_distr(item[4],cl[4]),
                   plot_distr(item[5],cl[4]), plot_distr(item[6],cl[4]),
                   plot_distr(item[7],cl[4]), plot_distr(item[8],cl[4]),
                   plot_distr(item[9],cl[4]), plot_distr(item[10],cl[4]),
                   labels = label[1:10], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

## correlation matrix(numeric)-------------------
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

# for (i in 1:nrow(cor_mt)){ # set diagonal from 1 to 0
#     cor_mt[i,i] <- 0        # to free
# }

## Correlation matrix plot: static
# col3 <- colorRampPalette(c("purple","blue","white", "red", "orange"))
# corrplot(cor_mt, method="color",  
#          type="full", order="hclust",
#          sig.level = 0.05, insig = "blank", col=col3(200))

