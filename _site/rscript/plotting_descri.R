library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales) # for ggplot color
library(plotly)

psy_test_f <- read_rds("./data/psy_test_filtered.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")


## pie chart-------------------------
blank_theme <- theme_minimal()+
    theme(
        plot.subtitle=element_text(size=7, face="plain"),
        plot.caption=element_text(size=7, face="plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )

gender <- psy_test_f %>%
    mutate(gender= fct_recode(gender,
            "Female"="female",
            "Male"="male",
            "Other"="other")) %>%
    count(gender)

hue <- hue_pal()(3)

gender <- ggplot(gender, aes(x="", y=n, fill=gender))+
    geom_bar(width = 1, stat="identity")+
    scale_y_continuous(breaks = NULL)+
    coord_polar("y", start=0) +
    scale_fill_manual(values=c(hue[1], hue[3], "black"))+
    blank_theme+
    geom_text(aes(y = c(150, 43, 218), # theta(clockwise: number stands for counts, not degrees)
                  x = c(1.15, 1.2, 1.58), # polar axis length
              label = percent(n/sum(n))), size=3)+
    labs(title="Gender Composition", fill="Gender", subtitle =paste(" Samples: ",sum(gender$n), sep=""))


## grade---------------------
grade <- psy_test_f %>%
    mutate(grade= fct_recode(grade,
            "Second"="second",
            "Third"="third",
            "Forth*"="forth",
            "Graduated*"="graduated",  
            "Others"="NoDegree",
            "Others"="suspend")) %>%
    mutate(grade = grade %>% fct_infreq()) %>% # order factor by freq
    ggplot(aes(x=grade, fill=grade))+
    geom_bar(show.legend=F)+
    theme(
        plot.subtitle=element_text(size=7, face="plain"),
        plot.caption=element_text(size=7, face="plain",vjust=0),
        axis.text.x=element_text(size=7, face="plain"),
        axis.text.y=element_text(size=7, face="plain"),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"),
        plot.title=element_text(hjust=0, size=14, face="bold"))+
    labs(x="", y="Count",title="Grade Composition", fill="Grade", subtitle=paste(" Samples: ",sum(nrow(psy_test_f)),sep=""), caption='Forth: Grades above four included. \n Graduated: Grad school students included.')

## college---------------------
# levels(fct_lump(psy_test_f$college %>% fct_infreq(), n=9))

college <- psy_test_f %>%
    mutate(college = college %>% fct_infreq()) %>% # order factor by freq
    mutate(college = fct_lump(college, n=9)) %>%
    mutate(college = fct_recode(college,"其它"="Other")) %>%
    ggplot(aes(x=college, fill=college))+
    geom_bar(show.legend=T)+
    theme(
        legend.key.size=unit(0.7, "line"),
        legend.text=element_text(size=7, face="plain"),
        legend.title=element_text(face="bold"),
        plot.subtitle=element_text(size=7, face="plain"),
        plot.caption=element_text(size=7, face="plain"),
        axis.text.x=element_text(size=7, face="plain"),
        axis.text.y=element_text(size=7, face="plain"),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"),
        plot.title=element_text(hjust=0, size=14, face="bold"))+
    scale_x_discrete(labels = c("臺大","清大","中原","臺師大","輔大","中國醫","臺藝大","交大","成大","其它"))+
    labs(x="", y="Count \n ",title="College Composition", fill="", subtitle=paste(" Samples: ",sum(nrow(psy_test_f)), sep=""), caption=" ")


## dept---------------------
dept <- psy_test_f %>%
    mutate(dept = dept %>% fct_infreq()) %>% # order factor by freq
    mutate(dept = fct_lump(dept, n=12)) %>%
    mutate(dept = fct_recode(dept,"其它"="Other")) %>%
    ggplot(aes(x=dept, fill=dept))+
    geom_bar(show.legend=T)+
    theme(
        legend.key.size=unit(0.7, "line"),
        legend.text=element_text(size=7, face="plain"),
        legend.title=element_text(face="bold"),
        plot.subtitle=element_text(size=7, face="plain"),
        plot.caption=element_text(size=7, face="plain"),
        axis.text.x=element_text(size=7, face="plain"),
        axis.text.y=element_text(size=7, face="plain"),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"),
        plot.title=element_text(hjust=0, size=14, face="bold"))+
    scale_x_discrete(labels = c("心理","管理","工程","外文","社會","藝術","傳播","醫學","經濟","教育","生物","法律","治療","資訊","其它"))+
    labs(x="", y="Count \n ",title="Department Composition", fill="", subtitle=paste(" Samples: ",sum(nrow(psy_test_f)), sep=""))


