library(readr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidytext)
library(readxl)
library(lmerTest)
library(lme4)
library("sjPlot")
library("sjmisc")
library("sjlabelled")

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}
##### EXP 1 #####

# get raw data
javascript_results_exp1 <- read.pcibex("exp1-results.csv")
javascript_results_exp1_demo <- read.pcibex("exp1-results-demo.csv")

# filter only the experimental items & ratings and rbind the two dfs
df1 <- javascript_results_exp1 %>% filter(Label == "Experiment" & PennElementType == "Scale") 
df2 <- javascript_results_exp1_demo %>% filter(Label == "Experiment" & PennElementType == "Scale") 
df_exp1 <- rbind(df1,df2)

# data type converter
df_exp1$Value %<>%  as.numeric()
df_exp1$ECM %<>% as.factor() 

# checking if all participants completed the experiment
sumcontrol <- df_exp1 %>% 
  group_by(MD5.hash.of.participant.s.IP.address) %>% 
  summarise(number = n())

# summary
sum_rating_exp1 <- df_exp1 %>% 
  group_by(License.Type,ECM) %>% 
  summarize(rating = mean(Value))

# plot the data
p1 <- sum_rating_exp1 %>% ggplot(aes(License.Type,rating, fill = ECM)) + geom_bar(stat="identity",position="dodge") +
  labs(title = "Grammatical judgement test results by NPI licensing type and ECM", 
       x = "Licensing Type", y = "Grammaticality") +
  scale_y_continuous(limits = c(0,5)) +
  theme(text=element_text(family="Times New Roman", size=17))
ggsave("p1.png",p1, height=5.75, width=9)

##### Exp 2 #####

# get raw data
javascript_results_exp2 <- read.pcibex("exp2-results.csv")
javascript_results_exp2_demo <- read.pcibex("exp2-results-demo.csv")

# filter only the experimental items & ratings and rbind the two dfs
df1 <- javascript_results_exp2 %>% filter(Label == "Experiment" & PennElementType == "Scale") 
df2 <- javascript_results_exp2_demo %>% filter(Label == "Experiment" & PennElementType == "Scale") 
df_exp2 <- rbind(df1,df2)

# data type converter
df_exp2$Value %<>%  as.numeric()
df_exp2$nominalizer %<>% as.factor() 

# checking if all participants completed the experiment
sumcontrol2 <- df_exp2 %>% 
  group_by(MD5.hash.of.participant.s.IP.address) %>% 
  summarise(number = n())

# summary
sum_rating_exp2 <- df_exp2 %>% 
  group_by(License.Type,nominalizer) %>% 
  summarize(rating = mean(Value))

# plot the data
p2 <- sum_rating_exp2 %>% ggplot(aes(License.Type,rating, fill = nominalizer)) + geom_bar(stat="identity",position="dodge") +
  labs(title = "Grammatical judgement test results by NPI licensing and nominalizer type", 
       x = "Licensing Type", y = "Grammaticality") +
  scale_y_continuous(limits = c(0,5)) + 
  theme(text=element_text(family="Times New Roman", size=17))
p2
ggsave("p2.png",p2, height=5.75, width=9)

# statistical tests

# exp 1
df_exp1$Value %<>% as.numeric() 

m <- lmer(Value ~ License.Type + ECM + (1|MD5.hash.of.participant.s.IP.address) + (1|Item), data = df_exp1)
summary(m)

# plot model
model_plot1 <- plot_model(m, y.offset = .4)
model_plot1 <- model_plot1 + theme(text=element_text(family="Times New Roman", size=17))
ggsave("model_plot1.png",model_plot1)

# exp 2
df_exp2$Value %<>% as.numeric() 

m2 <- lmer(Value ~ License.Type + nominalizer + (1|MD5.hash.of.participant.s.IP.address) + (1|Item), data = df_exp2)
summary(m2)

# plot model
model_plot2 <- plot_model(m2, y.offset = .4)
model_plot2 <- model_plot2 + theme(text=element_text(family="Times New Roman", size=17))
ggsave("model_plot2.png",model_plot2)

# post-hoc item analysis
sum1 <- df_exp1 %>% group_by(Item,Group,License.Type, ECM) %>% 
  summarize(rating = mean(Value))
sum2 <- df_exp2 %>% group_by(Item,Group,License.Type, nominalizer) %>% 
  summarize(rating = mean(Value))
