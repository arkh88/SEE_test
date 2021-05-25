####################
# SEE
####################
# 1st column= persion_id
# 2ncolumn = disp_date

library(dplyr)
load(file = "C:/KU/SEE/sim_disp_apixaban.rdata")

sim_disp_apixaban<-sim_disp_apixaban[,c("person_id","disp_date")]


#### Function Meta Data ###########

SEE<-function(data,id,date){
  options(warn=-1)
  options(scipen = 999999)
  library(plyr)
  library(dplyr)
  library(tableone)
  library(tidyverse)
  library(data.table)
  library(latticeExtra)
  library(factoextra)
  library(spatstat)
  

  
  names(data)<-c("person_id","disp_date")
  
  main_data<-data 
  
  test1 <- data %>%
    group_by(person_id) %>%
    arrange(disp_date) %>%
    filter(row_number()==1)
  
  test2 <- data %>%
    group_by(person_id) %>%
    arrange(disp_date) %>%
    filter(row_number()==n())
  
  test1 <- test1[,c("person_id","disp_date")]
  test2 <- test2[,c("person_id","disp_date")]
  
  colnames(test1) <- c("person_id","start.fu")
  colnames(test2) <- c("person_id","end.fu")
  
  test3 <- merge(test1, test2, by="person_id")
  
  data <- merge(data, test3, by="person_id")
  
  
  ############################
  data$test <- 0
  data$test <- ifelse(data$disp_date>= data$start.fu, ifelse(data$disp_date<= data$end.fu,1,data$test),data$test)
  data <- data[which(data$test==1),]
  
  data <- data %>%
    group_by(person_id) %>%
    arrange(person_id, disp_date) %>%
    dplyr::mutate(prev_disp_date = dplyr::lag(disp_date, n=1, default = NA))
  
  data$event.interval <- as.numeric(data$disp_date - data$prev_disp_date)
  DTa <- data[which(data$event.interval>=0),] 
  


per <- ecdfplot(~DTa$event.interval)
x <- per$panel.args[[1]]$x
ecdfs <- lapply(split(DTa$event.interval, 1), ecdf)
y <- sapply(ecdfs, function(e) e(DTa$event.interval))
y <- as.vector(y)
dfper <- cbind(x,y)
dfper <- as.data.frame(dfper)
dfper <- dfper[which(dfper$y<=0.8),]
# plot(x,y)
# plot(dfper$x,dfper$y)
a <- as.numeric(max(dfper$x))
DTa <- DTa[which(DTa$event.interval<=a),]

#Random sequence
set.seed(1234)
m2 <- DTa %>% group_by(person_id)
DTa <- sample_n(m2, 1)
d <- density(log(DTa$event.interval))
#plot(d)
x <- d$x
y <- d$y
z <- max(x)


a <- data.table(
  x=x,
  y=y
)

a <- scale(a)

# Silhouette method
a2 <- fviz_nbclust(a, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

max_cluster1 <- a2$data
max_cluster1<-as.numeric(max_cluster1$clusters[which.max(max_cluster1$y)])

mom <- a2

kmean_data<-as.data.frame(d$x,row.names = DTa$person_id)

clusters <- kmeans(kmean_data, max_cluster1)

Dta_clust<-cbind(d$x,cluster=clusters$cluster)
Dta_clust<-as.data.frame(Dta_clust)

Dta_clust$V1 <- round(exp(Dta_clust$V1),0)
Dta_clust <- unique(Dta_clust)
colnames(Dta_clust) <- c("event.interval","cluster")
DTa <- merge(DTa, Dta_clust, by="event.interval",all.x=T)

p1 <- as.data.frame(aggregate(DTa$event.interval, by=list(cluster=DTa$cluster), median))
colnames(p1) <- c("cluster","median_cluster")

DTa <- merge(DTa, p1, by ="cluster")
DTa2 <- DTa[,c(-2,-4)]
DTa2 <- DTa2 %>%
  group_by(person_id) %>%
  filter(row_number()==1)
main_data2 <- main_data
colnames(main_data2)[1] <- "person_id"
main_data2 <- merge(main_data2, DTa2, by ="person_id", all.x=T)
summary(main_data2$cluster)



xx<-as.data.frame(table(Dta_clust$cluster))
xx <- xx[which.max(xx$Freq),]
xx <- as.numeric(xx[1,1])
p2 <- p1[which(p1$cluster==xx),]
main_data2$median_cluster <- ifelse(is.na(main_data2$median_cluster), p2$median_cluster, main_data2$median_cluster)
main_data2$cluster <- ifelse(is.na(main_data2$cluster), max_cluster1+1, main_data2$cluster)

main_data2<-main_data2 %>% dplyr::select(-test)


return(main_data2)
}



