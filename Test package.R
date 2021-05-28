library(SEE)
library(dplyr)
library(ggplot2)

#Using an existing dataset
data("sim_disp_apixaban")
sim_disp_apixaban<- sim_disp_apixaban[,c("person_id","disp_date")]
sim_disp_apixaban <- SEE(sim_disp_apixaban, person_id, disp_date)

sim_disp_apixaban <- sim_disp_apixaban %>%
  group_by(person_id) %>%
  arrange(person_id, disp_date) %>%
  dplyr::mutate(prev_disp_date2 = dplyr::lag(disp_date, n=1, default = NA))

sim_disp_apixaban$duration <- sim_disp_apixaban$disp_date - sim_disp_apixaban$prev_disp_date2

sim_disp_apixaban <- sim_disp_apixaban %>%
  group_by(person_id) %>%
  arrange(person_id, disp_date) %>%
  dplyr::mutate(p_number = seq_along (disp_date))

sim_disp_apixaban$p_number <- as.factor(sim_disp_apixaban$p_number)

per <- ecdfplot(~sim_disp_apixaban$duration)
x <- per$panel.args[[1]]$x
ecdfs <- lapply(split(sim_disp_apixaban$duration, 1), ecdf)
y <- sapply(ecdfs, function(e) e(sim_disp_apixaban$duration))
y <- as.vector(y)
dfper <- cbind(x,y)
dfper <- as.data.frame(dfper)
dfper <- dfper[which(dfper$y<=0.80),]
a <- as.numeric(max(dfper$x))

dfper <- sim_disp_apixaban[which(sim_disp_apixaban$duration<=a),]

b <- as.data.frame(tapply(dfper$duration, dfper$p_number, FUN=median))

b$prev <- lag(b$`tapply(dfper$duration, dfper$p_number, FUN = median)`) 
b$delta <- ((b$`tapply(dfper$duration, dfper$p_number, FUN = median)`-b$prev)/b$prev)*100


#Plot needed -> show all the values in x
plot(abs(b$delta),ylim=c(0,20), xlab="Prescription number", ylab="Delta duration")
abline(h=10) #here the user should able to select h

#Plot needed
ggplot(dfper, aes(x=p_number, y=duration, fill=p_number)) + 
  geom_boxplot() 

dfper$cluster <- as.factor(dfper$cluster)

#Plot needed
ggplot(dfper, aes(x=cluster, y=duration, fill=cluster)) + 
  geom_boxplot() 

# #Using another simulated dataset
# eksd <- sample(seq(as.Date('1995-01-01'), as.Date('2020-12-01'), by="week"), 1000)
# pnr <- as.factor(rep(1:10, each=100))
# temp <- "PID"
# atc <-  "B01XX01"
# mydf <- as.data.frame(cbind(eksd, pnr))
# mydf$pnr<- with(mydf, paste0(temp, pnr))
# mydf$eksd <- as.Date(eksd, "%YYYY/%mm/%dd")
# packsize <- rep(c(25,50,100), times = nrow(mydf))
# a <- as.numeric(nrow(mydf))
# packsize <- packsize[1:a]
# apk <- round(runif(nrow(mydf), min=1, max=2))
# strnum <- rep(c(25,50,75,100, 150, 200), times = nrow(mydf))
# strnum <- strnum[1:a]
# 
# mydf <- as.data.frame(cbind(mydf, atc, packsize, apk, strnum))
# 
# mydf <- mydf[,c("pnr","eksd")]
# 
# mydf <- SEE(mydf, pnr, eksd)
