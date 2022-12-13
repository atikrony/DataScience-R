page = read_html(https://www.forbes.com/lists/global2000/?sh=770441b35ac0)

companyname = page %>% html_nodes(".name") %>% html_text()
country = page %>% html_nodes(".country") %>% html_text()
sales = page %>% html_nodes(".sales") %>% html_text()
profit = page %>% html_nodes(".profit") %>% html_text()
assets = page %>% html_nodes(".assets") %>% html_text()
marketvalue = page %>% html_nodes(".value") %>% html_text()

##writing csv file
forbes2000 = data.frame(companyname,country,sales,profit,assets,marketvalue, stringsAsFactors = FALSE)
write.csv(forbes2000, "forbes2000.csv")




forbes2000$sales<-gsub("\\$","",as.character(forbes2000$sales))
forbes2000$sales<-gsub("B","",as.character(forbes2000$sales))
forbes2000$sales<-gsub("M","",as.character(forbes2000$sales))


forbes2000$profit<-gsub("\\$","",as.character(forbes2000$profit))
forbes2000$profit<-gsub("B","",as.character(forbes2000$profit))
forbes2000$profit<-gsub("M","",as.character(forbes2000$profit))


forbes2000$assets<-gsub("\\$","",as.character(forbes2000$assets))
forbes2000$assets<-gsub("B","",as.character(forbes2000$assets))
forbes2000$assets<-gsub("M","",as.character(forbes2000$assets))

forbes2000$marketvalue<-gsub("\\$","",as.character(forbes2000$marketvalue))
forbes2000$marketvalue<-gsub("B","",as.character(forbes2000$marketvalue))
forbes2000$marketvalue<-gsub("M","",as.character(forbes2000$marketvalue))
write.csv(forbes2000, "C:\\Users\\atikr\\Documents\\newforbes2000.csv", row.names=FALSE)


marketvalue <- as.numeric(factor(forbes2000$marketvalue))
sales <- as.numeric(factor(forbes2000$sales))
forbes2000$marketvalue<- as.numeric(factor(forbes2000$marketvalue))
forbes2000$sales <- as.numeric(factor(forbes2000$sales ))
forbes2000$assets <- as.numeric(factor(forbes2000$assets ))
forbes2000$profit <- as.numeric(factor(forbes2000$profit ))
write.csv(forbes2000, "C:\\Users\\atikr\\Documents\\newforbes2000.csv", row.names=FALSE)


forbes2000


 for(i in 1:nrow(forbes2000)){
  if(forbes2000$marketvalue[i] <= 0300 ){
    forbes2000$Type[i] <- "Small valued company"
  }else if(forbes2000$marketvalue[i] <= 0600){
    forbes2000$Type[i] <- "Medium valued company"
  }else if(forbes2000$marketvalue[i] <= 1000){
    forbes2000$Type[i] <- "Big company"
  }else {forbes2000$Type[i] <- "Giant company"}
}

forbes2000


class(forbes2000$Type)


forbes2000$catagory[forbes2000$Type=="Small valued company"] <- "1"
forbes2000$catagory[forbes2000$Type=="Medium valued company"] <- "2"
forbes2000$catagory[forbes2000$Type=="Big company"] <- "3"
forbes2000$catagory[forbes2000$Type=="Giant company"] <- "4"

write.csv(forbes2000, "C:\\Users\\atikr\\Documents\\finalnewforbes2000.csv", row.names=FALSE)

mean(forbes2000$profit)
median(forbes2000$profit)
mode(forbes2000$profit)
max(forbes2000$profit)-mean(forbes2000$profit)
var(forbes2000$profit)
sd(forbes2000$profit)
quantile(forbes2000$profit)
IQR(forbes2000$profit)
summary(forbes2000)


X1 <- c(forbes2000$profit)
total_mode <- function(x) { 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
total_mode(X1)

#Histogram
hist(forbes2000$sales)
hist(forbes2000$profit)
hist(forbes2000$assets)
hist(forbes2000$marketvalue)

#ggplot visualize
forbes2000=data.frame(forbes2000$profit) 
str(forbes2000)
install.packages("ggplot2")
library(ggplot2)
ggplot(forbes2000, aes(forbes2000$companyname, forbes2000$profit))+
  geom_point(size = 3)


