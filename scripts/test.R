# install openxlsx
install.packages("openxlsx") 
# load library
library(openxlsx)
library(dplyr)
library(dslabs)
options(digits = 3)

# load heights data from  dslabs
data(heights)

# calculate mean, SD, quantile
mean(heights$height)
sd(heights$height)
quantile(heights$height)
summary(heights)

# The Shapiro-Wilk Test For Normality
shapiro.test(heights$height)

# seperate the data by sex
# %>% is pipe
female <- heights %>% filter(sex =="Female" )
male <- heights %>% filter(sex =="Male" ) 

# calculate mean, SD, quantile
mean(male$height)
mean(female$height)

sd(male$height)
sd(female$height)

summary(male$height)
summary(female$height)


# The Shapiro-Wilk Test For Normality
shapiro.test(male$height)
shapiro.test(female$height)

# Summary
sex  <- c("Male", "Female", "Total")
count <- c(length(male$height),length(female$height), length(heights$height) )
mean <- c(round(mean(male$height), digits = 2), round(mean(female$height), digits = 2), round (mean(heights$height), digits = 2))
SD <- c(round(sd(male$height), digits = 2), round(sd(female$height), digits = 2), round(sd(heights$height), digits = 2))

my.table <- cbind(Sex = sex, Count = count, Mean = mean, SD = SD)
my.table


# Make a function (this is better) 
mySummary <- function(x){
  return(c( Count = length(x), Mean = round(mean(x), digits = 2), SD = round(sd(x), digits = 2))) 
}

male.sum <- mySummary(male$height)
female.sum <- mySummary(female$height)
total.sum <- mySummary(heights$height)
my.table2 <- rbind(Male = male.sum, Female = female.sum, Total = total.sum )
my.table2

# write to xlsx file
write.xlsx(my.table2, file = "table2.xlsx", colNames = T, rowNames = T)

#  dotplot
ggplot(data=heights, aes(x = sex, y = height)) +  
  geom_dotplot(binaxis='y', stackdir='center', stackratio=0.5, dotsize=0.3)