# ==============================================================================
# ==============================================================================
# Sindhya Rajendra Babu
# Course: Data Mining /Winter Semester 2021
# Assignment on Clustering on Retail Data 
# ==============================================================================
# ==============================================================================


rm(list=ls())
while(!is.null(dev.list()))
{
  dev.off()
}                                                                               # Resetting R's brain and closing all active plots 

getwd()                                                                         # get the current working directory 

library(stats)
library(ggfortify)
library(dplyr)
library(tidyverse)
library(ggplot2)                                                                #loading required packages

GroceryData <- read.csv("Web_Baskets_2020.csv",header = TRUE, sep = ";")        #importing grocery data into R 
str(GroceryData)  
head(GroceryData)                                                               # viewing the first 6 records of the Grocery data 
tail(GroceryData)

Grocerydata_new <- subset(GroceryData, date != "2020-04-31" & date != "2020-05-01")
Date_Arrange <-  Grocerydata_new %>% 
  arrange(desc(date))                                                           #Arranging by date to check if see data from Apr 1st to May 1st 

head(Date_Arrange,20)
tail(Date_Arrange)

Cat_name <-  unique(Grocerydata_new$article_cat)                                    #To check the unique article category names   
view(Cat_name)


#-------------------------------------------------------------------------------
#TASK 1.a : To find the five best-selling categories 
#-------------------------------------------------------------------------------

AvgQuan <- Grocerydata_new %>%                                                      #To display the top 5 best selling articles 
  group_by(article_cat) %>%                                                     
  summarise(monthly_sales = sum(quantity)) %>% 
  arrange(desc(monthly_sales))  %>% 
  top_n(5, monthly_sales) 

view(AvgQuan)                                                        

barplot(AvgQuan$monthly_sales, names.arg = AvgQuan$article_cat,                 #barplot of top 5 categories             
         main = "Monthly sales of top five best selling categories",col="blue",
         ylim = c(0,400000))

#using ggplot
ggplot(data=AvgQuan)+
  geom_bar(mapping=aes(x=article_cat,y=monthly_sales),stat = 'identity',fill = "#08306b")+
  scale_y_continuous(labels = scales::format_format(big.mark = ",",scientific = FALSE))+
  labs(x="Article Category", y="Number of items sold", title="April Sales: Top five best selling categories")+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20))+
  coord_flip()
  


data <- aggregate(quantity ~ article_cat + article_name, Grocerydata_new, sum)      #cross check using aggregate function 



#-------------------------------------------------------------------------------
#TASK 1.b : Visualization of five best selling categories 
#-------------------------------------------------------------------------------


GroData_mod <- filter(Grocerydata_new, article_cat                              #filtering the best 5 articles
                      %in% c('pants', 'shirts', 'underpants', 
                             'bras', 'nightgown/pajamas'))

data_top5 <- aggregate(quantity ~ date + article_cat + article_name,            #aggregating or grouping by date,article category and name
                       GroData_mod, sum)    

data_top5_mod <- data_top5 %>%                                                  #displays the best article of five categories
  group_by(article_cat) %>%
  arrange(desc(quantity)) %>%
  slice(1:1)                                                                    #selects the first row from each article category


GroData_new <- filter(GroData_mod, article_name %in% 
                        c('women underwired bra','women nightgown', 
                          'women panties','women trousers','women shirt'))      #Filtering best selling article from 5 categories

Data_vis <- mutate(GroData_new, days_April=substr(date, start=9,stop=10))       #creating a new attribute days_April

ggplot(data = Data_vis)+                                                        #Using ggplot for visualisation
  geom_bar(mapping= aes(x= days_April, y= quantity, color= article_name), 
           stat = 'identity')+
  facet_wrap(~article_cat, nrow =3)+
  labs(x="April Month 2020", y="Number of items sold", 
       title="Best selling Article from Top 5 categories respectively")+
  theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 20))

#-------------------------------------------------------------------------------
#To visualise the best sales/least sales of the correspoding week days
#-------------------------------------------------------------------------------

df_days <- Grocerydata_new %>%                                                  #creating week days
  mutate(days=weekdays(as.Date.character(date))) %>%
  select(days,article_cat,article_name,quantity)

overall_sales <- df_days %>%
  summarise(overall= sum(quantity))                                             #overall sales of all articles combined


df_weekly <- df_days %>%                                                        #weekly sales of articles
  group_by(days) %>%
  summarise(tot_sales_per_weekday= sum(quantity), 
            prop_sales=tot_sales_per_weekday/overall_sales)

df_art_week <- df_days %>%                                                      #grouping by articles cat, names and days 
  group_by(days,article_cat,article_name) %>%
  summarise(art_sales_per_weekday= sum(quantity))                                                                                


ggplot(data=df_art_week)+                                                       #visualisation of best day sales in April
  geom_bar(mapping = aes(x=days, y=art_sales_per_weekday, fill = days),
           stat = "identity")+
  scale_y_continuous(labels = scales::format_format(big.mark = ",",scientific = FALSE))+
  labs(x = "Days of week", y = "Number of items sold",
       title = "Total items sales by week days") +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5))+
  coord_flip()
  
#to check overall sales percentage by categories
ccc <- df2 %>%
  select(article_cat,quantity) %>%
  group_by(article_cat) %>%
  summarise(tot_items_sold=sum(quantity),percentage_sales=tot_items_sold/3453335*100)

#-------------------------------------------------------------------------------
#TASK 2.a. Extracting new attributes for clustering analysis
#-------------------------------------------------------------------------------

grocery_one <- Grocerydata_new %>%                                              
  group_by(article_cat,article_name) %>%
  summarise(TotItems_sold=sum(quantity), AvgItems_sold_perday=TotItems_sold/30) %>%
  arrange(desc(AvgItems_sold_perday))


d <- density(grocery_one$AvgItems_sold_perday)
plot(d, col="blue", main="Distribution of average sales/day",                   #plotting the distribution of avg items per day using kernel density plot
     xlab = "Average sales per day", lwd=2,
     cex.lab = 1,
     cex.main = 2,
     cex.sub = 2)

            
grocery_two <- left_join(df_art_week,df_weekly)                                 

grocery_three <- grocery_two %>%
  mutate(avg_additional_units= tot_sales_per_weekday-art_sales_per_weekday)


de <- density(grocery_three$avg_additional_units)                               #plotting distribution of avg additional units 
plot(de,col="blue", main="Distribution of Average additional units",
     xlab = "Additional Items in basket", lwd=2,
     cex.lab = 1,
     cex.main = 2,
     cex.sub = 2)



df <- left_join(df_art_week,df_weekly)                                          

df1 <- df %>%
  mutate(Prop_sales_per_weekday =art_sales_per_weekday/tot_sales_per_weekday)

df2 <- aggregate(quantity ~ article_cat + article_name, Grocerydata_new, sum) 

df3 <- left_join(df1,df2)

df4 <- df3 %>%                                                                  #data for clustering
  mutate(avg_sales_perday= quantity/30, additional_units=tot_sales_per_weekday-art_sales_per_weekday)

#-------------------------------------------------------------------------------
#TASK 3. Finding clusters of articles
#-------------------------------------------------------------------------------

install.packages("factoextra")
library("factoextra")
#Using df4 (data contains 7 attributes) for clustering 

normalize <- function(x, na.rm = TRUE) {                                        # using min max normalization
  return((x- min(x)) /(max(x)-min(x)))
}

grocery_cluster <- as.data.frame(df4[4:10])                                     #preparing & scaling data
scaled_df4 <- normalize(grocery_cluster)


km.res <- kmeans(scaled_df4, 3, nstart = 20)                                    #computing k-means



fviz_cluster(km.res, data = scaled_df4,                                         #visualisation
             ggtheme = theme_minimal(),
             main = "Clustering with 7 attributes"
)

# data for clustering with 3 attributes
df5 <- Grocerydata_new %>%
  group_by(article_cat,article_name) %>%
  summarise(tot_qty=sum(quantity), avg_qty=tot_qty/30, prop_sales=tot_qty/overall_sales)

#data for clustering with 7 attributes
df4 <- df3 %>%                                           
  mutate(avg_sales_perday= quantity/30, additional_units=tot_sales_per_weekday-art_sales_per_weekday)

