### PFDA TP066869 Eugene Winata


##Loac CSV File
#for PC
house_dataset = read.csv("C:\\Users\\user\\OneDrive - Asia Pacific University\\Y2\\Semester 3\\Programming for Data Analysis (PFDA)\\PFDA Assignment\\House_Rent_Dataset.csv", header = TRUE)
#for Laptop
#house_dataset = read.csv("C:\\Users\\Eugene Winata\\OneDrive - Asia Pacific University\\
#                         Y2\\Semester 3\\Programming for Data Analysis (PFDA)\\
#                         PFDA Assignment\\House_Rent_Dataset.csv", 
#                         header = TRUE)

house_dataset

View(house_dataset)



##Check Missing data (If there any)
sum(is.na(house_dataset))



##Install Library (Do Once!)
install.packages("tidyverse")
install.packages("ggridges")



#Library Package
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggridges)


#View dataset Detail
str(house_dataset)

remove(LocalPortion)

summary(house_dataset)

##Data Manipulation/Transformation
#Add Column 
house_dataset$month = format(as.Date(house_dataset$Posted.On, format = "%m/%d/%Y"), "%m")
house_dataset$date = format(as.Date(house_dataset$Posted.On, format = "%m/%d/%Y"), "%d")

MonthPortion = house_dataset %>% group_by(month) %>% summarise(Count = length(month))
CityPortion = house_dataset %>% group_by(City) %>% summarise(Count = length(City))


pie(CityPortion$Count, CityPortion$City, col = rainbow(length(CityPortion$Count)), main = "House Portion by Cities", clockwise = TRUE)
legend("topright", CityPortion$City, cex = 0.8, fill =rainbow(length(CityPortion$Count)))

#What are the factors that determine the Rent Value between 3000 to 1500000?
#1.1 Find the relation between rent value and house size from the given list
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Size, y = Rent ,col = month)) +
  geom_point(position = "jitter", shape = 1) +
  geom_smooth(method = lm, col = "red") +
  scale_fill_brewer(palette = "Accent") + 
  theme_dark() +
  theme(legend.position = "right", 
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) + 
  labs(title = "Scatter Plot Relationship for Rent Price and Size",
       caption = "based on the given dataset",
       x = "House Size",
       y = "Rent Price") + 
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Size, y = Rent )) + 
  geom_violin(col = "black", fill = "grey") + labs(title = "Violin") + 
  theme_dark() +
  theme(legend.position = "right", 
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) + 
  labs(title = "Violin Graph for Rent Price and Size",
       caption = "based on the given dataset",
       x = "House Size",
       y = "Rent Price") + 
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.2 Find the analysis of how the ranged rent value impact the quantity of BHK
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(BHK)) +
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "light blue", high = "blue") +
  geom_density(aes(y = ..count..), col = "grey", adjust = 4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Rent Price and Number of BHK", 
       caption = "based on the given dataset",
       x = "Number of BHK(Bedroom, Hall, Kitchen) ",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.3 How does city will be affected by the ranged rent value
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = City)) + 
  geom_bar(aes(x = City, fill = City), show.legend = FALSE, width = 1,col = "black") + 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_flip() + 
  coord_polar()+
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  labs(title = "Bar - Pie Chart for Rent Price and City", 
       caption = "based on the given dataset") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.4 Find the relationship between area type and the rent price
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Area.Type, fill = Area.Type)) + 
  geom_bar(width = 0.5, col = "black") +
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Area Type with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Area Type ",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ],
       aes(x = Rent, y = Area.Type, fill = Area.Type)) + 
  geom_density_ridges() +
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph for Area Type with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Rent Price ",
       y = "Area Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.5 Find the relation between ranged rent and number of bathrooms in the house
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Bathroom)) + 
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "yellow", high = "red") +
  geom_density(aes(y=..count..), colour="grey", adjust=4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram and Density Line\nfor Rent Price and Number of Bathroom", 
       caption = "based on the given dataset",
       x = "Number of Bathroom",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.6 How does ranged rent could impact the customer type
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Tenant.Preferred, fill = Tenant.Preferred)) + 
  geom_bar(col = "black", width = 0.5) +
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Customer Type with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Customer Tenant Type",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.7 Does the house provider have some impact on the ranged rent
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Point.of.Contact, fill = Point.of.Contact)) + 
  geom_bar(col = "black") +
  scale_fill_brewer(palette = "Accent") + 
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Contact Person Type with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Point of Contact",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#1.8 Find the relation for ranged house price and furnish status of the house
ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Furnishing.Status, fill = Furnishing.Status)) + 
  geom_bar(col = "black") +
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Furnishing Status with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Furnishing Status",
       y = "Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent > 3000 & house_dataset$Rent < 1500000, ], 
       aes(x = Rent, y = Furnishing.Status, fill = Furnishing.Status)) + 
  geom_boxplot() + 
  geom_violin(alpha = 0.3) + 
  scale_fill_brewer(palette = "Accent") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Boxplot and Violin Graph for Furnishing Status with \nRent Price > 3000 and Rent Price < 1500000 ", 
       caption = "based on the given dataset",
       x = "Rent",
       y = "Furnishing Status") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5)) 

#2 How to determine the house that can fill for 4 people or more?
#2.1 Find the relation between BHK amount more or equal to 4 and city
ggplot(house_dataset[house_dataset$BHK >= 4, ], 
       aes(x = City, fill = City)) + 
  geom_bar(col = "black") + 
  scale_fill_brewer(palette = "Dark2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for City and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$BHK >= 4, ], 
       aes(x = City)) + 
  geom_bar(aes(x = City, fill = City), show.legend = FALSE, width = 1,col = "black") + 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_flip() + 
  coord_polar() +
  scale_fill_brewer(palette = "Dark2") +
  theme_dark()+
  labs(title = "Bar-Pie Chart for City and BHK More or Equal to 4", 
       caption = "based on the given dataset") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  
#2.2 Find the analysis of how the preferred BHK impact to the house size
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = BHK, y = Size, col = BHK )) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Scatter Plot Diagram for Size and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "Number of BHK",
       y = "House Size") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  
ggplot(house_dataset[house_dataset$BHK >= 4, ], 
       aes(x = BHK, y = Size )) + 
  geom_violin(fill = "orange") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Violin Graph for Size and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "Number of BHK",
       y = "House Size") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$BHK >= 4, ], 
       aes(x = Size)) + 
  geom_histogram(color = "black", bins = 30 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "pink", high = "purple") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Size and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.3 Find the relationship between number of bathroom and 4 or more amount of BHK
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = Bathroom)) + 
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "yellow", high = "red") +
  geom_density(aes(y = ..count..), col = "black", adjust = 2.5) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Bathroom and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "Bathroom Amount",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.4 How does BHK amount could affect the customer tenant type
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = BHK, fill = Tenant.Preferred)) + 
  geom_bar(position = "dodge", col ="black") + 
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Customer Type and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.5 Find the relation between BHK amount and house furnish status
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = Furnishing.Status, fill = City)) + 
  geom_bar(position = "dodge", col = "black") + 
  scale_fill_brewer(palette = "Dark2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Furnishing Status and BHK More or Equal to 4\nFiltered by City", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.6 Does the rent price affected by preferred amount of BHK
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = Rent)) + 
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "light blue", high = "purple") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram Graph for Rent and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$BHK >= 4, ], 
       aes(x = BHK, y = Rent)) + 
  geom_violin(fill = "pink") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "vertical") +
  labs(title = "Violin Graph for Rent Price and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.7 Find the connection for area type and the amount of BHK is 4
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = Area.Type, y = BHK, fill = Area.Type)) + 
  geom_violin() +
  scale_fill_brewer(palette = "Dark2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Violin Graph for Customer Type and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "BHK Amount") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#2.8 Does the preferred BHK amount have some impact to the house contact person
ggplot(house_dataset[house_dataset$BHK >= 4, ],
       aes(x = BHK, fill = Point.of.Contact)) + 
  geom_bar(col = "black", position = "dodge") + 
  scale_fill_brewer(palette = "Dark2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Contact Person and BHK More or Equal to 4", 
       caption = "based on the given dataset",
       x = "BHK Quantity",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#3 An IT company are looking for houses for their new coming employees near their city company’s branch office with a budget of 600000 for it. What kind of factors could affect the house option?
#3.1 How does city location could be affected by the limited rent price and tenant type
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = City, fill = City)) + 
  geom_bar(col = "black") + 
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for City Amount and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  

#3.2 Find the connection between BHK amount and tenant type
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = BHK, y = month, fill = month)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot for BHK Quantity and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "Month") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = BHK, y = Tenant.Preferred, col = month)) +
  geom_jitter() +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for BHK Quantity and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "Month") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.3 Find the relation between bathroom quantity and house amount that available
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Bathroom)) + 
  geom_histogram(color = "black", bins = 30 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "yellow", high = "black") +
  geom_density(aes(y=..count..), colour="grey", adjust=4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram and Density Line for BHK Quantity and \nRent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Bathroom Amount",
       y = "Density House Quantity ") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.4 Does house contact person could be influenced by the provided company budget
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(y = Rent, x = Point.of.Contact, fill = Point.of.Contact)) + 
  geom_violin() + geom_boxplot(alpha = 0.4) +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Violin Graph and Box Plot for Unit Contact Person and \nRent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Contact Person",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.5 Find the relationship between furnish status and city
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Furnishing.Status, fill = City)) + 
  geom_bar(position = "dodge", col = "black") + 
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Furnishing Status and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Furnishing.Status, fill = City)) + 
  geom_bar(position = "dodge", col = "black") + 
  facet_grid(. ~ City) + 
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Furnishing Status and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Furnishing.Status, y = City , col = month)) + 
  geom_jitter() +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for Furnishing Status and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "City") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.6 How does the rent price have some impact on the area type
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Area.Type, y = Rent , fill = Area.Type)) + 
  geom_boxplot() + 
  geom_violin(alpha = 0.3) +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot & Violin Graph for Furnishing Status and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Area.Type, y = Rent , col = Area.Type)) + 
  geom_jitter() +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for Furnishing Status and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.7 Determine how the house size will affected by the budget for rent price
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Size, y = Rent,col = City)) + 
  geom_point() +
  scale_fill_brewer(palette = "Paired") + 
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Scatter Diagram for Size and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Size",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Size, y = Rent,col = City)) + 
  geom_point() + 
  geom_smooth() +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Scatter Diagram with Line for Size and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Size",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Size, y = Rent, col = City)) + 
  geom_point() +  
  geom_smooth() + 
  facet_grid(. ~ City) +
  scale_fill_brewer(palette = "Paired") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Scatter Diagram with Line for Size and Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Size",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#3.8 Find the relationship between budget and house amount that available
ggplot(house_dataset[house_dataset$Rent < 600000 & house_dataset$Tenant.Preferred == "Bachelors" ,], 
       aes(x = Rent)) + 
  geom_line(stat = "count") + 
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  labs(title = "Line Graph Quantity for Rent Below 600000 for Bachelors", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))



#4 What is the information that could be gathered from posted houses from April to July in 2022?
#4.1 Analyze the house quantity that posted between the interval months

MonthPortion = house_dataset %>% group_by(month) %>% summarise(Count = length(month))
MonthPortion$Percent = 
  100 * (MonthPortion$Count/sum(MonthPortion$Count))
MonthPortion$Percent = round(MonthPortion$Percent,2)

ggplot(MonthPortion, 
       aes(x = month, y = Count, col = month)) + 
  geom_line(group=1) + 
  geom_point() +
  scale_fill_brewer(palette = "Pastel1") + 
  theme_dark() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Quantity Line Graph based on Month ", 
       caption = "based on the given dataset",
       x = "Month",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(MonthPortion, 
       aes(x = "",y = Percent, fill = month)) + 
  geom_bar(stat = "identity", width = 1, col = "black") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Percent, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Pie Chart based Quantity per Month ", 
       caption = "based on the given dataset",
       x = "Percent",
       y = "") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.2 Find the relation between each specified month and house size that is available
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",], 
       aes(x = month, y = Size, fill = month)) + 
  geom_violin(alpha = 0.3) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot and Violin Graph based Quantity per Month ", 
       caption = "based on the given dataset",
       x = "Month",
       y = "Size") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.3 Does the city affected by the month of the posted house
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(x = City, fill = month)) + 
  geom_bar(position = "dodge", col = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Each Month based on City ", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.4 Find the relation of rent price and month of the post
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(x = Rent, fill = month)) + 
  geom_histogram() +
  facet_grid(month ~ .) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on House Rent Price per Month ", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(y = Rent, x = month, col = month)) +
  geom_jitter() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on House Rent Price per Month ", 
       caption = "based on the given dataset",
       x = "Posted Month",
       y = "Rent Price") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.5 How the tenant type amount affected by the specified month
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(x = Tenant.Preferred, fill = Tenant.Preferred)) + 
  geom_bar(col = "black", position = "dodge") +
  facet_grid(. ~ month) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart based on Tenant Type per Month ", 
       caption = "based on the given dataset",
       x = "Tenant Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.6 Determine how the furnishing status and month of house posted connected
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(x = Furnishing.Status, fill = month)) + 
  geom_histogram(stat = "count", col = "black", position = "dodge") + 
  scale_fill_brewer(palette = "Pastel1") + 
  theme_dark() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram Graph based on House Quantity per Month ", 
       caption = "based on the given dataset",
       x = "Month",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.7 Does the area type and month of the posted house have any relation
ggplot(house_dataset[house_dataset$month == "04" |
                       house_dataset$month == "05" |
                       house_dataset$month == "06" |
                       house_dataset$month == "07",],
       aes(x = month , fill = Area.Type)) + 
  geom_bar(col = "black") + 
  facet_grid(. ~ Area.Type) +
  scale_fill_brewer(palette = "Pastel1") + 
  theme_dark() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(colour = "black", size = 13, face = "bold"),
          legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
    labs(title = "Bar Chart based Area Type per Month", 
         caption = "based on the given dataset",
         x = "Month",
         y = "House Quantity") +
    theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
          plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#4.8 Find the relation between month of the posted house and contact person
ggplot(house_dataset[house_dataset$month == "04" | 
                       house_dataset$month == "05" | 
                       house_dataset$month == "06" | 
                       house_dataset$month == "07",],
       aes(x = Point.of.Contact, fill = month)) +
  geom_histogram(stat = "count", col = "black") + 
  facet_grid(. ~ month) + 
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram Graph based on House Contact Person per Month", 
       caption = "based on the given dataset",
       x = "House Contact Person",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$month == "04" | 
                       house_dataset$month == "05" | 
                       house_dataset$month == "06" | 
                       house_dataset$month == "07",],
       aes(x = Point.of.Contact, y = month, col = month)) + 
  geom_jitter() + 
  scale_fill_brewer(palette = "Pastel1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot based on House Contact Person per Month", 
       caption = "based on the given dataset",
       x = "House Contact Person",
       y = "Posted Month") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#5 What are the factors that can affect the marketing for customers that are looking for a house for their family?
#5.1 Find the relation between the preferred tenant type and house size
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = Size, fill = Tenant.Preferred)) + 
  geom_density() + 
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph based on House Size Person and Tenant Type", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "House Quantity Density") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#5.2 Analyze the connection between tenant type and number of BHK in the house
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = BHK, fill = Tenant.Preferred)) + 
  geom_bar(position = "dodge", col = "black") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart based on BHK and Tenant Type", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = BHK, y = Tenant.Preferred, col = BHK)) + 
  geom_count() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Count Graph based on BHK and Tenant Type", 
       caption = "based on the given dataset",
       x = "BHK Amount",
       y = "Tenant Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#5.3 Determine how the relationship between type of tenant and rent price below 500000
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" & house_dataset$Rent < 500000 | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family" & house_dataset$Rent < 500000 , ], 
       aes(x = Rent, fill = Tenant.Preferred)) +
  geom_histogram(bins = 25, col = "black") +
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Pastel2") + 
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on Rent and Tenant Type", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  

ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" & house_dataset$Rent < 500000 | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family" & house_dataset$Rent < 500000 , ], 
       aes(x = Rent, fill = Tenant.Preferred, y = Tenant.Preferred)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph based on Rent Price and Tenant Type", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "Tenant Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#5.4 Determine how the relationship between type of tenant and rent price greater or equal to 500000 
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" & house_dataset$Rent >= 500000 | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family" & house_dataset$Rent >= 500000 , ], 
       aes(x = Rent, fill = Tenant.Preferred)) +
  geom_histogram(bins = 25, col = "black") + 
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on Rent Price greater or equal to 500000 and Tenant Type", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" & house_dataset$Rent >= 500000 | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family" & house_dataset$Rent >= 500000 , ], 
       aes(x = Rent, fill = Tenant.Preferred, y = Tenant.Preferred)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on Rent Price greater or equal to 500000 and Tenant Type", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "Tenant Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#5.5 Does area type could be influenced by the preferred tenant type 
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = Area.Type, fill = Tenant.Preferred)) +
  geom_histogram(stat = "count", col = "black", bins = 20) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on Area Type and Tenant Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5)) 

ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = Area.Type, y = Tenant.Preferred, col = month)) +
  geom_jitter() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Chart based on Area Type and Tenant Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  
#5.6 Find the connection between city and specific tenant type
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = City, fill = Tenant.Preferred)) + 
  facet_grid(Tenant.Preferred ~ .) +
  geom_bar(position = "dodge", col ="black")  +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart based on City and Tenant Type", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = City, y = Tenant.Preferred, col = month)) + 
  geom_jitter() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Chart based on City and Tenant Type", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  

#5.7 Analyze the relationship between number of bathroom and the tenant type
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = Bathroom)) +
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "light green", high = "dark green") +
  geom_density(aes(y = ..count..), col = "grey", adjust = 4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram and Density Graph based on Bathroom amount and Tenant Type", 
       caption = "based on the given dataset",
       x = "Bathroom",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))
  
#5.8 Determine does the preferred tenant type could affect the house contact person amount
ggplot(house_dataset[house_dataset$Tenant.Preferred == "Family" | 
                       house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = Tenant.Preferred, fill = Point.of.Contact)) +
  geom_histogram(stat = "count", col = "black", position = "dodge") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "navy", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram based on House Contact Person and Tenant Type", 
       caption = "based on the given dataset",
       x = "Tenant Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#6 What are the factors that could affect the preferred local area in Ambattur, Electronic City, kst Chattarpur Apartments, Velachery, and Gachibowli for the customers?
#6.1 Analyze the house quantity that posted on specified area locality
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = Area.Locality, fill = Area.Locality)) + geom_bar(col = "black") + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart Quantity Relation based on Area Locality", 
       caption = "based on the given dataset",
       x = "Area Locality",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#6.2 Determine does the customer type affected by the listed area locality
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = Area.Locality, fill = Tenant.Preferred)) + 
  geom_histogram(stat = "count", position = "dodge", col = "black") + 
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram Relation for Tenant Type based on Area Locality", 
       caption = "based on the given dataset",
       x = "Area Locality",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#6.3 Find the relation between listed area locality and house size greater or equal to 2000
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "Electronic City" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "Velachery" & house_dataset$Size >= 2000|
                       house_dataset$Area.Locality == "Gachibowli" & house_dataset$Size >= 2000 , ], 
       aes(x = Size, y = Area.Locality)) + 
  geom_count() + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Count Plot for House Size greater than or equal to 2000 based on Area Locality", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Area Locality") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "Electronic City" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" & house_dataset$Size >= 2000 | 
                       house_dataset$Area.Locality == "Velachery" & house_dataset$Size >= 2000|
                       house_dataset$Area.Locality == "Gachibowli" & house_dataset$Size >= 2000 , ], 
       aes(x = Size, fill = Area.Locality)) + 
  geom_boxplot() + 
  facet_grid(Area.Locality ~ .) +
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Count Plot for House Size greater than or equal to 2000 based on Area Locality", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Area Locality") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#6.4 Find the relation between listed area locality and house size less than 2000
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "Electronic City" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "Velachery" & house_dataset$Size < 2000|
                       house_dataset$Area.Locality == "Gachibowli" & house_dataset$Size < 2000 , ], 
       aes(x = Size, fill = Area.Locality)) + 
  geom_boxplot(notch = TRUE)  + 
  facet_grid(Area.Locality ~ .) + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot for House Size smaller than 2000 based on Area Locality", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Area Locality") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "Electronic City" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" & house_dataset$Size < 2000 | 
                       house_dataset$Area.Locality == "Velachery" & house_dataset$Size < 2000|
                       house_dataset$Area.Locality == "Gachibowli" & house_dataset$Size < 2000 , ], 
       aes(x = Size, y = Area.Locality)) + 
  geom_count() + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Count Plot for House Size smaller than 2000 based on Area Locality", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Area Locality") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#6.5 How does the listed area locality have impact on the city category
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = City, fill = Area.Locality)) + 
  geom_bar(position = "dodge", col = "black") + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for House Quantity of Area Locality based City", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#6.6 Determine does the amount of BHK will be affected by the given area locality
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = BHK, fill = Area.Locality)) + 
  geom_histogram(stat = "count", col = "black") + 
  facet_grid(Area.Locality ~ .) + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for House Quantity of Area Locality based on Number of BHK", 
       caption = "based on the given dataset",
       x = "Number of BHK",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#6.7 Analyze does house contact person and given area locality have connection
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = Point.of.Contact)) + 
  geom_bar(aes(x = Area.Locality, fill = Point.of.Contact), width = 1,col = "black") + 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_flip() + 
  coord_polar() +
  aes(x = reorder(Point.of.Contact, )) +
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar - Pie Chart for House Quantity based on Area Locality", 
       caption = "based on the given dataset",
       x = "",
       y = "") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#6.8 How the relationship between rent price and listed area locality
ggplot(house_dataset[house_dataset$Area.Locality == "Ambattur" | 
                       house_dataset$Area.Locality == "Electronic City" | 
                       house_dataset$Area.Locality == "kst chattarpur Apartments" | 
                       house_dataset$Area.Locality == "Velachery" |
                       house_dataset$Area.Locality == "Gachibowli" , ], 
       aes(x = Rent, y = Area.Locality, fill = Area.Locality)) + 
  geom_violin() + 
  geom_point() + 
  scale_fill_brewer(palette = "Set1") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "maroon", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Violin Graph and Point Plot for Price House Rent based on Area Locality", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "Area Locality") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#7 Does carpet area and super area have any effect on the customer preferences?
#7.1 Does the given area type affect the month and date it posted on
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Area.Type, fill = month)) + 
  geom_bar(position = "dodge", col = "black") +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for House Quantity per Month based on Area Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Area.Type, y = month, col = date)) + 
  geom_jitter() +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for Posted House per Month based on Area Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "Month Posted") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.2 Determine does house contact person have any relation with listed area type
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Area.Type , fill = Point.of.Contact)) + 
  geom_histogram(stat = "count", position = "dodge", col = "black") + 
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation House Contact Person Amount \nbased on Area Type and Customer Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#7.3 Find the connection between customer type and selected area type
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Area.Type , fill = Tenant.Preferred)) + 
  geom_histogram(stat = "count", position = "dodge", col = "black") + 
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Set2") + 
  theme_dark() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation Tenant Type House Quantity based on Area Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Area.Type , y = Tenant.Preferred, col = Area.Type)) + 
  geom_count() +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Count Plot for Relation Tenant Type House Quantity based on Area Type", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "Tenant Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.4 Find the relationship for listed area type and size of the available house
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Size , y = Area.Type, fill = Area.Type)) + 
  geom_violin(col = "black") +
  geom_boxplot(alpha =  0.3, col = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot & Violin Graph for Relation between Area Type and House Size", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Area Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.5 Does the amount of bathroom will be getting some impact by the given area type
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Bathroom , fill = Area.Type)) + 
  geom_histogram(stat = "count", position = "dodge", col = "black") +
  facet_grid(Area.Type ~ .)+
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Area Type and Number of Bathroom", 
       caption = "based on the given dataset",
       x = "Number of Bathroom",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Bathroom)) + 
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "green", high = "orange") +
  geom_density(aes(y=..count..), colour="grey", adjust=4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph for Relation between Area Type and Number of Bathroom", 
       caption = "based on the given dataset",
       x = "Number of Bathroom",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.6 Find the relation between listed area type and amount of BHK in the house
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = BHK)) + 
  geom_histogram(color = "black", bins = 20 , aes(fill = ..count..)) + 
  scale_fill_gradient("Level", low = "green", high = "orange") +
  geom_density(aes(y=..count..), colour="grey", adjust=4) +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Area Type and Number of BHK(Bedroom, Hall, Kitchen)", 
       caption = "based on the given dataset",
       x = "Number of BHK(Bedroom, Hall, Kitchen)",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = BHK, fill = Area.Type)) + 
  geom_density(col = "black", adjust = 4) +
  facet_grid(Area.Type ~ .)+
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph for Relation between Area Type and Number of BHK(Bedroom, Hall, Kitchen)", 
       caption = "based on the given dataset",
       x = "Number of BHK(Bedroom, Hall, Kitchen)",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.7 Determine does the house amount in the city are affected by the specified area type
CityPortion = house_dataset %>% group_by(City) %>% summarise(Count = length(City))
pie(CityPortion$Count, CityPortion$City, col = rainbow(length(CityPortion$Count)), 
    main = "House Portion by Cities", clockwise = TRUE)
legend("topright", CityPortion$City, cex = 0.8, fill =rainbow(length(CityPortion$Count)))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = City, fill = Area.Type)) + 
  geom_histogram(stat = "count", col = "black") + 
  facet_grid(Area.Type ~ .)+
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Area Type and City House Quantity", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#7.8 How the house rent price relation with the selected area type
ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Rent, y = Area.Type, fill = Area.Type)) + 
  geom_violin(col = "black") + 
  geom_point(size = 0.01) +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Violin Graph for Relation between Area Type and Rent Price", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "Area Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$Area.Type == "Super Area" | 
                       house_dataset$Area.Type == "Carpet Area" , ], 
       aes(x = Rent, y = Area.Type, col = Area.Type)) + 
  geom_jitter() +
  scale_fill_brewer(palette = "Set2") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "dark green", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for Relation between Area Type and Rent Price", 
       caption = "based on the given dataset",
       x = "Rent Price",
       y = "Area Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#8 A fresh graduate student is hired by a company that is located in Mumbai and Bangalore, what are some considerations that could affect the company decision to rent a house for the new employee?
#8.1 Determine the house quantity based on selected city and the preferred tenant
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ], 
       aes(x = City, fill = Tenant.Preferred)) +
  geom_bar(stat = "count", position = "dodge", col = "black" ) + 
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Relation between City and Tenant Type Quantity", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.2 Find the relation between area type and tenant type in both city
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Area.Type, fill = Tenant.Preferred)) +
  geom_histogram(stat = "count", col = "black") +
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Area Type and Tenant Type House Quantity", 
       caption = "based on the given dataset",
       x = "Area Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.3 Analyze the connection between house size and the preferred tenant type based on city
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Size, y = Tenant.Preferred, fill = Tenant.Preferred)) + 
  geom_point() + 
  geom_boxplot(alpha = 0.9) + 
  geom_violin(alpha = 0.3) +
  facet_grid(. ~ City) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot and Violin Graph for Relation between \nHouse Size and Tenant Type Quantity", 
       caption = "based on the given dataset",
       x = "House Size",
       y = "Tenant Type") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.4 Find the relationship for amount of BHK in the available house and selected tenant type
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Tenant.Preferred , fill = Tenant.Preferred)) +
  geom_histogram(stat = "count", col = "black") +
  facet_grid(City ~ BHK) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Number of BHK(Bedroom, Hall, Kitchen) and Tenant Type Quantity", 
       caption = "based on the given dataset",
       x = "Tenant Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.5 Determine the connection of house contact person and the city
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = City, fill = Tenant.Preferred)) +
  geom_bar(position = "dodge", col = "black") +
  facet_grid( Point.of.Contact ~ .) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Relation between City and \nHouse Contact Person Amount", 
       caption = "based on the given dataset",
       x = "City",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.6 Find the relationship between number of bathrooms in each city and sorted by specific tenant type
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Tenant.Preferred , fill = Tenant.Preferred)) +
  geom_histogram(stat = "count", col = "black") +
  facet_grid(City ~ Bathroom) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Histogram for Relation between Tenant Type and Number of Bathroom", 
       caption = "based on the given dataset",
       x = "Tenant Type",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Bathroom , fill = Tenant.Preferred )) + 
  geom_density() + 
  facet_grid(Bathroom ~ Tenant.Preferred) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Density Graph for Relation between Tenant Type and Number of Bathroom", 
       caption = "based on the given dataset",
       x = "Bathroom",
       y = "Density House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

#8.7 Does the house quantity in selected city and sorted tenant type impact on the house rent price
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Rent, y = City , fill = Tenant.Preferred)) + 
  geom_boxplot(col = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot for Relation between City and Rent Price", 
       caption = "based on the given dataset",
       x = "House Price",
       y = "City") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))

ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Rent, y = City , col = Tenant.Preferred)) + 
  geom_jitter() +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Box Plot for Relation between City and Rent Price", 
       caption = "based on the given dataset",
       x = "House Price",
       y = "City") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


#8.8 Find the relation between house furnish status and selected city with preferred tenant
ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Furnishing.Status, fill = City)) +
  geom_bar(col = "black") +
  facet_grid(Tenant.Preferred ~ .) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Bar Chart for Relation between City and  Furnish Status", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "House Quantity") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5)) 

ggplot(house_dataset[house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Mumbai" & house_dataset$Tenant.Preferred == "Bachelors/Family" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors" | 
                       house_dataset$City == "Bangalore" & house_dataset$Tenant.Preferred == "Bachelors/Family", ],
       aes(x = Furnishing.Status, y = Tenant.Preferred, col = City)) +
  geom_jitter() + 
  facet_wrap(~City) +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "right",
        legend.box = "horizontal",
        legend.title = element_text(colour = "black", size = 13, face = "bold"),
        legend.text = element_text(colour = "brown", size = 11 , face = "italic", hjust = 0.5)) +
  labs(title = "Jitter Plot for Relation between City and  Furnish Status", 
       caption = "based on the given dataset",
       x = "Furnish Status",
       y = "Tenant.Preferred") +
  theme(plot.title = element_text(colour = "black", size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dark gray", size = 14, face = "italic", hjust = 0.5))


