library(tidyverse)
library(data.table)
library(ggrepel)
library(readxl)
library(scales)

setwd("E:\\_dev\\MSDS670\\R\\")

########################################################################
# DWI Rates in Autin

# Open dwi with xlsx
dwi <- read_xlsx('Week_7_In_Class_Exersise_Data.xlsx', sheet = 'DWI Rates')

# Label the first row as column names
colnames(dwi) <- as.character(unlist(dwi[1,]))

#delete first row
dwi = dwi[-1, ]

# Convert to datatable
dwi <- data.table(dwi)

# Check the structure
str(dwi)

# Convert month to factor and set levels
dwi$Month <- factor(dwi$Month, levels = c("January", "February", "March", "April", "May", "June", "July"))

# convert DWI Rates to numveric
dwi$`DWI Rates in Austin` <- as.numeric(dwi$`DWI Rates in Austin`)

# Create a second data seris with the values after Uber/Lyft left
dwi2 <-dwi[5:7]


ggplot(data = dwi, aes(Month, `DWI Rates in Austin`, group = 1, label=`DWI Rates in Austin`)) + 
  geom_point(color = "grey") + geom_line(color = "grey") +
  
   geom_point(data = dwi2, aes(Month, `DWI Rates in Austin`), group = 1, color = "blue") +
   geom_line(data = dwi2, aes(Month, `DWI Rates in Austin`), group = 1, color = "blue") +
   
   geom_text_repel(nudge_y = -15, nudge_x = -0.1, segment.color = 'transparent', colour = "grey") +
  
   geom_text_repel(data = dwi2, aes(dwi2$Month, dwi2$`DWI Rates in Austin`, group = 1), 
                   nudge_y = -15, nudge_x = -0.1, segment.color = 'transparent', colour = "blue") +
   
   scale_y_continuous(limits = c(300, 550)) + xlab("") +
   
   annotate("segment", x = 'May', xend = 'May', y = 500, yend = 425, colour = "red", size=1, arrow=arrow()) + 
   annotate("text", x = 'May', y = 510, label = "Uber and Lyft Left in May" , color="red", size=3)  +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.title.y = element_text(hjust=0.95, vjust = 3)) +
   
  labs(title="DWI Rates in Austin", subtitle = "DWI rates in Austing increased after Uber and Lyft left in May")


########################################################################
# APAC Travel Market

# Open APAC Travel Market with xlsx
apac <- read_xlsx('Week_7_In_Class_Exersise_Data.xlsx', sheet = 'APAC Travel Market')

# Transpose data
apac <- t(apac)

# Label the first row as column names
colnames(apac) <- as.character(unlist(apac[1,]))

#delete first row
apac = apac[-1, ]

# Convert to datatable
apac <- data.table(apac)

# Recreate the year column
apac$Year = c(2000, 2016)

# Convert columns to number and convert back to datatable
apac <- sapply(apac, as.numeric )
apac <- data.table(apac)

# Check the structure
str(apac)


# Create a plot
ggplot(data = apac, aes(Year, Other, group = 1)) + 
  geom_point(color = "grey") + geom_line(color = "grey") +
   
  geom_point(data = apac, aes(Year, `Latin America`), group = 1, color = "grey") +
  geom_line(data = apac, aes(Year, `Latin America`), group = 1, color = "grey") +
  
  geom_point(data = apac, aes(Year, apac$`Middle East`), group = 1, color = "grey") +
  geom_line(data = apac, aes(Year, apac$`Middle East`), group = 1, color = "grey") +
  
  geom_point(data = apac, aes(Year, apac$`North America`), group = 1, color = "grey") +
  geom_line(data = apac, aes(Year, apac$`North America`), group = 1, color = "grey") +
  
  geom_point(data = apac, aes(Year, apac$Europe), group = 1, color = "red") +
  geom_line(data = apac, aes(Year, apac$Europe), group = 1, color = "red") +
  
  geom_point(data = apac, aes(Year, apac$`Asia Pacific`), group = 1, color = "blue") +
  geom_line(data = apac, aes(Year, apac$`Asia Pacific`), group = 1, color = "blue") +
  
  labs(title="Percent Share World Travel Market",
  subtitle = "APAC travel market share has increased since 2000 \nat the expense of Europe's travel market") + 
  xlab("") + ylab("Percent Share of World Travel Market") + 
  
  scale_y_continuous(limits = c(0, 0.45), labels = scales::percent) + 
  scale_x_discrete(limits = c(2000, 2016), expand = c(.1, .1)) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title.y = element_text(hjust=0.95, vjust = 3)) +
  
  annotate("text", x = 1999.8, y = .35, label = "35%" , color="Red", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .27, label = "27%" , color="Red", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = .36, label = "Europe" , color="Red", size=3, hjust = 'left') + 

  annotate("text", x = 1999.8, y = .10, label = "10%" , color="Grey", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .09, label = "9%" , color="Grey", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = .11, label = "Other" , color="Grey", size=3, hjust = 'left') +
  
  annotate("text", x = 1999.8, y = .02, label = "0.03%" , color="Grey", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .02, label = "0.03%" , color="Grey", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = 0.02, label = "Middle East" , color="Grey", size=3, hjust = 'left') +

  annotate("text", x = 1999.8, y = .047, label = "0.04%" , color="Grey", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .047, label = "0.04%" , color="Grey", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = 0.057, label = "Latin America" , color="Grey", size=3, hjust = 'left') +
  
  annotate("text", x = 1999.8, y = .27, label = "27%" , color="Grey", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .25, label = "25%" , color="Grey", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = 0.285, label = "North America" , color="Grey", size=3, hjust = 'left') +
  
  annotate("text", x = 1999.8, y = .21, label = "21%" , color="Blue", size=3, hjust = 'right') + 
  annotate("text", x = 2016.2, y = .31, label = "31%" , color="Blue", size=3, hjust = 'left') +
  annotate("text", x = 2001, y = 0.2, label = "Latin America" , color="Blue", size=3, hjust = 'left') 

########################################################################
# Philly Housing Prices

# Open Philly with xlsx
philly <- read_xlsx('Week_7_In_Class_Exersise_Data.xlsx', sheet = 'Philly')

# Label the first row as column names
colnames(philly) <- as.character(unlist(philly[1,]))

#delete first row
philly = philly[-1, ]

# Convert to datatable
philly <- data.table(philly)

# Convert price to numeric
philly$`Number of Square Feet per $1,000,000` <- as.numeric(philly$`Number of Square Feet per $1,000,000`)

# Divide the price per sqf by 10
philly$`Number of Square Feet per $1,000,000` <- philly$`Number of Square Feet per $1,000,000`/10

# Change the Price column name
setnames(philly, "Number of Square Feet per $1,000,000", "Number_of_Square_Feet_per_100000")

# Check the structure
str(philly)

# Subset the data
philly2 <- philly[27:39]


# Your turn to plot
#ggplot(philly2, aes(x = Number_of_Square_Feet_per_100000)) +
#  geom_histogram(stat = "bin", binwidth=100)

library(forcats)

mutate(City = fct_reorder(City, desc(Number_of_Square_Feet_per_100000)))
PhillyP<-ggplot(philly2, aes(x=reorder(City, Number_of_Square_Feet_per_100000), y=Number_of_Square_Feet_per_100000)) + 
  geom_bar(stat="identity", width=0.5, color="gray",fill="light blue")+
  geom_text(aes(label=Number_of_Square_Feet_per_100000), vjust=-0.5, size=3.5)+
  theme_minimal()+
  scale_fill_manual("legend", values = c("Raleigh" = "black", "Las Vegas" = "orange", "Philadelphia" = "blue"))+
  scale_color_gradient2(name="",
                        breaks = c(1000, 1500, 2000),
                        labels = c("'1000", "'1500", "'2000"),
                        low = muted("blue"),
                        high = muted("red"),
                        mid = "gray60",
                        midpoint = 1500)

PhillyP + labs(title="Philadelphia Offers One of the Lowest Square Footage for cost", 
              x="City", y = "Square Feet per $100,000")
  
#PhillyP + coord_flip()
