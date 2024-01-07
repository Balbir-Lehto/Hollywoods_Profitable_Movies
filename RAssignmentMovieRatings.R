#R assignment Balbir Lehto 
#Import libraries

library(tidyverse)
library(ggplot2)

#Load data:
df<- read.csv("https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")

#Take a look at the data:
view(df)
#check data types:
str(df)
#check for duplicates:
duplicated(df)
# Check for missing values:
colSums(is.na(df))


#remove both 'na' and empty strings in the dataframe:
df1= df[Reduce(`&`, lapply(df, function(x) !(is.na(x)|x==""))),]
#write cleaned dataframe to a .csv file:
write.csv(df1, "clean_df.csv")
#summary statistics:
summary(df1)



#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ 
scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+
theme(axis.text.x = element_text(angle = 90))


# identical scatter plot to above but with added colours
ggplot(df1, aes(x = Lead.Studio, y = Rotten.Tomatoes.., colour = Lead.Studio)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 110)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(values = c("blue", "green", "red", "purple", "orange", "violet", "brown", 
                                 "purple", "black", "brown", "navy","darkgreen","magenta")) +
  labs(title = "Rotten Tomatoes score vs Genre", x = "Lead Studio", y = "Rotten Tomatoes")


#bar chart
ggplot(df1, aes(x=Year)) + geom_bar()


# identical bar chart to above but with added colours
ggplot(df1, aes(x = Year, fill = factor(Year))) + 
geom_bar(color = "black") +
scale_fill_manual(values = c("blue", "green", "yellow", "brown", "navy")) +
labs(title = "Number of movies made per year", x = "Year", y = "Number of movies")


#pie chart of world-wide gross vs genre

pie_chart_df1 <- ggplot(df1, aes(x = "Genre", y = Worldwide.Gross, fill = Genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Genre by worldwide gross", fill = "Genre")
print(pie_chart_df1)


# treemap of aggregate score vs Genre
df1$agg_score = (df1$Audience..score.. + df1$Rotten.Tomatoes..)/2
p <- treemap(
  df1,
  index = c("Genre"),
  vSize = "agg_score",
  #draw = FALSE,
  type = "index",
  palette = "Set3",
  bg.labels = "white",
  align.labels = list(c("center", "center"), c("right", "bottom"))
)

print(p)

