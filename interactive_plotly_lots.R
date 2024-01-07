#An initial look at producing interactive plots using R 
library(plotly)

#upload dataset
df<- read.csv("https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")

#remove 'na' and empty strings
df1= df[Reduce(`&`, lapply(df, function(x) !(is.na(x)|x==""))),]
df1$agg_score = (df1$Audience..score.. + df1$Rotten.Tomatoes..)/2
write.csv(df1, "clean_df.csv")

# mean Rotten.Tomatoes.. score for each Studio
mean_scores <- df1 %>%
  group_by(Lead.Studio) %>%
  summarize(mean_score = round(mean(Rotten.Tomatoes..)))

#plot
fig <- plot_ly(mean_scores, x = ~Lead.Studio, y = ~mean_score, type = 'bar',
               hovertemplate = paste('<i>Score</i>: %{y}<br>',
                                     '<br><b>Studio</b>: %{x}<br>'),
               showlegend = TRUE)

# Print  plot
print(fig)



#bar chart of aggregate (Rt and Audience) scores for each film
fig2 <-  plot_ly(x = c(df1$agg_score), y = c(df1$Film), type = 'bar') %>%
  layout(title = 'Film vs Aggregate audience score',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#aaaa', 
           zerolinewidth = 4, 
           gridcolor = '#bbbb'), 
         yaxis = list( 
           zerolinecolor = '#aaaa', 
           zerolinewidth = 4, 
           gridcolor = 'ffff'))

print(fig2)