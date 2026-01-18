

library(ggplot2)
library(plotly)

# shortfall plot

# Three potential plots:
# - bar chart
# - step line graph
# - stacked bar chart


# Mock dataframe

"
Y axis: total shortfall amount
X axis: timeline in weeks

"

shortfall_data <- data.frame(
  Weeks = c("Week 1", "Week 2", "Week 3", "Week 4"),
  Shortfall = c(0, 250, 1500, 0)
)

ggplot(shortfall_data, aes(x=Weeks, y=Shortfall)) + 
  geom_bar(stat = "identity")


bar_fig <- plot_ly(
  x = c("Week 1", "Week 2", "Week 3", "Week 4"),
  y = c(0, 250, 1500, 0),
  name = "shortfall",
  type = "bar"
)


step_fig <- plot_ly(
  shortfall_data, x = ~Weeks, y = ~Shortfall, type = 'scatter', mode = 'lines',
  line = list(shape = 'hv', color = 'blue', width = 2)
)
step_fig

# circos plot