# Arts and Culture Taskforce Nightlife Data Zone

```{r, include = FALSE}
library(tidyverse)
library(lubridate)

plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.75),
  axis.ticks=element_blank())

wage_tax_revenue <- read_excel("~/GitHub/ACTF_nightlife/data/wage-tax-revenue.xlsx") %>%
  rename(Arts_Entertainment_Recreation = 'Arts, Entertainment, and Other Recreation') %>%
  mutate(year = year(ymd(date)),
         month = month(ymd(date)))
```

```{r
wage_tax_revenue %>%
  select(-year, -month, -Total) %>%
  gather(-date, key = "variable", value = "value") %>%
  group_by(date, variable) %>%
  summarize(sum = sum(value)) %>%
  ggplot()+
  geom_line(aes(x = date, y=sum, color = variable))+
  ylab("Wage Tax Revenue ($)")+
  xlab("Date")+
  labs(
    title = "Philadelphia Wage Tax Receipts for Three Hospitality Industries)",
    subtitle = "Dollars unadjusted for inflation. Source: Philadelphia City Controller's Office")+
  plotTheme
}