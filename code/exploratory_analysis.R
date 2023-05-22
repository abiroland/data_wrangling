# data visualization
# prepared by: Roland

# prep
library(tidyverse)
library(bbplot)
library(viridis)
library(ggthemes)
library(showtext) # font
library(ggsci) # font 
library(ggridges)
library(patchwork)


showtext.auto()

df$prices <- as.numeric(df$prices)

df$new_segment <- fct_recode(df$new_segment,
  "consumer" = "consumer", 
  "corporate" = "corporate", 
  "home office" = "home_office"
)

#plot 1 percentage distribution of ship mode by client segment

df %>%
  with(table(ship_mode, new_segment)) %>%
  prop.table(1) %>%
  data.frame() %>%
  mutate(
    freq = round(Freq*100,1),
  ) %>%
  ggplot(aes(
    x = freq, y = ship_mode, 
    fill = new_segment
  )) +
  geom_bar(stat = "identity") +
   scale_fill_tableau() +
  theme_clean() + 
  labs(
    title = "ship mode vs clients segments",
    x = "",
    y = "ship mode"
  ) + 
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_text(
      family = "Light 300",
      size = 15
    ),
    axis.text.y = element_text(
      family = "Light 300",
      size = 10 
    )
  ) +
  geom_text(aes(label = freq), size = 5, color = "white",
            family = "Light 300", position = position_stack(vjust = .4))

#plot 2 year vs country subset by client segment
df %>% 
  group_by(country, year) %>%
  mutate(
    avgprice = mean(as.numeric(prices))
  ) %>%
  ggplot(aes(
    x = avgprice, y = year, fill = country)) +
  geom_density_ridges(
    alpha = .9, color = "white", size = .8
  ) +
  scale_x_continuous(labels = scales::dollar_format()) + 
  scale_fill_tableau()+
  theme_clean() +
  labs(
    title = "Averege shipment fee to each country by year",
    x = "Average price",
    y = "Year"
  ) +
  theme(
    plot.title = element_text(
      family = "Light 300",
      face = "bold",
      size = 15
    )
  )

df$year <- as.numeric(df$year)

#plot 3: ship mode with below and above average prices

p1 <- df %>%
  filter(country == "CA") %>%
  group_by(ship_mode, country) %>%
  mutate(
    stdprice = round((prices - mean(prices))/sd(prices),1),
    group = ifelse(stdprice < 0, "below average", "above average")
  ) %>%
  ggplot(aes(
    x = fct_reorder(ship_mode,stdprice), y = stdprice, 
    label = group
  )) +
  geom_bar(stat = "identity", aes(fill = group)) +
  coord_flip() +
  facet_wrap(~country~year, scales = "free_x") +
  theme_bw() +
  scale_fill_tableau()+
  labs(
    title = "Normalized shipment price",
    x = "",
    y = "",
    ) +
  theme(
    plot.title = element_text(
      family = "Light 300",
      face = "bold",
      size = 15
    )
  )+
  theme(
    legend.position = "none"
  ) +
  guides(fill = guide_legend(reverse = T))


p2 <- df %>%
  filter(country == "US") %>%
  group_by(ship_mode, country) %>%
  mutate(
    stdprice = round((prices - mean(prices))/sd(prices),1),
    group = ifelse(stdprice < 0, "below average", "above average")
  ) %>%
  ggplot(aes(
    x = fct_reorder(ship_mode,stdprice), y = stdprice, 
    label = group
  )) +
  geom_bar(stat = "identity", aes(fill = group)) +
  coord_flip() +
  facet_wrap(~country~year, scales = "free_x") +
  theme_bw() +
  scale_fill_tableau()+
  labs(
    title = "",
    x = "",
    y = "",
  ) +
  theme(
    plot.title = element_text(
      family = "Light 300",
      face = "bold",
      size = 15
    )
  )+
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(reverse = T))

p1/p2

df %>% 
  group_by(new_segment, year) %>%
  mutate(
    avgprice = round(mean(prices),2)
  ) %>%
  ggplot(aes(
    x = year, y = new_segment, 
    fill = avgprice
  )) +
  geom_tile(aes(fill = avgprice), 
            color = "white", lwd = 1)+
  scale_fill_gradient2_tableau() +
  geom_text(aes(label = avgprice), color = "white") +
  theme_bw() +
  labs(
    title = "Client distribution by avergae price for each year",
    x = "Year",
    y = "Segment",
  ) +
  theme(
    plot.title = element_text(
      family = "Light 300",
      face = "bold",
      size = 15
    )
  )
    
