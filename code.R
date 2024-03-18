library(tidyverse)
library(causact)

# LOAD DATA
carsDF = readr::read_csv("carsFixed.csv")
# VIEW QUICK SUMMARY OF DATA
carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            numberOfBossVisits = sum(boss))

# CREATE GRAPHICAL MODEL
graph = dag_create() %>%
  dag_node("Cars Fixed","K",
           data = carsDF$carsFixed,
           rhs = poisson(rate)) %>%
  dag_node("Exp Cars Fixed - Shop Level","rate",
           rhs = exp(alpha_shop + beta_shop * x),
           child = "K") %>%
  dag_node("Intercept - Shop Level","alpha_shop",
           rhs = normal(alpha,sigma_alpha),
           child = "rate") %>%
  dag_node("Boss Effect - Shop Level","beta_shop",
           rhs = normal(beta,sigma_beta),
           child = "rate") %>%
  dag_node("Intercept - Midas Level","alpha",
           rhs = normal(3,2),
           child = "alpha_shop") %>%
  dag_node("Std Dev - Midas Level","sigma_alpha",
           rhs = uniform(0,2),
           child = "alpha_shop") %>%
  dag_node("Exp Boss Effect - Midas Level","beta",
           rhs = normal(0,1),
           child = "beta_shop") %>%
  dag_node("Std Dev Boss Effect","sigma_beta",
           rhs = uniform(0,2),
           child = "beta_shop") %>%
  dag_node("Boss Present","x",
           data = carsDF$boss,
           child = "rate") %>%
  dag_plate("Observation","i",
            nodeLabels = c("K","rate","x")) %>%
  dag_plate("Shop","j",
            nodeLabels = c("beta_shop","alpha_shop"),
            data = carsDF$shopID,
            addDataNode = TRUE)

# DISPLAY GRAPHICAL MODEL
graph %>% dag_render()

# GET POSTERIOR DISTRIBUTION
drawsDF = graph %>% dag_numpyro(seed = 1234567)

carsDF

library(ggplot2)

drawsDF %>% dagp_plot()

##################################

carsDF$Boss_Present_Text <- ifelse(carsDF$boss == 1, "Patrick Available", "Patrick Unavailable")

valuesNeedingColors = unique(carsDF$Boss_Present_Text)
colorList = c("orange",
              "gray")
names(colorList) = valuesNeedingColors

graph1 = carsDF %>% ggplot(aes(x = observation, y = carsFixed , fill=Boss_Present_Text, group=shopID)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~shopID) +
  labs(title = "Cars Fixed Over the Last 10 Weeks",
       subtitle = "Effect of Patrick's Presence on Car Fixing", x = "Observation", y = "Number of Cars Fixed") +
  theme_minimal()

graph1


draw = drawsDF %>%
  mutate(rate_bosspresent1 = exp(alpha_shop_1.0 + beta_shop_1.0 * 1)) %>% mutate(rate_ShopLevel_bossabsent1 = exp(alpha_shop_1.0)) %>%
  mutate(rate_bosspresent2 = exp(alpha_shop_2.0 + beta_shop_2.0 * 1)) %>% mutate(rate_ShopLevel_bossabsent2 = exp(alpha_shop_2.0)) %>%
  mutate(rate_bosspresent3 = exp(alpha_shop_3.0 + beta_shop_3.0 * 1)) %>% mutate(rate_ShopLevel_bossabsent3 = exp(alpha_shop_3.0)) %>%
  mutate(rate_bosspresent4 = exp(alpha_shop_4.0 + beta_shop_4.0 * 1)) %>% mutate(rate_ShopLevel_bossabsent4 = exp(alpha_shop_4.0)) %>%
  mutate(rate_bosspresent5 = exp(alpha_shop_5.0 + beta_shop_5.0 * 1)) %>% mutate(rate_ShopLevel_bossabsent5 = exp(alpha_shop_5.0))


plotCaption = 
  "Median Profit (dark point) and 90% Credible Interval"

postDF = drawsDF %>%
  mutate(rate_bosspresent1 = exp(alpha_shop_1.0 + beta_shop_1.0 * 1)) %>%
  mutate(rate_bossabsent1 = exp(alpha_shop_1.0)) %>%
  mutate(shop_1 = rate_bosspresent1 - rate_bossabsent1)  %>%
  mutate(rate_bosspresent2 = exp(alpha_shop_2.0 + beta_shop_2.0 * 1)) %>%
  mutate(rate_bossabsent2 = exp(alpha_shop_2.0)) %>%
  mutate(shop_2 = rate_bosspresent2 - rate_bossabsent2) %>%
  mutate(rate_bosspresent3 = exp(alpha_shop_3.0 + beta_shop_3.0 * 1)) %>%
  mutate(rate_bossabsent3 = exp(alpha_shop_3.0)) %>%
  mutate(shop_3 = rate_bosspresent3 - rate_bossabsent3) %>%
  mutate(rate_bosspresent4 = exp(alpha_shop_4.0 + beta_shop_4.0 * 1)) %>%
  mutate(rate_bossabsent4 = exp(alpha_shop_4.0)) %>%
  mutate(shop_4 = rate_bosspresent4 - rate_bossabsent4) %>%
  mutate(rate_bosspresent5 = exp(alpha_shop_5.0 + beta_shop_5.0 * 1)) %>%
  mutate(rate_bossabsent5 = exp(alpha_shop_5.0)) %>%
  mutate(shop_5 = rate_bosspresent5 - rate_bossabsent5) %>%
  mutate(q05_1 = stats::quantile(shop_1,0.05)) %>%
  mutate(q50_1 = stats::quantile(shop_1,0.50))%>%
  mutate(q95_1 = stats::quantile(shop_1,0.95))

carsDF_merge = carsDF %>% group_by(shopID) %>%
  summarize(q05_1 = stats::quantile(postDF$shop_1,0.05),
            q50_1 = stats::quantile(postDF$shop_1,0.50),
            q95_1 = stats::quantile(postDF$shop_1,0.95),
            q05_2 = stats::quantile(postDF$shop_2,0.05),
            q50_2 = stats::quantile(postDF$shop_2,0.50),
            q95_2 = stats::quantile(postDF$shop_2,0.95),
            q05_3 = stats::quantile(postDF$shop_3,0.05),
            q50_3 = stats::quantile(postDF$shop_3,0.50),
            q95_3 = stats::quantile(postDF$shop_3,0.95),
            q05_4 = stats::quantile(postDF$shop_4,0.05),
            q50_4 = stats::quantile(postDF$shop_4,0.50),
            q95_4 = stats::quantile(postDF$shop_4,0.95),
            q05_5 = stats::quantile(postDF$shop_5,0.05),
            q50_5 = stats::quantile(postDF$shop_5,0.50),
            q95_5 = stats::quantile(postDF$shop_5,0.95),
  ) 




data1 = carsDF_merge %>% select(shopID,q05_1,q50_1,q95_1) %>% filter(shopID == 1) %>% mutate(maxexpense = (ceiling(q95_1)*100)) %>% mutate(minexpense = (floor(q05_1)*100))
data2 = carsDF_merge %>% select(shopID,q05_2,q50_2,q95_2) %>% filter(shopID == 2) %>% mutate(maxexpense = (ceiling(q95_2)*100)) %>% mutate(minexpense = (floor(q05_2)*100))
data3 = carsDF_merge %>% select(shopID,q05_3,q50_3,q95_3) %>% filter(shopID == 3) %>% mutate(maxexpense = (ceiling(q95_3)*100)) %>% mutate(minexpense = (floor(q05_3)*100))
data4 = carsDF_merge %>% select(shopID,q05_4,q50_4,q95_4) %>% filter(shopID == 4) %>% mutate(maxexpense = (ceiling(q95_4)*100)) %>% mutate(minexpense = (floor(q05_4)*100))
data5 = carsDF_merge %>% select(shopID,q05_5,q50_5,q95_5) %>% filter(shopID == 5) %>% mutate(maxexpense = (ceiling(q95_5)*100)) %>% mutate(minexpense = (floor(q05_5)*100))

library(dplyr)

# Assuming data1, data2, ..., data5 are your datasets
# Replace "new_column_name" with the desired common column name

data1 <- data1 %>%
  rename(
    q05 = q05_1,
    q50 = q50_1,
    q95 = q95_1
  )

data2 <- data2 %>%
  rename(
    q05 = q05_2,
    q50 = q50_2,
    q95 = q95_2
  )

data3 <- data3 %>%
  rename(
    q05 = q05_3,
    q50 = q50_3,
    q95 = q95_3
  )

data4 <- data4 %>%
  rename(
    q05 = q05_4,
    q50 = q50_4,
    q95 = q95_4
  )

data5 <- data5 %>%
  rename(
    q05 = q05_5,
    q50 = q50_5,
    q95 = q95_5
  )

# Now merge the datasets
merged_data <- bind_rows(data1, data2, data3, data4, data5)

annotations <- data.frame(
  shopID = c(3),
  annotation_text = c("Shop managed by Micheal - Patrick's Brother"))

plotCaption = 
  "Average Additional Cars Fixed (dark point) and 90% Credible Interval \n Assuming revenue per car fixed is $100"


merged_data%>% 
  ggplot(aes(y=shopID)) + 
  geom_linerange(aes(xmin = q05, xmax= q95), size = 4, color = "lightsteelblue3") + 
  geom_point(aes(x = q50), size = 5, color = "darkorange3") + 
  geom_text(data = annotations, aes(x = 1.2, y=3.1, label = annotation_text),
            vjust = -0.5, hjust = 0, size = 5, color = "red")+
  labs(title = "Expected Additional Cars Fixed When Boss Visits", x = "Expected Additional Cars Fixed", y = "Shops") +
  geom_text(aes(x = 17, 
                label = 
                  paste0("Expected Addtional Revenue = " ,
                         scales::dollar(minexpense), " to ", scales::dollar(maxexpense))),
            vjust = 0.5, hjust = 0,
            size = 3, color = "darkred") +
  scale_x_continuous(limits = c(-10,30)) +
  theme_minimal(15)
