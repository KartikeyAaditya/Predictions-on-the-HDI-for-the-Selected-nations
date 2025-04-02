library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(janitor)
library(skimr)
library(RColorBrewer)
library(purrr)
library(broom)

path <- read_csv("E:/Bihar data/GDL-Subnational-HDI-data.csv")

str(path)
colnames(path)
colSums(is.na(path))
head(path, 10)
skim(path)


threshold = nrow(path)/2

path_clean <- path%>%
  select(where(function(k) sum(is.na(k)) <= threshold))
 
path_clean <- path_clean%>%
  mutate(across(where(is.numeric), function(k) ifelse(is.na(k), mean(k, na.rm = TRUE), k)))%>%
  mutate(across(where(is.numeric), function(k) ifelse(is.nan(k), 0, k)))

path_clean

colSums(is.na(path_clean))


selected_countries <- c("Russia","India","Bangladesh","Pakistan","Nepal","Sri Lanka","Myanmar","United States","China")
hdi_data <- path_clean%>%
  filter(Country %in% selected_countries, Level == "National")

head(hdi_data,15)

hdi_clean <- hdi_data%>%
  select(-c("Continent","ISO_Code","Level","GDLCODE","Region"))

hdi_long <- hdi_clean%>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "HDI")%>%
  mutate(Year = as.numeric(Year), HDI = as.numeric(HDI))


hdi_long <- hdi_long%>%
  arrange(Country,Year)%>%
  group_by(Country)%>%
  mutate(hdi_growth = (HDI - lag(HDI))/lag(HDI)*100 )%>%
  replace_na(list(hdi_growth = 0))

hdi_long


lm_model <- lm(hdi_growth~Year, data = hdi_long)

summary(lm_model)

model = lm(HDI~Year,data = hdi_long)

summary(model)


hdi_train <- hdi_long%>%
  filter(Year <= 2022)


models <- hdi_long%>%
  group_by(Country)%>%
  nest()%>%
  mutate(model = map(data, ~lm(HDI ~poly(Year, 2), data = .x)),
         pred_data = map(model, function(k){
           pred <- predict(k, newdata = data.frame(Year = c(2023,2024,2025,2026)), interval = "confidence")
           data.frame(Year = c(2023,2024,2025,2026), HDI = pred[, "fit"], hdi_upper = pred[, "upr"], hdi_lower = pred[, "lwr"])
         })
  )

prediction_year <- models%>%
  unnest(pred_data)%>%
  select(Country,Year,HDI,hdi_upper,hdi_lower)

hdi_long <- hdi_train%>%
  bind_rows(prediction_year)%>%
  arrange(Country,Year)%>%
  group_by(Country)%>%
  mutate(hdi_growth = (HDI - lag(HDI))/lag(HDI)*100)%>%
  replace_na(list(hdi_growth = 0))



p <- plot_ly(hdi_long,
             x = ~Year,
             y = ~HDI,
             group = ~Country,
             color = ~Country,
             colors = "viridis",
             hoverinfo = "text",
             type = "scatter",
             mode = "lines+markers",
             text = ~paste("Country:",Country, "<br>Year:",Year, "<br>HDI:", round(HDI,3), "<br>HDI_Growth:", round(hdi_growth,3),"%"),
             line = list(width = 4, color = "viridis", dash = "solid"),
             marker= list(size = 8, opacity = 0.7, symbol = "circle"))%>%
  layout(
    title = list(text = "Human Development Index Across The Selected Nations",font = list(color = "black", size = 18),x = 0.5,y = 0.98,xanchor = "center"),
    xaxis = list(title = "Year",gridcolor = "lightblue",showgrid = TRUE,zeroline = FALSE),
    yaxis = list(title = "Human Development Index",gridcolor = "pink",showgrid = TRUE,zeroline = FALSE),
    font = list(family = "Arial",size = 14),
    legend = list(title = list(text = "Country"), x = 1, y = 1, xanchor = "left", yanchor = "top",bordercolor = "white",borderwidth = 2),
    paper_bgcolor = "FFFFFF",
    plot_bgcolor = "FFFFFF",
    template = "plotly_dark"
  )
  
                           
  
p
