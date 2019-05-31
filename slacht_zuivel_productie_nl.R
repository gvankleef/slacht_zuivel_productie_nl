library(dplyr)
library(cbsodataR)
library(ggplot2)
library(lubridate)
library(reshape2)
library(scales)

##hier kan je op datasets zoeken: https://opendata.cbs.nl/statline/portal.html?

##create dataset for slachtingen since 1990
slacht <- cbs_get_data('7123slac') %>%
  cbs_add_date_column() %>%
  cbs_add_label_columns()

##create dataset for zuivel productie since 1995
zuivel <- cbs_get_data('7425zuiv') %>%
  cbs_add_date_column() %>%
  cbs_add_label_columns()

##remove na's & ony select the numbers per month
slacht_complete_month <- slacht[!is.na(slacht$AantalSlachtingen_1) & slacht$Perioden_freq == "M",]
zuivel_complete_month <- zuivel[!is.na(zuivel$Hoeveelheid_1) & zuivel$Perioden_freq == "M",]

##totaal slachtingen since 1990 x1000
slacht_agg <- slacht_complete_month %>%
  group_by(Slachtdieren_label) %>%
  summarise(aantal_slachtingen = sum(AantalSlachtingen_1))
slacht_agg[order(slacht_agg$aantal_slachtingen, decreasing = TRUE),]

##boxplot with total slachtingen per month (since 1990) broken out by the animal names
plot2 <- ggplot(slacht_complete_month, aes(x = month, y= AantalSlachtingen_1, fill = month)) +
  geom_boxplot() + facet_wrap(.~Slachtdieren_label, scales = "free")

##trend over the year per animal sort per month
slacht_complete_month_2005 <- slacht_complete_month[slacht_complete_month$Perioden_Date >= as.Date("2005-01-01"),] 

##slacht plot starting from 1990
ggplot(slacht_complete_month, aes(x = Perioden_Date, y = AantalSlachtingen_1)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  theme_bw() + 
  scale_x_date(breaks = "5 year", minor_breaks = "1 year", date_labels =  "%Y") + 
  scale_y_continuous(labels = comma) +
  facet_wrap(.~Slachtdieren_label, scales = "free") +
  labs(title = "slachtingen per maand vanaf 1990", x = "Jaar" , y = "slachtingen x1000")

##slacht plot starting from 2005
ggplot(slacht_complete_month_2005, aes(x = Perioden_Date, y = AantalSlachtingen_1)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  theme_bw() + 
  scale_x_date(breaks = "5 year", minor_breaks = "1 year", date_labels =  "%Y") + 
  scale_y_continuous(labels = comma) +
  facet_wrap(.~Slachtdieren_label, scales = "free") +
  labs(title = "slachtingen per maand vanaf 2005", x = "Jaar" , y = "slachtingen x1000")

##zuivel productie starting from 1995
ggplot(zuivel_complete_month, aes(x = Perioden_Date, y = Hoeveelheid_1)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  theme_bw() + 
  scale_x_date(breaks = "5 year", minor_breaks = "1 year", date_labels =  "%Y") + 
  scale_y_continuous(labels = comma) +
  labs(title = "zuivelproductie per maand vanaf 1995", x = "Jaar" , y = "zuivel in kilo's x1000")


##correlation between animals
cordf_1 <- select(slacht_complete_month, Perioden_Date,Slachtdieren_label, AantalSlachtingen_1)
cordf_2 <- dcast(cordf_1, Perioden_Date ~ Slachtdieren_label, value.var = "AantalSlachtingen_1")
cor(select(cordf_2,-Perioden_Date))

##correlation between zuivel productie and jonge kalveren slacht
slacht_jonge_kalveren_2009 <- slacht_complete_month[slacht_complete_month$Perioden_Date >= as.Date("2009-01-01") & slacht_complete_month$Slachtdieren_label == "Kalveren jonger dan 9 maanden",] 
zuivel_2009 <- zuivel_complete_month[zuivel_complete_month$Perioden_Date >= "2009-01-01",]
cor(slacht_jonge_kalveren_2009$AantalSlachtingen_1, zuivel_2009$Hoeveelheid_1)
