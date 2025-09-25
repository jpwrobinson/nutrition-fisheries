library(tidyverse)
library(countrycode)
theme_set(theme_bw())

pmii<-readRDS('data/inadequate_intakes/mean_national_inadequate_intakes_of_fish_nutrients.Rds')
gdd<-read.csv('data/gdd_seafood_intake_country.csv')

gdd<-gdd %>% left_join(pmii) %>% 
  mutate(country = countrycode(iso3, 'iso3c', 'country.name.en'))

meds<-gdd %>% summarise(median = median(median), pdeficient = median(pdeficient, na.rm=TRUE))

g1<-ggplot(gdd, aes(median, pdeficient)) + 
  geom_vline(xintercept = meds[1] %>% pull, col='red', alpha=0.5) +
  geom_hline(yintercept = meds[2] %>% pull, col='red', alpha=0.5) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country), size=2) +
  labs(y = 'prevalence inadequate intake', x = 'Seafood consumption, g/day')

pdf(file = 'pmii_vs_consumption.pdf', height=4, width=7)
print(g1)
dev.off()
