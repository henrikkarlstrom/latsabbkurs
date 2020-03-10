## OPPSETT ##
#------------------------------------#

# Last inn pakkene vi trenger
library(tidyverse)

# Last inn data vi trenger
norway_pubs <- data("norway_pubs")

## RYDDING AV DATA ##
#------------------------------------#

# Samler års-info i egen kolonne
norway_pubs_lang <- norway_pubs %>%
  pivot_longer(
    cols = c(`2011`:`2019`),
    names_to = "År",
    values_to = "Antall"
    )

# Sprer typene på to variabler
norway_pubs_ryddig <- norway_pubs_lang %>%
  pivot_wider(
    names_from = "Type",
    values_from = "Antall"
  )

## ANALYSE ##
#------------------------------------#

# Beregne poeng per publikasjon
beregning <- norway_pubs_ryddig %>%
  mutate(
    poeng_per_publikasjon = Publikasjonspoeng / Publikasjoner
    )

# Undersøke om beregningsmodell har noe å si
modell <- norway_pubs_ryddig %>%
  mutate(
    beregningsmodell = if_else(År < 2015, "gammel", "ny")
    )

# Beregne poeng per publikasjon under to modeller
oppsummering <- modell %>%
  group_by(beregningsmodell) %>%
  summarise(
    publikasjonspoeng = sum(Publikasjonspoeng),
    publikasjoner = sum(Publikasjoner),
    poeng_per_publikasjon = publikasjonspoeng / publikasjoner
  )
