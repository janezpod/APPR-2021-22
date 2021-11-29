# Sistem za izposojo koles Capital Bikeshare

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2021/22.

## Tematika

Capital Bikeshare je sistem za izposojo koles, ki se nahaja v Washington, D.C. in okolici. Po velikosti je tretji največji v Združenih državah Amerike. Obravnavani bodo podatki od novembra 2018 do novembra 2021. Analiza se bo osredotočila na vpliv vremena in dela prostih dni na izposojo koles in relacijo voženj. Neizbežno bom analiziral tudi vpliv koronavirusa na uporaba koles, saj se je pandemija začela marca 2020 in v trenutku pisanja še poteka.

### Viri
- [Capital Bike Share](https://www.capitalbikeshare.com/system-data)
- [Meteostat](https://meteostat.net/en/place/365KYE)
- [Government of the District of Columbia](https://edpm.dc.gov/issuances/legal-public-holidays-2021/)
- [New York Times](https://github.com/nytimes/covid-19-data)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
