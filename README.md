# Sistem za izposojo koles Capital Bikeshare

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2021/22.

## Tematika

Capital Bikeshare je sistem za izposojo koles, ki se nahaja v Washington, D.C. in okolici. Po velikosti je tretji največji v Združenih državah Amerike. Obravnavani bodo podatki od leta 2010 do vključno. Analiza se bo osredotočila na vpliv vremena in pandemije Covid-19 na izposojo koles.

### Viri
- [Capital Bikeshare](https://www.capitalbikeshare.com/system-data)
- [Meteostat](https://meteostat.net/en/place/365KYE)
- [Government of the District of Columbia](https://edpm.dc.gov/issuances/legal-public-holi=days-2021/)
- [New York Times](https://github.com/nytimes/covid-19-data)

## Program

POZOR: Prenos traja kar nekaj casa, saj so podatki o kolesarskih vožnjah
skupaj veliki cca. 4 Gb
Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

POZOR: Program ob prvem zagonu sam prenese podatke z [Amazon Web Service](https://s3.amazonaws.com/capitalbikeshare-data/index.html). Prenos traja kar nekaj časa, saj so podatki o kolesarskih vožnjah skupaj veliki cca. 4 Gb.

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
