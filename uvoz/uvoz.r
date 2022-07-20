#2. faza: Uvoz podatkov

source('lib/funkcije.r', encoding='UTF-8')
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#  Podatki o zgodovini vozenj s kolesi se zacenjo leta 2010. Do vkljucno leta
# 2017 so bili podatki na AWS nalozeni letno oz. cetrtletno, poznejsi pa so
# nalozeni mesecno.
#  POZOR: Prenos traja kar nekaj casa, saj so podatki o kolesarskih vožnjah
# skupaj veliki cca. 4 Gb

PrenosCB(2011, 2021)

ImenaStolpcevCB <- ImenaStolpcev('tripdata')

#  Ko na hitro pregledamo imena stolpcev tabel, se nam zdi, da so stolpci v
# tabelah enako poimenovane od tabele 1 (2011) do vkljucno tabele 52 (202003).
# Stolpci vsebujejo cas voznje, datum zacetka in konca voznje, ime in stevilka
# zacetne in koncne postaje, stevilko kolesa ter tip narocnine.
#
#  Sledi spremeba v poimenovanju stolpcev, ki je dosledna za vse sledece
# podatke. Dodani se tudi stolpci za ID voznje, tip kolesa ter geografsko sirino in
# dolzino zacetne in koncne postaje. Odstranjen je stolpec za cas voznje.
#
#  S spodnjo zanko lahko preverimo, da so stolpci med seboj res dosledni v 
# tabli 1 do tabele 52 ter da so stolpci med seboj dosledni v tabeli 53 
# do zadnje tabele 73.

for (i in 1:length(ImenaStolpcevCB)) {
  if (i <= 52) {
    if (all(ImenaStolpcevCB[[i]] == ImenaStolpcevCB[[52]])) {
      ImenaStolpcevCB[[i]] = TRUE
    }
  }
  else {
    if (all(ImenaStolpcevCB[[i]] == ImenaStolpcevCB[[length(ImenaStolpcevCB)]])) {
      ImenaStolpcevCB[[i]] = TRUE
    }
  }
}
rm(i)
all(as.logical(unlist(ImenaStolpcevCB)))

#  Najprej preberemo in uredimo tabele od vkljucno 53 do 73. Dodamo stolpec 
#(duration), ki nam pove dolzino voznje v sekundah in odstranimo stolpec
# unikatnih imen vozenj (ride_id).

tbl2 <- 
  list.files(
    path = './podatki',
    pattern = 'tripdata',
    full.names = TRUE
  )[53:73] %>%
  map_df(
    ~read_csv(
      .,
      show_col_types = FALSE,
      col_types = cols(
        .default = col_guess(),
        start_station_id = col_double(),
        end_station_id = col_double(),
        member_casual = col_factor(),
        rideable_type = col_factor()
      )
    )
  ) %>%
  mutate(
    duration = difftime(ended_at, started_at) %>%
      as.duration() %>%
      as.numeric('seconds')
  ) %>%
  select(
    -ride_id
  )

#  Uredimo tabele 1 do vkljucno 52. Stolpce preimenujemo v skladu s tabelo tbl2,
# dodamo stolpec (rideable_type), ki opisuje tip kolesa. Vse vrednosti tega
# stolpca nastavimo na docked_bike, saj ce pogledamo prvih nekaj mesecev tabele
# tbl2 so vse vrednosti docked_bike, sele kasneje se pojavijo drugi vnosi.

tbl1 <- 
  list.files(
    path = './podatki',
    pattern = 'tripdata',
    full.names = TRUE
  )[1:52] %>%
  map_df(
    ~read_csv(
      .,
      show_col_types = FALSE,
      col_types = cols(
        .default = col_guess(),
        'Start station number' = col_double(),
        'End station number' = col_double(),
        'Member type' = col_factor()
      )
    )
  ) %>%
  rename(
    duration = 'Duration',
    started_at = 'Start date',
    ended_at = 'End date',
    start_station_id = 'Start station number',
    start_station_name = 'Start station',
    end_station_id = 'End station number',
    end_station_name = 'End station',
    member_casual = 'Member type'
  ) %>%
  select(
    -'Bike number'
  ) %>%
  mutate(
    rideable_type = 'docked_bike'
  )

#  Zelimo ustvariti loceno tabelo, ki vsebuje vse potrebne podatke o postajah.
# Preverimo ali tabela tbl2 vsebuje vse postaje, ki so v tabeli tbl1.

postaje_tbl1 <- tbl1$start_station_id %>%
  unique()

postaje_tbl2 <- tbl2$start_station_id %>%
  unique()

manjkajoce_postaje_id <- setdiff(postaje_tbl1,postaje_tbl2)

manjkajoce_postaje <- tbl1 %>%
  select(
    start_station_id,
    start_station_name
  ) %>%
  filter(
    start_station_id %in% manjkajoce_postaje_id
  ) %>%
  unique() %>%
  arrange(start_station_id)

#  Opazimo, da ne vseubje 6 postaj. Ko podrobneje pogledamo, imajo vse
# postaje kar nekaj vozenj, zato jih ne moremo kar ignorirati. Postaji 
# 22nd & H St NW in 22nd & H St  NW (disabled) bomo zdruzili v eno, saj si 
# delita station_id 0.
# (Naj opozorim, da JE pri drugi postaji med St in NW dvojni presledk)

#  Na sreco so postaje poimenovane po kriziscu cest in jih manjka le 5, njihove
# koordinate poiscemo na Google Maps in jih rocno dodamo v tabelo manjkajocih
# postaj.

tbl1 <- tbl1 %>%
  mutate(
    start_station_name = replace(
      start_station_name,
      start_station_name == '22nd & H  NW (disabled)',
      '22nd & H St NW'
      ),
      end_station_name = replace(
        end_station_name,
        end_station_name == '22nd & H  NW (disabled)',
        '22nd & H St NW'
      )
    )

manjkajoce_postaje <- manjkajoce_postaje %>%
  add_column(
    start_lat = c(38.89959, 38.86292, 38.88362, 39.09420, 38.92360),
    start_lng = c(-77.04884, -77.05183, -76.95754, -77.13259, -77.23131)
  )

#  Iz tabele tbl2 izluscimo id, ime in koordinate za vsako postajo. Ker se
# koordinate za iste postaje razlikujejo za nekaj decimalk vzamemo njihovo
# povprecje. Na koncu dodamo se manjkajoce postaje in dobimo tabelo vseh postaj.

postaje <- tbl2 %>%
  select(
    station_name = start_station_name,
    station_id = start_station_id,
    start_lat,
    start_lng
  ) %>%
  group_by(
    station_id,
    station_name
  ) %>%
  summarise(
    lat = mean(start_lat),
    lng = mean(start_lng)
  ) %>%
  ungroup(
    station_id,
    station_name
    ) %>%
  add_row(
    station_name = manjkajoce_postaje$start_station_name,
    station_id = manjkajoce_postaje$start_station_id,
    lat = manjkajoce_postaje$start_lat,
    lng = manjkajoce_postaje$start_lng
  ) %>%
  arrange(station_id) 

#  Opazimo tudi nekaj sumljivih postaj, ki striljo ven:
#
# 32900 (Motivate BX Tech office) 38.96441, -77.01076
# 32901 (6035 Warehouse) 38.96381, -77.01027
# 32902 (Motivate Tech Office) NA, NA
# NA (MTL-ECO5-03) NA, NA
# NA (NA) 38.91626 ,-77.02439
#
#  Zadnjo postajo, ki nima ne id stevilke ne imena, lahko takoj razlozimo.
# 17.6.2020 se prvic v bazi pojavi eletkricno kolo, ki si ga ni potrebno
# izposoditi oz. vrniti na doloceni postaji, temvec kjerkoli znoraj nekega
# omejenjga obomcja mesta. NA NA predstavlja povprecno lokacijo izposoje
# elektricnega kolesa.
#
#  Ostale bomo podrobneje pogledali tako, da bomo presteli stevilo opravljenih
# vozenj iz ali v postajo, maksimalni cas voznje, minimalni cas voznje in
# povprecni cas voznje (v sekundah).
#  OPOMBA: Spodnja kodo za sumljive_postaje bi se verjetno dalo napisati bistveno
# elegantnejse in  krajse, a tudi ta grda koda deluje.

sumljive_postaje <- bind_rows(
  tbl2 %>%
    filter(
      start_station_id == 32900 |
      end_station_id == 32900
    ) %>%
    mutate(
      count = n()
    ) %>%
    transmute(
      station_id = 32900,
      station_name = 'Motivate BX Tech office',
      count = count,
      max_duration = max(duration),
      min_duration = min(duration),
      avg_duration = mean(duration)
    ) %>%
    unique(),
  tbl2 %>%
    filter(
      start_station_id == 32901 |
      end_station_id == 32901
    ) %>%
    mutate(
      count = n()
    ) %>%
    transmute(
      station_id = 32901,
      station_name = '6035 Warehouse',
      count = count,
      max_duration = max(duration),
      min_duration = min(duration),
      avg_duration = mean(duration)
    ) %>%
    unique(),
  tbl2 %>%
    filter(
      start_station_id == 32902 |
      end_station_id == 32902
    ) %>%
    mutate(
      count = n()
    ) %>%
    transmute(
      station_id = 32902,
      station_name = 'Motivate Tech Office',
      count = count,
      max_duration = max(duration),
      min_duration = min(duration),
      avg_duration = mean(duration)
    ) %>%
    unique(),
  tbl2 %>%
    filter(
      start_station_name == 'MTL-ECO5-03' |
      end_station_name == 'MTL-ECO5-03'
    ) %>%
    mutate(
      count = n()
    ) %>%
    transmute(
      station_id = NA,
      station_name = 'MTL-ECO5-03',
      count = count,
      max_duration = max(duration),
      min_duration = min(duration),
      avg_duration = mean(duration)
    ) %>%
    unique()
)

#  Ko pogledamo tabelo sumljivih vidimi, da so vse zares sumljive, zato
# iz tabele tbl2 izlocimo voznje v in iz teh postaj, izlocimo jih tudi iz 
# tabele postaj tako, da vse postaje se enkrat izracunamo.

tbl2 <- tbl2 %>%
  filter(
    start_station_name %ni% sumljive_postaje$station_name,
    end_station_name %ni% sumljive_postaje$station_name
  )

postaje <- tbl2 %>%
  select(
    station_name = start_station_name,
    station_id = start_station_id,
    start_lat,
    start_lng
  ) %>%
  group_by(
    station_id,
    station_name
  ) %>%
  summarise(
    lat = mean(start_lat),
    lng = mean(start_lng)
  ) %>%
  ungroup(
    station_id,
    station_name
  ) %>%
  add_row(
    station_name = manjkajoce_postaje$start_station_name,
    station_id = manjkajoce_postaje$start_station_id,
    lat = manjkajoce_postaje$start_lat,
    lng = manjkajoce_postaje$start_lng
  ) %>%
  arrange(station_id)

#  Ko imamo tabelo postaj in njihovih koordinat lahko iz tabele tbl2 odstranimo
# koordinate, tabelo preuredimo in spnemo s tabelo tbl1.

tblCB <- bind_rows(
  tbl1 <- tbl1 %>%
    select(
      started_at,
      ended_at,
      start_station_id,
      end_station_id,
      member_casual,
      rideable_type
      ),
  tbl2 <- tbl2 %>%
    select(
      started_at,
      ended_at,
      start_station_id,
      end_station_id,
      member_casual,
      rideable_type
    )
)

#  Dolgo casa sem se spraseval kaksna je razlika med 'docked_bike' in
# 'classic_bike', saj v stolpcu ridable_type, saj slednji nenadoma izgine iz
# podatkov Aprila 2020 in je od takrat naprej naveden 'classic_bike', kasneje
# pa se mu pridruzi se 'electric_bike'. Sumil sem, da so le preimenovali in da
# razlik med njima ni. Potrdilo sem dobil v sledecem sporocilu:
#
# Hi Janez,
#
# In April 2020 we made some changes to the platform that Divvy runs on. Prior 
# to that date, a classic bike (not electric) is described as a "docked_bike" 
# in the monthly data, after April 2020 the same bikes are tracked as 
# "classic_bike". The bikes are the same, there's no difference between them 
# (other than the date-related change described above). Let us know if you have 
# any further questions.
#
# Regards,
# lyft

tblCB$rideable_type[tblCB$rideable_type=='docked_bike'] <- 'classic_bike'

#  Pospravimo nepotrebne tabele in spremenljivke in sprostimo spomin.

rm(
  ImenaStolpcevCB,
  postaje_tbl1,
  postaje_tbl2,
  manjkajoce_postaje_id,
  manjkajoce_postaje,
  sumljive_postaje,
  tbl1,
  tbl2
)

gc()

print(object.size(tblCB),units = 'Gb')

#  Vremenski podatki vsebujejo kar nekaj nevsecnosti. Podatki FMTM in PGTM so
# zabelezeni le do 1.4.2013, a jih lahko spustimo, saj so nepomembni vetrovni
# podatki. WESD vsebuje le NA in jih odstranimo. Podatki TAVG (Average 
# Temperature) se zacnejo beleziti sele 1.4.2013, kar bo pomembno vplivalno na 
# analizo. Podatki WT** (Weather types) so zabelezeni z 1, ce so resnicni in NA,
# ce niso, zato jih prebermo kot logical in NA spremenimo v FALSE.
# (Zagotovo obstaja elegantnejsa zapis z vektrojem.)

noaa <- read_csv(
  './podatki/NOAA.csv',
  show_col_types = FALSE,
  col_types = cols(
    .default = col_guess(),
    WT01 = col_logical(),
    WT02 = col_logical(),
    WT03 = col_logical(),
    WT04 = col_logical(),
    WT05 = col_logical(),
    WT06 = col_logical(),
    WT08 = col_logical(),
    WT09 = col_logical(),
    WT11 = col_logical(),
    WT13 = col_logical(),
    WT14 = col_logical(),
    WT15 = col_logical(),
    WT16 = col_logical(),
    WT17 = col_logical(),
    WT18 = col_logical(),
    WT21 = col_logical(),
    WT22 = col_logical()
  )
) %>%
  select(
    -STATION,
    -FMTM,
    -PGTM,
    -WESD
  ) %>%
  replace_na(
    list(
      WT01 = FALSE,
      WT02 = FALSE,
      WT03 = FALSE,
      WT04 = FALSE,
      WT05 = FALSE,
      WT06 = FALSE,
      WT08 = FALSE,
      WT09 = FALSE,
      WT11 = FALSE,
      WT13 = FALSE,
      WT14 = FALSE,
      WT15 = FALSE,
      WT16 = FALSE,
      WT17 = FALSE,
      WT18 = FALSE,
      WT21 = FALSE,
      WT22 = FALSE
    )
  )
