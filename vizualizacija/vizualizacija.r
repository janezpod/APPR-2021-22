# 3. faza: Vizualizacija podatkov

#  Najprej bomo malce na splosno pogledali le kolesarkse podatke, da dobimo
# obcutek kaj se sploh dogaja, nato bomo pocasi dodali ostale podatke.
# Posebaj se bomo omejili na pred Covid podatke tj. do vkljucno leta 2019

# 1. graf: V katerih letih so se najveckrat izposodili kolo

graf1 <- tblG1 %>%
  ggplot(
    mapping = aes(x = factor(year), y = n, fill = factor(member_type))
  ) +
  geom_col() +
  scale_fill_manual(
    labels = c('Nečlan', 'Član'),
    values = wes_palette('Zissou1', 2)
  ) +
  labs(
    title = paste('Število vo', '\u17e', 'enj po letih', sep = ''),
    x = 'Leto',
    y = paste('Število vo', '\u17e', 'enj (v letih)', sep = ''),
    fill = 'Tip uporabnika',
  ) +
  theme_classic()

#  Opazimo, da ustreza nasim pricakovanjem. Vec se vozijo tisti, ki imajo 
# clanarino in splosni upad v stevilu vozenj leta 2020 je smiselen zaradi Covida.

# 2. graf: V katerih letih so se najdlje izposodili kolo

graf2 <- tblG1 %>%
  ggplot(
    mapping = aes(x = factor(year), y = dur, fill = factor(member_type))
  ) +
  geom_col() +
  scale_fill_manual(
    labels = c('Nečlan', 'Član'),
    values = wes_palette('Zissou1', 2)
  ) +
  labs(
    title = paste('Skupni ', '\u010d' , 'as izposoje po letih', sep = ''),
    x = 'Leto',
    y = paste('Skupni ', '\u010d' , 'as izposoje', sep = ''),
    fill = 'Tip uporabnika',
  ) +
  theme_classic()

#  Morda je na prvi pogled presenetljivo, da si neclani, navkljub manjsemu
# stevilu vozenj, koleas skupno izposodijo za dlje kot clani. Ena izmed moznih
# interpretacij je, da si kolo v vecini izposojajo turisti oz ljudje, ki se
# podajo na daljse scenicne voznje in si ogledajo mesto. Med clani pa
# prevladujejo uporabniki, ki kolo uporabljajo za vsakdanji prevoz med sluzbo in
# domom.
#  Zelo zanimivo je tudi opaziti, da navkljub upadu vozenj v letu 2020 in 2021,
# se je skupni cas izposoje povecal, predvsem med neclani. Morda so ljudje
# raje namesto avtobusa in ostalih potniskih storitev tudi za daljse prevoze
# posegli po kolesu, ceprav manjkrat (saj so recimo vecino casa delali od doma).

# 3. graf: Povprecni cas voznje

graf3 <- tblG1 %>%
  filter(
    year > 2017
  ) %>%
  ggplot(
    mapping = aes(x = factor(season), y = dur_avg, fill = factor(season))
  ) +
  geom_col() +
  scale_fill_manual(
    labels = c('Zima', 'Pomlad', 'Poletje', 'Jesen'),
    values = c('cornflowerblue', 'green3', 'orangered', 'orange2')
  ) +
  scale_x_discrete(
    labels = c('Zima', 'Pomlad', 'Poletje', 'Jesen'),
  ) + 
  labs(
    title = paste('Povprecni ', '\u010d', 'as izposoje po letnem ', '\u010d', 'asu', sep = ''),
    x = paste('Letni ', '\u010d' , 'as', sep = ''),
    y = paste('Povprecni ', '\u010d', 'as izposoje (v minutah)', sep = ''),
    fill = paste('Letni ', '\u010d', 'as', sep  = ''),
  ) +
  facet_grid(
    col = vars(year),
    row = vars(member_type),
    labeller = labeller(member_type = c('casual'='Nečlan', 'member'='Član'))
  ) + 
  theme(
    legend.position="none"
  ) +
  theme_minimal()
  
#  Pricakovano je povprecni cas voznje visji za neclane kot za clane. Zanimivo
# je da se je povprecni cas vozenj povecal predvsem v spomladanskih in poletnih
# casih. Zaradi zaprtosti doma so morda vec kolesarili v sprostitvene namene.

# 4. graf: Ali je potrebna delitev na mesece (oz ali je delitev na letne case
# dovolj) za stevilo vozenj

graf4 <- tblG1 %>%
  filter(
    year > 2017
  ) %>%
  ggplot(
    mapping = aes(x = factor(month), y = n, fill = factor(season))
  ) +
  geom_col() +
  scale_fill_manual(
    labels = c('Zima', 'Pomlad', 'Poletje', 'Jesen'),
    values = c('cornflowerblue', 'green3', 'orangered', 'orange2')
  ) +
  labs(
    title = paste('Primerjava števila vo', '\u17e', 'enj po letihi in mesecih', sep = ''),
    x = 'Mesec',
    y = paste('Število vo', '\u17e', 'enj', sep = ''),
    fill = paste('Letni ', '\u010d', 'as', sep  = ''),
  ) +
  facet_grid(
    col = vars(year),
    row = vars(member_type),
    labeller = labeller(member_type = c('casual'='Nečlan', 'member'='Član'))
  ) +
  theme_minimal()

#  Ugotovimo, da je delitev na letne case dovolj in, da z delitvijo na posamezne
# mesece ne dobimo bistveno več uporabnih informacij. Hkrati opazimo ociten 
# vpliv covida v mesecu Marcu in Aprilu leta 2020, ko za Clane stevilo vozenj
# pade (ko bi moralo narasti). Zanimivo je, da je upad vozenj pri clanih v 
# zimskem casu manjsu kot pri neclanih. Morda se raven clanov v letu 2021 ni
# vrnila nazaj tako visoko ker so si v tem casu kupili svoja kolesa.

# 5. graf: Stevilo vozenj po dnevih in letih

graf5 <- tblG1 %>%
  ggplot(
    mapping = aes(x = factor(day), y = n, fill = factor(season))
  ) +
  geom_col() +
  scale_x_discrete(
    limits = c('2', '3', '4', '5', '6', '7', '1'),
    labels = c('PON', 'TOR', 'SRE', paste('\u010c', 'ET', sep = ''), 'PET', 'SOB', 'NED')
  ) +
  scale_fill_manual(
    labels = c('Zima', 'Pomlad', 'Poletje', 'Jesen'),
    values = c('cornflowerblue', 'green3', 'orangered', 'orange2')
  ) +
  labs( 
    title = paste('Primerjava števila vo', '\u17e', 'enj po dneh', sep = ''),
    x = 'Dan v tednu',
    y = paste('Število vo', '\u17e', 'enj', sep = ''),
    fill = paste('Letni ', '\u010d', 'as', sep  = '')
  ) +
  facet_grid(
    row = vars(member_type),
    labeller = labeller(member_type = c('casual'='Nečlan', 'member'='Član'))
  ) +
  theme_minimal()

#  Clani ocitno vec kolesarijo med delavnikom, medtem ko Neclani uporabljajo 
# storitve bolj med vikendom

# 6. graf: Delez vozenj z elektricnimi kolesi (prvic se pojavijo Julija 2020)

graf6 <- tblG1 %>%
  filter(
    year >= 2020,
    month >= 6
  ) %>%
  group_by(
    rideable_type
  ) %>%
  summarise(
    n = sum(n)
  ) %>%
  mutate(
    all = sum(n),
    p = round((n / all) * 100)
  ) %>%
  select(
    rideable_type,
    p
  ) %>%
  ggplot(
    mapping = aes(x = '', y = p, fill = factor(rideable_type))
  ) +
  geom_bar(
    stat = 'identity',
    width = 1
  ) +
  coord_polar(
    'y'
  ) +
  geom_text(
    aes(label = c('83 %', '17 %')),
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(
    labels = c('Navadno kolo', 'Električno kolo'),
    values = wes_palette('GrandBudapest1', 2)
  ) +
  labs(
    title = 'Izposoja po tipu kolesa od Julija 2020 do konca leta 2021',
    fill = 'Tip kolesa'
  ) +
  theme_void()

#  Nesmiselno je primerjati cas voznje z elektricnim proti navadnim, saj nevemo
# hitrosti. Vendar je 17 % nezamerljivo velik delez.

# 1. zemljevid: Poglejmo kje se nahajajo postaje

zemljevid1 <- c(left = -77.4, bottom = 38.75, right = -76.75, top = 39.15) %>%
  get_stamenmap(
    zoom = 11,
    maptype = 'toner-lite'
  ) %>%
  ggmap(
    extent = 'device',
    legend= 'none'
  ) + 
  geom_point(
    data = tblZ1,
    aes(
      x=lng,
      y=lat
      ),
    fill = 'red',
    shape = 23,
    alpha = 0.6
  ) +
  labs(
    x = 'Zemljepisna dolžina',
    y = 'zemljepisna širina'
  ) + 
  ggtitle(
    'Lokacija postaj'
  ) +
  theme_bw(
  ) +
  theme(
    legend.position = 'none'
  )

# 2. zemljevid: Gostota postaj je bolje razvidna

zemljevid2 <- c(left = -77.4, bottom = 38.75, right = -76.75, top = 39.15) %>%
  get_stamenmap(
    zoom = 11,
    maptype = 'toner-lite'
  ) %>%
  ggmap(
    extent = 'device',
    legend= 'none'
  ) + 
  stat_density2d(
    data = postaje,
    aes( 
      x=lng,
      y=lat,
      fill=..level..,
      alpha=..level..
      ),
    geom="polygon"
  ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(10, "Spectral"))
  ) +
  labs(
    x = 'Zemljepisna dolžina',
    y = 'zemljepisna širina'
  ) + 
  ggtitle(
    'Gostota postaj'
  ) +
  theme_bw(
  ) +
  theme(
    legend.position = 'none'
  )

#  Postaje urejene po številu izposoj

tblZ1 <- left_join(
  tblG2 %>%
    rename(
      station_id = start_station_id
    ) %>%
    group_by(
      station_id
    ) %>%
    summarise(
      izposoje = sum(n)
    ),
  postaje
) %>%
  arrange(
    desc(izposoje)
  ) %>%
  drop_na(
  ) %>%
  slice(1:15)

#  Postaje urejene po številu vrnitev

tblZ2 <- left_join(
  tblG2 %>%
    rename(
      station_id = end_station_id
    ) %>%
    group_by(
      station_id
    ) %>%
    summarise(
      vrnitve = sum(n)
    ),
  postaje
) %>%
  arrange(
    desc(vrnitve)
  ) %>%
  drop_na(
  ) %>%
  slice(1:15)

# 3. zemljevid: 15 najbolj prometnih postaj za izposojo kolesa

zemljevid3 <- c(left = -77.07, bottom = 38.873, right = -76.975, top = 38.925) %>%
  get_stamenmap(
    zoom = 14,
    maptype = 'toner-lite'
  ) %>%
  ggmap(
    extent = 'device',
    legend= 'none'
  ) +
  geom_point(
    data = tblZ1,
    aes(
      x=lng,
      y=lat,
      fill = izposoje
    ),
    alpha = 0.6,
    shape = 23,
    size = 5
  ) +
  labs(
    x = 'Zemljepisna dolžina',
    y = 'zemljepisna širina',
    fill = 'Izposoja koles'
  ) + 
  ggtitle(
    '15 najbolj prometnih postaj'
  ) +
  theme_bw(
  )

# 4. zemljevid: 15 najbolj prometnih postaj za vrnitev kolesa

zemljevid4 <- c(left = -77.07, bottom = 38.873, right = -76.975, top = 38.925) %>%
  get_stamenmap(
    zoom = 14,
    maptype = 'toner-lite'
  ) %>%
  ggmap(
    extent = 'device',
    legend= 'none'
  ) +
  geom_point(
    data = tblZ2,
    aes(
      x=lng,
      y=lat,
      fill = vrnitve
    ),
    alpha = 0.6,
    shape = 23,
    size = 5
  ) +
  
  labs(
    x = 'Zemljepisna dolžina',
    y = 'zemljepisna širina',
    fill = 'Vritve koles'
  ) + 
  ggtitle(
    '15 najbolj prometnih postaj'
  ) +
  theme_bw(
  )
