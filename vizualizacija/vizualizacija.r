# 3. faza: Vizualizacija podatkov

#  Najprej bomo malce na splosno pogledali le kolesarkse podatke, da dobimo
# obcutek kaj se sploh dogaja, nato bomo pocasi dodali ostale podatke.
# Posebaj se bomo umejili na pre Covid podatke tj. do vkljucno leta 2019

tbl <- tbl1 %>%
  slice(which(row_number() %% 1000 == 1))

# 1. graf: V katerih letih so se najveckrat izposodili kolo

tblG1 <- tbl1 %>%
  group_by(
    year,
    month,
    season,
    member_type
  ) %>%
  summarise(
    n = n(),
    dur = sum(duration)/ (60 * 24 * 365), # v letih
    dur_avg = dur / n * (24 * 365) # v minutah
  )

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
    title = 'Število voženj po letih',
    x = 'Leto',
    y = 'Število voženj',
    fill = 'Tip uporabnika',
  ) 

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
    title = 'Skupni čas izposoje po letih',
    x = 'Leto',
    y = 'Skupno čas izposoje (v letih)',
    fill = 'Tip uporabnika',
  ) 

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
    title = 'Povprecni čas izposoje po letnem času',
    x = 'Letni čas',
    y = 'Povprečni čas izposoje (v minutah)',
  ) +
  facet_grid(
    col = vars(year),
    row = vars(member_type),
    labeller = labeller(member_type = c('casual'='Nečlan', 'member'='Član'))
  ) + 
  theme(legend.position="none")
  
#  Pricakovano je povprecni cas voznje visji za neclane kot za clane. Zanimivo
# je da se je povprecni cas vozenj povecal predvsem v spomladanskih in poletnih
# casih. Zaradi zaprtosti doma so morda vec kolesarili v sprostitvene namene.

# 4. graf: Ali je potrebna delitev na mesece (oz ali je delitev na letne čase
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
    title = 'Primerjava števila voženj po letihi in mesecih',
    x = 'Mesec',
    y = 'Število voženj',
    fill = 'Letni čas',
  ) +
  facet_grid(
    col = vars(year),
    row = vars(member_type),
    labeller = labeller(member_type = c('casual'='Nečlan', 'member'='Član'))
  )

#  Ugotovimo, da je delitev na letne čase dovolj in, da z delitvijo na posamezne
# mesece ne dobimo bistveno več uporabnih informacij. Hkrati opazimo ociten 
# vpliv covida v mesecu Marcu in Aprilu leta 2020, ko za Clane stevilo vozenj
# pade (ko bi moralo narasti).
