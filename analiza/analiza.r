# 4. faza: Napredna analiza podatkov

#  Za zacetek si poglejmo korelacij med izbranimi spremenljivkami

tblMk <- tblM %>%
  ungroup(
  ) %>%
  dplyr::select(
    n,
    dur_avg,
    cases_incidence,
    tavg_aug,
    prcp,
    member_type
  ) 

korelacije1 <- tblMk %>%
  ggpairs(
    columns = 1:5,
    ggplot2::aes(colour=member_type),
    columnLabels = c(
      'Število voženj',
      paste('\u010c', 'as vožnje', sep = ''),
      'Covid-19',
      'Temperatura',
      'Padavine'
    )
  ) +
  ggtitle(
    'Korelacije in porazdelitve'
  )
  
korelacije2 <- tblMk[1:5] %>% 
  rename(
    st_vozenj = n,
    povp_cas = dur_avg,
    pojavnost_cov = cases_incidence,
    povp_tem = tavg_aug,
    dez = prcp
  ) %>%
  ggcorr(
    nbreaks = 10,
    label  = TRUE,
    size = 4,
    hjust = 0.75
  ) +
  ggtitle(
    'Korelacije'
  )
  
#  Ker se porazdelitve na korelaciji1 slabo vidijo za Covid-19 in za dez jih
# poglejmo se v posebnem primeru, ko so > 0.

porazdelitev1 <- tblM %>%
  filter(
    cases_incidence > 0
  ) %>%
  ggplot(
    mapping = aes(
      x = cases_today
    )
  ) +
  geom_density(
    color='darkblue',
    fill='lightblue'
  ) +
  labs(
    title = 'Porazdelitev primerov Covid-19, ko so večji od 0',
    x = 'Potrjeni primeri Covid-19 na dan',
    y = 'Gostota',
  ) +
  theme_classic()

porazdelitev2 <- tblM %>%
  filter(
    prcp > 0
  ) %>%
  ggplot(
    mapping = aes(
      x = prcp
    )
  ) +
  geom_density(
    color='darkblue',
    fill='lightblue'
  ) +
  labs(
    title = 'Porazdelitev količine dežja, ko je večja od 0',
    x = 'Količina padavin (mm)',
    y = 'Gostota',
  ) +
  theme_classic()

tblM2 <- tblM %>%
  ungroup(
  ) %>% 
  dplyr::select(
    n,
    member_type,
    tavg_aug,
    cases_incidence
    )

tblM1 <- tblM2 %>% 
  dplyr::select(
    -cases_incidence
  )

#  Razdelimo sedaj podatke na ucne in testne

set.seed(42)
tr <- createDataPartition(
  tblM$n, 
  p = .75, 
  list = F
  )

tblM_ucni <- tblM[tr, ]
tblM_testni <- tblM[-tr, ]

tblM1_ucni <- tblM_ucni %>%
  ungroup(
    ) %>% 
  dplyr::select(n,
         tavg_aug
  )

tblM1_testni <- tblM_testni %>% 
  ungroup(
  ) %>% 
  dplyr::select(n,
         tavg_aug
  )

tblM2_ucni <- tblM_ucni %>% 
  ungroup(
  ) %>%
  dplyr::select(n,
         tavg_aug,
         cases_incidence,
         prcp, 
         snwd, 
         wt09
  )

tblM2_testni <- tblM_testni %>%
  ungroup(
  ) %>% 
  dplyr::select(n,
         tavg_aug, 
         cases_incidence,
         prcp,
         snwd, 
         wt09
  )

tblM3_ucni <- tblM_ucni %>% 
  ungroup(
  ) %>% 
  dplyr::select(
    -year,
    -season,
    -day, 
    -rideable_type,
    -date,
    -dur, 
    -dur_avg
  )

tblM3_testni <- tblM_testni %>%
  ungroup(
  ) %>% 
  dplyr::select(
    -year, 
    -season,
    -day, 
    -rideable_type,
    -date,
    -dur, 
    -dur_avg
  )

#  Naucimo najprej modele z linearno regresijo

lin.model1a <- tblM1_ucni %>%
  ucenje(n ~ tavg_aug, 'lin.reg')
summary(lin.model1a)

lin.model2 <-  tblM2_ucni %>%
  ucenje(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, 'lin.reg')
summary(lin.model2)

lin.model3 <- tblM3_ucni %>%
  ucenje(n ~ ., 'lin.reg')
summary(lin.model3)

#  Pa se z nakljucnimi gozdovi

ng.reg.model1 <- tblM1_ucni %>%
  ucenje(n ~ tavg_aug, 'ng')
summary(ng.reg.model1)
print(ng.reg.model1)

ng.reg.model2 <-  tblM2_ucni %>%
  ucenje(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, 'ng')
summary(ng.reg.model2)
print(ng.reg.model2)

ng.reg.model3 <- tblM3_ucni %>%
  ucenje(n ~ ., 'ng')
summary(ng.reg.model3)
print(ng.reg.model3)

#  Napovedi in primerjava modelov

#  Napoved linearnega modela 1a
napoved_testni_lin.model1a <- predict(lin.model1a, tblM1_testni)
lin1a <- postResample(napoved_testni_lin.model1a, tblM1_testni$n)

#  Napoved linearnega modela 2
napoved_testni_lin.model2 <- predict(lin.model2, tblM2_testni)
lin2 <- postResample(napoved_testni_lin.model2, tblM2_testni$n)

#  Napoved linearnega modela 3
napoved_testni_lin.model3 <- predict(lin.model3, tblM3_testni)
lin3 <- postResample(napoved_testni_lin.model3, tblM3_testni$n)

#  'Napoved nakljucnega gozda 1
napoved_testni_ng.reg.model1 <- predict(ng.reg.model1 , tblM1_testni)$prediction
ng1 <- postResample(napoved_testni_ng.reg.model1, tblM1_testni$n)

#  Napoved nakljucnega gozda 2
napoved_testni_ng.reg.model2 <- predict(ng.reg.model2 , tblM2_testni)$prediction
ng2 <- postResample(napoved_testni_ng.reg.model2, tblM2_testni$n)

#  Napoved nakljucnega gozda 3 
napoved_testni_ng.reg.model3 <- predict(ng.reg.model3 , tblM3_testni)$prediction
ng3 <- postResample(napoved_testni_ng.reg.model3, tblM3_testni$n)

#  Sestavimo tabelo napak.

tblN <- bind_rows(
  lin1a,
  lin2,
  lin3,
  ng1,
  ng2,
  ng3
) %>%
  add_column(
    model = c(
      'lin1a',
      'lin2',
      'lin3',
      'ng1',
      'ng2',
      'ng3'
    )
  ) %>%
  relocate(
    model
  )

# #  Pomen tipa clanstva
# lin.model1b = lm(n ~ member_type + tavg_aug + member_type * tavg_aug, data = tblM1)
# summary(lin.model1b)
# #  Plot linearnega modela 1b (na testnih podatkih)
# plot(n~tavg_aug,tblM1,col=tblM$member_type,pch=20)
# curve(predict(lin.model1b,newdata=data.frame(tavg_aug=x,member_type='casual')),col=1,add=T)
# curve(predict(lin.model1b,newdata=data.frame(tavg_aug=x,member_type='member')),col=2,add=T)

grafN1 <- tblN %>%
  pivot_longer(
    .,
    cols = c(RMSE, MAE),
    names_to = 'mesure_type',
    values_to = 'mesure'
  ) %>%
  ggplot(
  ) + 
  geom_col(
    aes(
      x = mesure_type,
      y = mesure
    )
  ) +
  facet_grid(
    cols = vars(model)
  ) +
  labs( 
    title = 'Primerjava napak Root-mean-square deviation (RMSE) in Mean absolute error (MAE)',
    x = 'Tip napake',
    y = ''
  ) +
  theme_minimal()

grafN2 <- tblN %>%
  ggplot(
  ) + 
  geom_col(
    aes(
      x = model,
      y = Rsquared
    )
  ) +
  ylim(
    0,
    1
  ) +
  labs( 
    title = 'Primerjava napak Coefficient of determination (Rsquared)',
    x = 'Model',
    y = 'Rsquared'
  ) +
  theme_minimal()

#  Precno preverjanje

tblM1_ucni_i <- indikator.nd(tblM1_ucni)
tblM2_ucni_i <- indikator.nd(tblM2_ucni)
tblM3_ucni_i <- indikator.nd(tblM3_ucni)
pp_tblM1_ucni <- pp.razbitje(tblM1_ucni_i, stratifikacija = tblM1_ucni_i$nd)
pp_tblM2_ucni <- pp.razbitje(tblM2_ucni_i, stratifikacija = tblM2_ucni_i$nd)
pp_tblM3_ucni <- pp.razbitje(tblM3_ucni_i, stratifikacija = tblM3_ucni_i$nd)

print(precno.preverjanje(tblM1_ucni, pp_tblM1_ucni, n ~ tavg_aug, 'lin.reg', FALSE))
print(precno.preverjanje(tblM2_ucni, pp_tblM2_ucni, n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, 'lin.reg', FALSE))
print(precno.preverjanje(tblM3_ucni, pp_tblM3_ucni, n ~ ., 'lin.reg', FALSE))

print(precno.preverjanje(tblM1_ucni, pp_tblM1_ucni, n ~ tavg_aug, 'ng', FALSE))
print(precno.preverjanje(tblM2_ucni, pp_tblM2_ucni, n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, 'ng', FALSE))
print(precno.preverjanje(tblM3_ucni, pp_tblM3_ucni, n ~ ., 'ng', FALSE))


#  1. Hierarhično razvrščanje v skupine

#  Dendrogram vremenskih podatkov

razdalje <- noaaM[,-1] %>%
  dist()

dendrogram <- razdalje %>%
  hclust(method = 'ward.D')

plot(
  dendrogram,
  labels = FALSE,
  ylab = 'višina',
  main = NULL
)

#  Tabela kolen dendrograma
r = hc.kolena(dendrogram)

dendrogram1  <- diagram.kolena(r) +
  scale_x_discrete(
    breaks = seq(0, 364, 30)
  )

dendrogram2  <- diagram.kolena(r[1:60,]) +
  scale_x_discrete(
    breaks = seq(0, 60, 2)
  )

#  2. Metoda k-tih voditeljev

tblM.hc <- noaaM[,-1] %>%
  obrisi(hc = TRUE)

diagram1hc <- tblM.hc %>%
  diagram.obrisi() +
  scale_x_discrete(
    breaks = seq(0, 364, 30)
  )

diagram2hc <- tblM.hc %>%
  filter(
    k <= 30
  ) %>%
  diagram.obrisi()

tblM.km <- noaaM[,-1] %>%
  obrisi(hc = FALSE)

diagram1mk <- tblM.km %>%
  diagram.obrisi() +
  scale_x_discrete(
    breaks = seq(0, 364, 30)
  )

diagram2mk <- tblM.km %>%
  filter(
    k <= 30
  ) %>%
  diagram.obrisi()

datumi <- noaaM[, 1]

noaaXY <- 
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(datumi) %>%
  dplyr::select(datumi = date, x = V1, y = V2)

k1 = obrisi.k(tblM.km)

skupine1 = noaaM[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k1) %>%
  as.ordered()

k2 = obrisi.k(tblM.hc)

skupine2 = noaaM[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k2) %>%
  as.ordered()


skupineG1 <- diagram.skupine(noaaXY, noaaXY$datumi, skupine1, k1)

skupineG2 <- diagram.skupine(noaaXY, noaaXY$datumi, skupine2, k2)
