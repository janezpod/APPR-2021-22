# 4. faza: Napredna analiza podatkov

#  Za zacetek si poglejmo korelacij med izbranimi spremenljivkami

tblMk <- tblM %>%
  ungroup(
  ) %>%
  select(
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
  )
  
korelacije2 <- tblMk[1:5] %>% 
  ggcorr(
    nbreaks = 10,
    label  = TRUE,
    size = 4,
    hjust = 0.75
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
    title = 'Porazdelitev primerov Covid-19',
    x = 'Primeri Covid-19',
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
    title = 'Porazdelitev količine dežja',
    x = 'Količina padavin (mm)',
    y = 'Gostota',
  ) +
  theme_classic()

tblM2 <- tblM %>%
  ungroup(
  ) %>% 
  select(
    n,
    member_type,
    tavg_aug,
    cases_incidence
    )

tblM1 <- tblM2 %>% 
  select(
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
  select(n,
         tavg_aug
  )

tblM1_testni <- tblM_testni %>% 
  ungroup(
  ) %>% 
  select(n,
         tavg_aug
  )

tblM2_ucni <- tblM_ucni %>% 
  ungroup(
  ) %>%
  select(n,
         tavg_aug,
         cases_incidence,
         prcp, 
         snwd, 
         wt09
  )

tblM2_testni <- tblM_testni %>%
  ungroup(
  ) %>% 
  select(n,
         tavg_aug, 
         cases_incidence,
         prcp,
         snwd, 
         wt09
  )

tblM3_ucni <- tblM_ucni %>% 
  ungroup(
  ) %>% 
  select(
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
  select(
    -year, 
    -season,
    -day, 
    -rideable_type,
    -date,
    -dur, 
    -dur_avg
  )

rm(
  tr,
  tblM,
  tblMk
)

#  Naucimo najprej modele z linearno regresijo

lin.model_1a = lm(n ~ tavg_aug, data = tblM1_ucni)
summary(lin.model_1a)

lin.model_2 = lm(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, data = tblM2_ucni)
summary(lin.model_2)

lin.model_3 = lm(n ~ ., data = tblM3_ucni)
summary(lin.model_3)

#  Pa se z nakljucnimi gozdovi

ng.reg.model_1 = ranger(n ~ tavg_aug, tblM1_ucni)
summary(ng.reg.model_1)
print(ng.reg.model_1)

ng.reg.model_2 = ranger(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, tblM2_ucni)
summary(ng.reg.model_2)
print(ng.reg.model_2)

ng.reg.model_3 = ranger(n ~ ., tblM3_ucni)
summary(ng.reg.model_3)
print(ng.reg.model_3)

#  Primerjava modelov

print('Napoved linearnega modela 1a')
napoved_testni_lin.model_1a <-predict(lin.model_1a, tblM1_testni)
postResample(napoved_testni_lin.model_1a, tblM1_testni$n)

print('Napoved linearnega modela 2')
napoved_testni_lin.model_2 <-predict(lin.model_2, tblM2_testni)
postResample(napoved_testni_lin.model_2, tblM2_testni$n)

print('Napoved linearnega modela 3')
napoved_testni_lin.model_3 <-predict(lin.model_3, tblM3_testni)
postResample(napoved_testni_lin.model_3, tblM3_testni$n)

print('Napoved nakljucnega gozda 1')
napoved_testni_ng.reg.model_1 <-predict(ng.reg.model_1 , tblM1_testni)$prediction
postResample(napoved_testni_ng.reg.model_1, tblM1_testni$n)


print('Napoved nakljucnega gozda 2')
napoved_testni_ng.reg.model_2 <-predict(ng.reg.model_2 , tblM2_testni)$prediction
postResample(napoved_testni_ng.reg.model_2, tblM2_testni$n)

print('Napoved nakljucnega gozda 3')
napoved_testni_ng.reg.model_3 <-predict(ng.reg.model_3 , tblM3_testni)$prediction
postResample(napoved_testni_ng.reg.model_3, tblM3_testni$n)


