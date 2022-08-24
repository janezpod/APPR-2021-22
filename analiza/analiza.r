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

# rm(
#   tr,
#   tblM,
#   tblMk
# )

#  Naucimo najprej modele z linearno regresijo

lin.model1a = lm(n ~ tavg_aug, data = tblM1_ucni)
summary(lin.model1a)

lin.model2 = lm(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, data = tblM2_ucni)
summary(lin.model2)

lin.model3 = lm(n ~ ., data = tblM3_ucni)
summary(lin.model3)

#  Pa se z nakljucnimi gozdovi

ng.reg.model1 = ranger(n ~ tavg_aug, tblM1_ucni)
summary(ng.reg.model1)
print(ng.reg.model1)

ng.reg.model2 = ranger(n ~ tavg_aug + cases_incidence + prcp + snwd + wt09, tblM2_ucni)
summary(ng.reg.model2)
print(ng.reg.model2)

ng.reg.model3 = ranger(n ~ ., tblM3_ucni)
summary(ng.reg.model3)
print(ng.reg.model3)

#  Primerjava modelov

print('Napoved linearnega modela 1a')
napoved_testni_lin.model1a <- predict(lin.model1a, tblM1_testni)
postResample(napoved_testni_lin.model1a, tblM1_testni$n)

print('Napoved linearnega modela 2')
napoved_testni_lin.model2 <- predict(lin.model2, tblM2_testni)
postResample(napoved_testni_lin.model2, tblM2_testni$n)

print('Napoved linearnega modela 3')
napoved_testni_lin.model3 <- predict(lin.model3, tblM3_testni)
postResample(napoved_testni_lin.model3, tblM3_testni$n)

print('Napoved nakljucnega gozda 1')
napoved_testni_ng.reg.model1 <- predict(ng.reg.model1 , tblM1_testni)$prediction
postResample(napoved_testni_ng.reg.model1, tblM1_testni$n)

print('Napoved nakljucnega gozda 2')
napoved_testni_ng.reg.model2 <- predict(ng.reg.model2 , tblM2_testni)$prediction
postResample(napoved_testni_ng.reg.model2, tblM2_testni$n)

print('Napoved nakljucnega gozda 3')
napoved_testni_ng.reg.model3 <- predict(ng.reg.model3 , tblM3_testni)$prediction
postResample(napoved_testni_ng.reg.model3, tblM3_testni$n)


#  Pomen tipa clanstva
lin.model1b = lm(n ~ member_type + tavg_aug + member_type * tavg_aug, data = tblM1)
summary(lin.model1b)
#  Plot linearnega modela 1b (na testnih podatkih)
plot(n~tavg_aug,tblM1,col=tblM$member_type,pch=20)
curve(predict(lin.model1b,newdata=data.frame(tavg_aug=x,member_type='casual')),col=1,add=T)
curve(predict(lin.model1b,newdata=data.frame(tavg_aug=x,member_type='member')),col=2,add=T)

