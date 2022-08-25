source('lib/libraries.r', encoding='UTF-8')

# Negacija funkcije in
`%ni%` <- Negate(`%in%`)

# Sprejeme leto v formatu YYYY in je vkljucno od in do izbranega leta
PrenosCB <- function(OdLeto = 2010, DoLeto = OdLeto) {
  bucket <- get_bucket('capitalbikeshare-data')
  bucket <- bucket[-length(bucket)]
  seznam <- vector('character', length(bucket))
  if (missing(DoLeto)) {
    DoLeto <- OdLeto
  }
  for (i in bucket) {
    leto <- i$Key %>% 
      substr(1,4) %>% 
      strtoi()
    
    if (leto >= OdLeto & leto <= DoLeto) {
      temp <- tempfile()
      save_object(i$Key, file = temp, bucket = "capitalbikeshare-data")
      unzip(temp, exdir = './podatki', junkpaths = TRUE )
      unlink(temp)
    }
  }
}

# Prebere le imena stolpcev v seznam csv datotek v odvisnosti od vzorca
ImenaStolpcev <- function(pattern) {
  ImeDatotekCB <- list.files('./podatki', pattern = pattern, full.names = TRUE)
  ImeStolpcevCB <- vector(mode = 'list', length = length(ImeDatotekCB))
  k <- 1
  for (i in ImeDatotekCB) {
    stolpci <- read_csv(i, col_names = TRUE, n_max = 0, show_col_types = FALSE) %>%
      colnames()
    ImeStolpcevCB
    ImeStolpcevCB[[k]] <- stolpci
    k <- k +1
  }
  return(ImeStolpcevCB)
}

# 14-dnevno pojavnost izposojena iz ucilnice predmeta
drseca_vsota = function(x, n = 14) {
  cx = rep(0, length(x))
  for (i in seq_along(x)) {
    if (i < n) {
      cx[i] = NA
    } else {
      cx[i] = sum(x[(i-n+1):i])
    }
  }
  return(cx)
}

# Učenje modelov
ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    ng= ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    log.reg = ifelse(
      predict(model, podatki, type = "response") >= 0.5,
      1, -1
    ),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yn.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yn - yn.hat) ^ 2
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yd.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yd != yd.hat)
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

# Razrez vektor x na k enako velikih delov
razbitje = function(x, k) {
  
  # Razreži indekse vektorja na k intervalov
  razrez = cut(seq_along(x), k, labels = FALSE)
  
  # Razbij vektor na k seznamov
  # na osnovi razreza intervalov
  split(x, razrez)
}

pp.razbitje = function(n, k = 10, stratifikacija = NULL, seme = NULL) {
  
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
    return(razbitje(sample(1:n), k))
}

# Doda diskretno spremenljivka nd, ki ima vrednost 1, za n > mediana in -1 sicer.
indikator.nd = function(tbl) {
  tbl %>% 
    mutate(
      nd = if_else( n > median(tbl$n), 1, -1),
      nd = factor(nd, levels = c(-1,1))
    ) 
}

pp.razbitje = function(n, k = 10, stratifikacija = NULL, seme = NULL) {
  
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
  if (is.null(stratifikacija)) {
    return(razbitje(sample(1:n), k))
  }
  
  # če pa opravljamo stratifikacijo, razbitje izvedemo za vsako
  # vrednost spremenljive stratifikacija posebej in nato
  # podmnožice združimo v skupno razbitje
  r = NULL
  for (v in levels(stratifikacija)) {
    
    # Če smo pri prvi vrednosti vzpostavimo razbitje
    if (is.null(r)) {
      # opravimo razbitje samo za primere z vrednostjo v
      r = razbitje(sample(which(stratifikacija == v)), k)
    } else {
      # opravimo razbitje za vrednost v
      # in podmnožice združimo s trenutnim razbitjem
      r.v = razbitje(sample(which(stratifikacija == v)), k)
      for (i in 1:k) {
        r[[i]] = c(r[[i]], r.v[[i]])
      }
    }
  }
  r
}

precno.preverjanje = function(podatki, razbitje, formula, algoritem, razvrscanje) {
  
  # pripravimo vektor za napovedi
  if (razvrscanje) {
    pp.napovedi = factor(rep(1, nrow(podatki)), levels = c(-1,1))
  } else {
    pp.napovedi = rep(0, nrow(podatki))
  }
  
  # gremo čez vse podmnožice Si razbitja S
  for (i in 1:length(razbitje)) {
    # naučimo se modela na množici S \ Si
    model = podatki[ -razbitje[[i]], ] %>% ucenje(formula, algoritem)
    
    # naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = podatki[ razbitje[[i]], ] %>% napovedi(model, algoritem)
  }
  
  if (razvrscanje) {
    mean(pp.napovedi != podatki$n)
  } else {
    mean((pp.napovedi - podatki$n) ^ 2)
  }
}
