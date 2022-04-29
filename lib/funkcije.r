library(aws.s3)
# Sprejeme leto v formatu YYYY in je vkljucno od in do vnesenega leta
PrenosCB <- function(OdLeto = 2010, DoLeto) {
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

# Prebere imena stolpcev v seznam v odvisnosti od vzorca
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
