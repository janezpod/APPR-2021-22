library(aws.s3)

PrenosCB <- function(OdLeto = 2010, DoLeto) {
  # Sprejeme leto v formatu YYYY in je vkljucno od in do vnesenega leta
  bucket <- get_bucket('capitalbikeshare-data')
  bucket <- bucket[-length(bucket)]
  seznam <- vector('character', length(bucket))
  if (missing(DoLeto)) {
    DoLeto <- OdLeto
  }
  for (i in bucket) {
    leto <- i$Key %>% substr(1,4) %>% strtoi()
    if (leto >= OdLeto & leto <= DoLeto) {
      temp <- tempfile()
      save_object(i$Key, file = temp, bucket = "capitalbikeshare-data")
      unzip(temp, exdir = './podatki', junkpaths = TRUE )
      unlink(temp)
    }
  }
}

