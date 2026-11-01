# Las dos K-tablas de meau armadas directamente con ade4 (Thioulouse, 2011)
ktabs_meau_ade4 <- function() {
  e <- new.env()
  utils::data("meau", package = "ade4", envir = e)
  m <- e$meau
  w1 <- ade4::withinpca(m$env, m$design$season, scannf = FALSE, scaling = "total")
  p2 <- ade4::dudi.pca(m$spe, scale = FALSE, scannf = FALSE, nf = 2)
  w2 <- ade4::wca(p2, m$design$season, scannf = FALSE, nf = 2)
  sitios <- rep(paste0("S", 1:6), 4)
  list(X = ade4::ktab.within(w1, colnames = sitios), Y = ade4::ktab.within(w2, colnames = sitios))
}
