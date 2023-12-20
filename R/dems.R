chm_from_coregistered_dsm_dtm = function(dsm, dtm) {
  chm = dsm - dtm
  chm[chm < 0] = 0
  return(chm)
}