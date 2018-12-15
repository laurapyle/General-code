# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
# do proinsulin ratios correlate with biochemical and ECL antibody levels
# corr_pro <- rcorr(as.matrix(allpro[, c( "Pro_pep","gadz","ia2z","mIAA","ZnT8","ICA",
#                                         "ECL_GADA","ECL_IA2A","ECL_IAA")])
#                   ,type = "spearman")
# corr_pro <- flattenCorrMatrix(corr_pro$r, corr_pro$P)
# corr_pro <- corr_pro[corr_pro$row=="Pro_pep",]