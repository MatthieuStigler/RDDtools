# 
# if(FALSE){
# # library(foreign)
# library(KernSmooth)
# 
# source("/home/mat/Dropbox/HEI/rdd/Rcode/Lee_2008/plotBin.R")
# 
# individ_final <-read.dta("/home/mat/Dropbox/HEI/rdd/Rcode/Lee_2008/individ_final.dta")
# head(individ_final )
# dim(individ_final )
# 
# 
# 
# 
# 
# 
# 
# ### Fig 2a
# fig2 <- individ_final[, c("mmyoutcomenext", "mpmyoutcomenext", "difshare")]
# fig2_nona <- fig2[apply(fig2,1, function(x) !any(is.na(x))),]
# loc1 <- locpoly(fig2_nona$difshare, fig2_nona$mmyoutcomenext, bandwidth=0.05, gridsize=100)
# 
# plot(loc1$x, loc1$y)
# 
# head(loc1$x)
# 
# 
# 
# 
# plotBin(x=fig2_nona$difshare, y=fig2_nona$mmyoutcomenext, h=0.005, xlim=c(-0.5, 0.5), cex=0.7)
# plotBin(x=fig2_nona$difshare, y=fig2_nona$mmyoutcomenext, h=0.02)
# plotBin(x=fig2_nona$difshare, y=fig2_nona$mmyoutcomenext, type="number")
# 
# plotBin(x=fig2_nona$difshare, y=fig2_nona$mmyoutcomenext, h=0.02, xlab="aha")
# 
# 
# plotBinres <- plotBin(x=fig2_nona$difshare, y=fig2_nona$mmyoutcomenext, h=0.05, cex=0.7)
# 
# 
# ## MANUAL KSMOOTH 
# h <- 0.05
# ks1<- ksmooth(fig2_nona$difshare, fig2_nona$mmyoutcomenext, kernel="box", bandwidth=h, x.points=plotBinres$x)
# points(plotBinres$x, ks1$y, col=2, cex=0.6)
# 
# ## super manual check:
# se <- seq(from=-1, to=1, by=0.05)
# h <- 0.05
# 
# ks1<- ksmooth(fig2_nona$difshare, fig2_nona$mmyoutcomenext, kernel="box", bandwidth=h, x.points=se)
# ks1$y[ks1$x== 0.5]
# mean(fig2_nona$mmyoutcomenext[fig2_nona$difshare>=0.475 & fig2_nona$difshare<=0.525])
# mean(fig2_nona$mmyoutcomenext[fig2_nona$difshare>0.475 & fig2_nona$difshare<0.525])
# 
# po <- 0.70
# ks1$y[which.min(abs(ks1$x- po))]
# 
# mean(fig2_nona$mmyoutcomenext[fig2_nona$difshare>=po-0.05/2 & fig2_nona$difshare<=po+0.05/2])
# mean(fig2_nona$mmyoutcomenext[fig2_nona$difshare>po-0.05/2 & fig2_nona$difshare<po+0.05/2])
# 
# 
# 
# 
# }