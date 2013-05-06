
if(FALSE){
RDDdat_choice <- RDDdata(y=choice_pg, x=xx, cutpoint=30)
RDDdat_risk <- RDDdata(y=risky_option, x=xx, cutpoint=30)

RDbw_choice <- RDDbw_IK(RDDdat_choice)
RDbw_risk <- RDDbw_IK(RDDdat_risk)

rd_choice <- RDDreg_np(RDDdat_choice)
rd_choice_sam <- RDDreg_np(RDDdat_choice, slope="same")
rd_choice_lm <- RDDreg_lm(RDDdat_choice)

rd_risk <- RDDreg_np(RDDdat_risk)
rd_risk_sam <- RDDreg_np(RDDdat_risk, slope="same")


rd_choice
rd_choice_sam
rd_choice_lm
rdd_choice <- RDestimate(choice_pg~xx, cutpoint=30, bw=RDbw_choice, model=TRUE)
coef(summary(rdd_choice$model[[1]]))
coef(summary(rd_choice))
plot(rd_choice)

RDest_t0_RDDreg <- function(RDest){

  cutpoint <- getCall(RDest)$cutpoint
  co_RDest <- coef(summary(RDest$model[[1]]))[,1]

  coefs <- vector("numeric", 4)
  names(coefs) <- c("(Intercept)", "x", "D", "x_right")
  coefs["x"] <- co_RDest["Xl"]
  coefs["x_right"] <- co_RDest["Xr"]-co_RDest["Xl"]
  coefs["(Intercept)"] <- co_RDest["(Intercept)"] - cutpoint*co_RDest["Xl"]
  coefs["D"] <- co_RDest["Tr"] + cutpoint*(co_RDest["Xl"]-co_RDest["Xr"])
coefs
}

RDest_t0_RDDreg(RDest=rdd_choice)
coef(summary(rd_choice))[,1]

rd_risk
rd_risk_sam
RDestimate(risky_option~xx, cutpoint=30, bw=RDbw_risk)


head(model.frame(RDestimate(risky_option~xx, cutpoint=30, model=TRUE, bw=RDbw_risk)$model[[1]]))

head(model.frame(rd_choice_sam$model))

}