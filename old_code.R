## CT ATE CHF
```{r,echo=F}
##H&I estimator
chf_sub<-chf%>%filter(treatment<100,treatment>0)
chf_sub$y<-as.numeric(as.factor(chf_sub$mort30))
table(chf$y)
hi_list <- hi_est(Y = y,
                  treat = treatment,
                  treat_formula = treatment ~ pressor + sofa_24hours + DNI, #+ temperature + heart_rate +wbc + creatinine +
                  #lactate_cat + bicarbonate + hemoglobin + sodium + potassium + glucose, 
                  outcome_formula = y ~ treatment + I(treatment^2) + gps + I(gps^2) + treatment * gps,
                  data = chf_sub,
                  grid_val = seq(0, 100, by = 10),
                  #grid_val = c(seq(0, 50, by = 5),seq(50,100,by =10)),
                  treat_mod = "LogNormal")
sample_index <- sample(1:1000, 100)
plot(chf_sub$treatment[sample_index],
     chf_sub$y[sample_index],
     xlab = "treatment",
     ylab = "y",
     main = "hi estimate")
lines(seq(0, 100, by = 10),
      hi_list$param,
      lty = 2,
      lwd = 2,
      col = "blue")
legend('bottomright',
       "hi estimate",
       lty=2,
       lwd = 2,
       col = "blue",
       bty='y',
       cex=1)
```

```{r,echo=F}
##Spline estimator
chf_sub<-chf%>%filter(treatment<100,treatment>0)
chf_sub$y<-as.numeric(as.factor(chf_sub$mort30))

spl_list <- add_spl_est(Y = y,
                        treat = treatment,
                        treat_formula = treatment ~ pressor + sofa_24hours + DNI + temperature + heart_rate +wbc + creatinine,
                        #lactate_cat + bicarbonate + hemoglobin + sodium + potassium + glucose, 
                        #outcome_formula = y ~ treatment + I(treatment^2) + gps + I(gps^2) + treatment * gps,
                        data = chf_sub,
                        grid_val = seq(0, 100, by = 10),
                        knot_num = 3,
                        treat_mod = "LogNormal")
sample_index <- sample(1:1000, 100)
plot(chf_sub$treatment[sample_index],
     chf_sub$y[sample_index],
     xlab = "treatment",
     ylab = "y",
     main = "spl estimate")
lines(seq(0, 100, by = 10),
      spl_list$param,
      lty = 2,
      lwd = 2,
      col = "blue")
legend('bottomright',
       "spl estimate",
       lty=2,
       lwd = 2,
       col = "blue",
       bty='y',
       cex=1)
```


## CT ATE ESRD
```{r,echo=F}
##H&I estimator
esrd_sub<-esrd%>%filter(treatment<100,treatment>0)
esrd_sub$y<-as.numeric(as.factor(esrd_sub$mort30))
table(esrd$y)
hi_list <- hi_est(Y = y,
                  treat = treatment,
                  treat_formula = treatment ~ pressor + sofa_24hours + DNI, #+ temperature + heart_rate +wbc + creatinine +
                  #lactate_cat + bicarbonate + hemoglobin + sodium + potassium + glucose, 
                  outcome_formula = y ~ treatment + I(treatment^2) + gps + I(gps^2) + treatment * gps,
                  data = esrd_sub,
                  grid_val = seq(0, 100, by = 10),
                  treat_mod = "LogNormal")
sample_index <- sample(1:1000, 100)
plot(esrd_sub$treatment[sample_index],
     esrd_sub$y[sample_index],
     xlab = "treatment",
     ylab = "y",
     main = "hi estimate")
lines(seq(0, 100, by = 10),
      hi_list$param,
      lty = 2,
      lwd = 2,
      col = "blue")
legend('bottomright',
       "hi estimate",
       lty=2,
       lwd = 2,
       col = "blue",
       bty='y',
       cex=1)
```

```{r,echo=F}
##Spline estimator
esrd_sub<-esrd%>%filter(treatment<100,treatment>0)
esrd_sub$y<-as.numeric(as.factor(esrd_sub$mort30))

spl_list <- add_spl_est(Y = y,
                        treat = treatment,
                        treat_formula = treatment ~ pressor + sofa_24hours + DNI + temperature + heart_rate +wbc + creatinine +
                          lactate_cat + bicarbonate + hemoglobin + sodium + glucose, 
                        #outcome_formula = y ~ treatment + I(treatment^2) + gps + I(gps^2) + treatment * gps,
                        data = esrd_sub,
                        grid_val = seq(0, 100, by = 10),
                        knot_num = 3,
                        treat_mod = "LogNormal")
sample_index <- sample(1:1000, 100)
plot(esrd_sub$treatment[sample_index],
     esrd_sub$y[sample_index],
     xlab = "treatment",
     ylab = "y",
     main = "spl estimate")
lines(seq(0, 100, by = 10),
      spl_list$param,
      lty = 2,
      lwd = 2,
      col = "blue")
legend('bottomright',
       "spl estimate",
       lty=2,
       lwd = 2,
       col = "blue",
       bty='y',
       cex=1)
```