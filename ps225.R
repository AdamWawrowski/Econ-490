setwd("C:/Users/adamw/Desktop/ECON471")
getwd()
df<-read.table("vote1.txt",na.string=".")

attach(df)

voteA=V4
prtystrA=V7
democA=V3
lexpendA=V8
lexpendB=V9

model=lm(voteA~prtystrA+democA+lexpendA+lexpendB)

residual=resid(model)

resiModel=lm(residual~prtystrA+democA+lexpendA+lexpendB)

summary(resiModel)

residualsq=residual*residual

resisqModel=lm(residualsq~prtystrA+democA+lexpendA+lexpendB)

summary(resisqModel)

bptest(model)

pf(2.33, 4, 168, lower.tail=FALSE)

white.test(model)

whiteModel=lm(residualsq~prtystrA+democA+lexpendA+lexpendB+I(prtystrA^2)
              +I(democA^2)+I(lexpendA^2)+I(lexpendB^2)+prtystrA*democA
              +prtystrA*lexpendA+prtystrA*lexpendB+democA*lexpendA
              +democA*lexpendB+lexpendA*lexpendB)

summary(whiteModel)

niceModel=lm(residualsq~model[["fitted.values"]]+I((model[["fitted.values"]])^2))
      
summary(niceModel)
