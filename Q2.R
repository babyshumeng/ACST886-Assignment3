x <- c()
x[30:40] <- 30:40
x
qx.d.ind <- c()
qx.d.ind[30] <- 0.00043
qx.d.ind[31] <- 0.00042
qx.d.ind[32] <- 0.00041
qx.d.ind[33] <- 0.00040
qx.d.ind[34] <- 0.00041
qx.d.ind[35] <- 0.00042
qx.d.ind[36] <- 0.00043
qx.d.ind[37] <- 0.00045
qx.d.ind[38] <- 0.00049
qx.d.ind[39] <- 0.00053
qx.d.ind
qx.w.ind <- c()
qx.w.ind[30] <- 0.25
qx.w.ind[31] <- 0.20
qx.w.ind[32] <- 0.175
qx.w.ind[33] <- 0.15
qx.w.ind[34] <- 0.15
qx.w.ind[35] <- 0.15
qx.w.ind[36] <- 0.15
qx.w.ind[37] <- 0.15
qx.w.ind[38] <- 0.15
qx.w.ind[39] <- 1
qx.w.ind
qx.d.dep <- c()
qx.d.dep[30:39] <- qx.d.ind[30:39]*(1-0.5*qx.w.ind[30:39])
qx.d.dep
qx.w.dep <- c()
qx.w.dep[30:39] <- qx.w.ind[30:39]*(1-0.5*qx.d.ind[30:39])
qx.w.dep
al.x <- c()
al.x[30] <- 100000
ad.x.d <- c()
ad.x.w <- c()
for (j in 30:40) {
  ad.x.d[j] <- al.x[j]*qx.d.dep[j]
  ad.x.w[j] <- al.x[j]*qx.w.dep[j]
  al.x[j+1] <- al.x[j]-ad.x.d[j]-ad.x.w[j]
}

ad.x.d[30:40]
ad.x.w[30:40]
al.x[30:40]
i<-0.1 ###unit-fund earning rate
a<-c()
a[30]<-0.55 ###1-premium bases fee yr1
a[31:39]<-c(rep(0.99,9))### 1-premium bases fee yr1+
b<-0.005 ###buy/sell margin
m<-0.003 ###asset fee
Premium<-c()
Premium[30:39]<-c(rep(5000,10))
Invested_Premium<-Premium*a*(1-b)
Value_Unit<-c()
Value_Unit[30]<-Invested_Premium[30]*(1+i)*(1-m)
for(j in 31:39){
  Value_Unit[j]<-(Value_Unit[j-1]+Invested_Premium[j])*(1+i)*(1-m)}
table<-cbind(x[30:39],Invested_Premium[30:39],Value_Unit[30:39])
colnames(table)<-c("x","Invested premium(BOY)","Value of units(EOY)")
table
ExpectedI_P<-c()
t<-30:39
ExpectedI_P[t]<-Invested_Premium[t]*al.x[t]/al.x[30]
ExpectedP_V<-c()
ExpectedP_V[t]<-Value_Unit[t]*al.x[t+1]/al.x[30]
i<-0.1###unit-fund earning rate
interest<-c()
interest[30]<-ExpectedI_P[30]*i
j<-31:39
interest[j]<-i*(ExpectedP_V[j-1]+ExpectedI_P[j])
transfer<-c()
transfer[30]<-ExpectedI_P[30]+interest[30]-ExpectedP_V[30]
i<-31:39
transfer[i]<-ExpectedP_V[i-1]+ExpectedI_P[i]+interest[i]-ExpectedP_V[i]
table<-cbind(x[30:39],ExpectedI_P[30:39],interest[30:39],ExpectedP_V[30:39],transfer[30:39])
colnames(table)<-c("x","Invested premium(BOY)","Interest(EOY)","Policy Value(EOY)","Transfer(EOY)")
table
Premiumbased_Fees<-Premium*(1-a)
BUYSELL_margin<-Premium*a*b
expenses<-c()
expenses[30]<-5000*0.45+58
j<-31:39
expenses[j]<- 58*(1+0.2)^(j-30)
DeathSurr_BFT<-Value_Unit
table<-cbind(x[30:39],Premiumbased_Fees[30:39],BUYSELL_margin[30:39],expenses[30:39],DeathSurr_BFT[30:39])
colnames(table)<-c("x","Premium-based Fees(BOY)","BUY-SELL margin(BOY)","Expenses(BOY)","Death-Surr_BFT(EOY)")
table
i<-30:39
Exp_Premiumfee<-c()
Exp_Premiumfee[i]<-Premiumbased_Fees[i]*al.x[i]/al.x[30]
Exp_margin<-c()
Exp_margin[i]<-BUYSELL_margin[i]*al.x[i]/al.x[30]
Exp_expenses<-c()
Exp_expenses[i]<-expenses[i]*al.x[i]/al.x[30]
Exp_interest<-c()
Exp_interest<-0.08*(Exp_Premiumfee+Exp_margin-Exp_expenses)
Exp_DeathBFT<-c()
Exp_DeathBFT[i]<-DeathSurr_BFT[i]*ad.x.d[i]/al.x[30]
Exp_surrenderbenefit<-c()
Exp_surrenderbenefit[i]<-DeathSurr_BFT[i]*ad.x.w[i]/al.x[30]
Exp_transfer<-c()
Exp_transfer<-Exp_Premiumfee+Exp_margin+Exp_interest-Exp_expenses-Exp_DeathBFT-Exp_surrenderbenefit+transfer
table<-cbind(x[30:39],Exp_Premiumfee[30:39],Exp_margin[30:39],Exp_expenses[30:39],Exp_interest[30:39],Exp_DeathBFT[30:39],Exp_surrenderbenefit[30:39],Exp_transfer[30:39])
colnames(table)<-c("x","Exp_Premiumfee","Exp_margin","Eexpenses","Exp_interest","Exp_DeathBFT","Exp_surrenderbenefit","Exp_transfer")
table
PVofTransat12.5<-sum(Exp_transfer[30:39]*1.125^(-c(1:10)))
PVofTransat12.5
Profitability<-PVofTransat12.5/Premium[30]
Profitability
policy_value<-c()
policy_value<-Value_Unit
policy_value
f<-function(Premium)(abs(Exp_Premiumfee[39]+Exp_margin[39]-Exp_expenses[39]+Exp_interest[39]-Exp_DeathBFT[39]-Exp_surrenderbenefit[39]+(ExpectedI_P[39]+(ExpectedI_P[39]+Premium*al.x[39]/al.x[30])*i+Premium*al.x[39]/al.x[30])))
nlm(f,Value_Unit[39])