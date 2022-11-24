#Druga zadaća, Josipa Radnić, 1191240361

#Preuzela sam podatke u periodu od 1.1.2019 do 1.1.2020. na dnevnoj bazi za indeks Dow Jones i tvrtke IBM i Apple.
rm(list=ls())
setwd("C:/Users/Josipa/Pictures/Faks/DIPLOMSKI/EKON/2.zadaća")


#Ucitavamo podatke:
APPL=read.csv("AAPL.csv") 
INTL=read.csv("INTC.csv")
appl=APPL$Close
intl=INTL$Close
n=length(intl)


#Graficki prikaz vremenskih nizova:
plot(intl,type="l",col="green",ylab="vrijednosti",ylim=c(30,80))
lines(appl,col="red")
legend("topleft", legend=c("Intel","Apple"),col=c('green','red'),lty=c(1,1))
#Na temelju grafa mozemo primjetiti da postoji trend kod obe tvrtke, pa pretpostavljamo da nisu stacionarni.
#Takoder, prema zajednickom kretanju mozemo pretpostaviti da kointegriraju.


#Provjeravamo da su oba vremenska niza intrinsicno stacionarna reda 1, a to radimo prosirenim Dickey-Fuller testom, a odluku o broju ''lagova'' donosimo na temelju Akaike kriterija. Koristimo model s driftom.
library("urca")
dfappl=ur.df(appl,type="drift",selectlags="AIC")
summary(dfappl)
#Dobili smo vrijednost testne statistike 0.3019, sto je vece od svake kriticne vrijednosti na svim razinama 1%,5% i 10%. 
#Dakle, ne odbacujemo hipotezu H0, tj. hipotezu da vremenski niz appl nije stacionaran na tim razinama znacajnosti.
#Postupak ponavljamo za niz diferencija.
dappl=diff(appl)
df.dappl=ur.df(dappl,type="none",selectlags="AIC")
summary(df.dappl)
#Dobili smo vrijednost testne statistike -11.7703 sto je manje od svake kriticne vrijednosti na svim razinama 1,5% i 10%.
#Dakle, odbacujemo hipotezu H0 na tim razinama znacajnosti, tj. vremenski niz za Apple je intrinsicno stacionaran reda 1.

dfintl=ur.df(intl,type="drift",selectlags="AIC")
summary(dfintl)
#Dobili smo vrijednost testne statistike -1.4552, sto je vece od svake kriticne vrijednosti na svim razinama 1%,5% i 10%. 
#Dakle, ne odbacujemo hipotezu H0 koja govori da vremenski niz intl nije stacionaran na tim razinama znacajnosti.
#Postupak ponavljamo za niz diferencija.
dintl=diff(intl)
df.dintl=ur.df(dintl,type="none",selectlags="AIC")
summary(df.dintl)
#Dobili smo vrijednost testne statistike -10.3747 sto je manje od svake kriticne vrijednosti na svim razinama 1,5% i 10%.
#Dakle, odbacujemo hipotezu H0 na tim razinama znacajnosti, tj. vremenski niz za Intel je intrinsicno stacionaran reda 1.

#Dakle, oba vremenska niza (Apple i Intel) su intrinsicno stacionarna reda 1, pa ispunjavaju uvjet. Sada mozemo testirati kointegraciju ta dva vremenska niza.


#Procjena parametara pripadnog linearnog modela metodom najmanjih kvadrata
model=lm(intl~appl)
summary(model)
e=model$residuals
df.e=ur.df(e,type="none",selectlags="AIC")
summary(df.e)
#Najbolji model je prosireni s jednim ''lagom''. Dobili smo vrijednost testne statistike -1.9487, sto je manje od kriticne vrijednosti na razini znacajnosti 10%.
#Dakle, odbacujemo hipotezu H0 na razini znacajnosti 10%, tj. na toj razini mozemo zakljuciti da su reziduali stacionarni. 
#Kako su reziduali stacionarni, mozemo reci da su vremenski nizovi za Apple i Intel kointegrirajuci.

#Procjena parametara pripadnog modela korekcije gresaka:
ne=length(e)
ecm=lm(dintl~dappl+e[1:(ne-1)])
summary(ecm)
#Dobili smo da je slobodni clan(gama) jednak -0.02773, koeficijent uz dappl(delta) 0.58040 i koeficijent uz greske je -0.02899, tj lambda je 0.02899.
lambda=-ecm$coefficients[3]
lambda=unname(lambda)
lambda # lambda=0.02898829


#Povrati:
rappl=appl[2:n]/appl[1:(n-1)]-1
rintl=intl[2:n]/intl[1:(n-1)]-1
plot(rappl,type="l",col="green",xlab="vrijeme")
lines(rintl,col="red")
legend("topleft", legend=c("Apple","Intel"),col=c('green','red'),lty=c(1,1))
#Iz grafa mozemo primjetiti da postoje oscilacije u oba vremenska niza, no kod Intela su malo vece. Kako pokazuju razlicite trendove u razlicitim vremenskim trenutcima upucuje nas na heteroskedasticnost, ali ne mozemo nista konkretno zakljuciti.
#No, iako postoje oscilacije, mozemo iz grafa primjetiti da postoji nekakva stacionarnost.

#Pripadni reziduali za Apple:
mappl=mean(rappl)
eappl=rappl-mappl


#Procjena GARCH(1,1) parametara za reziduale:
library("tseries")
modelg=garch(eappl,order=c(1,1),trace=FALSE)
summary(modelg)
A0=modelg$coef[1]
A1=modelg$coef[2]
B1=modelg$coef[3]
A0 #0.0002060649
A1 #0.08575032
B1 #6.984794e-15

#Vremenski niz procijenjenih devijacija:
sigest=numeric(n)
sigest[1]=sqrt(A0/(1-A1-B1))
for(i in 2:n)
{
  sigest[i]=sqrt(A0+A1*(eappl[i-1])^2+B1*(sigest[i-1])^2)
}

plot(rappl,type="l",col="green",xlab="Vrijeme")
lines(sigest,col="red")
legend("topleft", legend=c("Povrati","Devijacije"),col=c('green','red'),lty=c(1,1))
#Mozemo primjetiti povremene vece devijacije u nekim vremenskim periodima. 

#Procjena vremenskog niza pripadnih 1%-tnih VaR:
alpha=0.01
z=qnorm(alpha)
z #kvantil: -2.326348
VaR=mappl+sigest*z
plot(rappl,type="l",col="green",xlab="Vrijeme")
lines(VaR[1:(n-1)],col="red")
legend("topleft", legend=c("Povrati","1% VaR"),col=c('green','red'),lty=c(1,1))
#Stvarni povrati u velikoj vecini slucajeva se nalaze iznad procjenjenog VaRa, tj. gubitak zaista nije bio gori od predvidenog.

#Koliko puta se stvarni povrat nasao ispod naseg procijenjenog 1%-tnog VaR-a:
sum(rappl<VaR[1:(n-1)])
#Stvarni povrat se 5 puta nasao ispod procijenjenog 1% VaR-a, dakle 5 puta smo ''krivo'' procjenili.
#Buduci da 1% od duljine uzorka 252 iznosi manje od 3, ocekujemo cca 2 slucajeva u kojima gubici su nadmasili VaR, rezultat 5 nije unutar nasih ocekivanja, pa je to malo losije nego sto bi ocekivali.

