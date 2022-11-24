#Prva zadaća, Josipa Radnić, 1191240361

#Preuzela sam podatke u periodu od 1.4.2015 do 1.4.2020. na mjesečnoj bazi za indeks Dow Jones i tvrtke The Coca-Cola Company, McDonald's.

setwd("C:/Users/Josipa/Pictures/Faks/DIPLOMSKI/EKON/1.zadaća")

#Učitavamo podatke:
MCD=read.csv("MCD.csv")
KO=read.csv("KO.csv") 
DJI=read.csv("DJI.csv")

dji=DJI$Close #podaci za Dow Jones International
mcd=MCD$Close #podaci za McDonald's
ko=KO$Close #podaci za The Coca-Cola Company
#dji=as.numeric(sub(",", "", dji, fixed = TRUE))

#Računamo povrate:
n=length(dji) # broj podataka je 60 = 5 godina * 12 mjeseci
n
r_dji=(dji[2:n]-dji[1:(n-1)])/dji[1:(n-1)] #tržišni povrat=Rm
r_dji
r_mcd=(mcd[2:n]-mcd[1:(n-1)])/mcd[1:(n-1)] #povrat na investiciju za McDonald's
r_mcd
r_ko=(ko[2:n]-ko[1:(n-1)])/ko[1:(n-1)] #povrat na investiciju za The Coca-Cola kompaniju
r_ko


#Grafički prikaz povrata:
plot(r_dji,type="l", col="green",ylab="povrati")
lines(r_mcd,col="red")
lines(r_ko,col="blue")
#Iz grafa ne možemo ništa konkretno zaključiti niti pretpostaviti, jer za obe tvrtke postoje velike razlike u rastu i padu te ne možemo procjeniti koji bi mogao biti stabilniji, tj. sigurniji za investiciju. Možemo samo primjetiti da su uglavnom McDonald'su povrati veći nego kod The Coca-Cola.


#Računamo beta koji je zapravo koeficijent beta1 koji se dobije procjenom parametara modela RI,i=beta0+beta1*RM,i+ei :
modelmcd=lm(r_mcd~r_dji)
beta_mcd=modelmcd$coefficients[2]
beta_mcd
beta_mcd=unname(beta_mcd,force=FALSE)
beta_mcd #0.6567142 

modelko=lm(r_ko~r_dji)
beta_ko=modelko$coefficients[2]
beta_ko
beta_ko=unname(beta_ko,force=FALSE)
beta_ko #0.5818815
#Vidimo da je volatilnost kod McDonaldsa veća nego kod The Coca-Cola. Možemo pretpostaviti da je The Coca-Cola manje rizična nego McDonalds.


#Stvarni prosječni mjesečni povrat na imovinu:
mean(r_mcd) #0.01034515
mean(r_ko) #0.002403918
#Vidimo da je kod McDonaldsa veći.

#Izračunat ćemo povrat na imovinu prema CAPM modelu. Za nerizičnu stopu povrata uzet ćemo 2% na godišnjoj razini i podijeliti s 12 jer podatke imamo u mjesečnim povratima.
r_f=0.02/12
pros_povr_mcd=r_f+beta_mcd*(mean(r_dji)-r_f)
pros_povr_mcd #0.00344095
pros_povr_ko=r_f+beta_ko*(mean(r_ko)-r_f)
pros_povr_ko #0.00209566
#Vidimo da je malo veći kod McDonaldsa, te da je kod Coca-Cole blizu stvarnom prosječnom povratu, dok kod McDonaldsa je manji od stvarnog prosječnog povrata.
#Dakle, CAPM model nam je dao da ćemo kod obe kompanije dobiti manje nego što jesmo. Razlike su:
mean(r_mcd)-pros_povr_mcd #0.0069042
mean(r_ko)-pros_povr_ko #0.0003082585


#Sharpeov omjer:
s_mcd=mean(r_mcd-r_f)/sqrt(var(r_mcd-r_f))
s_mcd #0.1776675
s_ko=mean(r_ko-r_f)/sqrt(var(r_ko-r_f))
s_ko #0.01733377
#Iako to nismo očekivali s obzirom na beta, investicija za McDonalds je isplativija jer je Sharpeov omjer veći, no za oba su i dalje dosta mala.


#Treynorov omjer:
t_mcd=mean(r_mcd-r_f)/beta_mcd
t_mcd #0.01321501
t_ko=mean(r_ko-r_f)/beta_ko
t_ko #0.001267013
#Treynorov omjer je usklađen sa Sharpeovim omjerom, ispituje koliko su povrati investicije
#u odnosu na njenu rizičnost ali preko beta, i vidimo dakle da je investicija za McDonalds isplativija.


#Jensenov alfa:
alfa_mcd=mean(r_mcd)-r_f-beta_mcd*(mean(r_dji)-r_f)
alfa_mcd #0.0069042
alfa_ko=mean(r_ko)-r_f-beta_ko*(mean(r_dji)-r_f)
alfa_ko #-0.0008348519
#Jensenov alfa mjeri koliko povrata smo ostvarili iznad ili ispod očekivanja.
#Također potvrđujemo da je investicija u McDonalds isplativija, pogotovo jer je Jensenov alfa kod Coca-Cole negativan.



#Prikaz u tablici:
tablica=matrix(c(beta_mcd,beta_ko,s_mcd,s_ko,t_mcd,t_ko,alfa_mcd,alfa_ko),nrow=4,byrow=T)
tablica
colnames(tablica)=c("McDonald's","The Coca-Cola")
rownames(tablica)=c("beta","Sharper","Treynor","Jensen")
tablica
#Beta nam govori o osjetljivosti naše investicije na promjene na tržištu. 
#Vidimo da je on veći kod McDonald'sa, dakle možemo pretpostaviti da je rizičniji od Coca-Cole iako kasnije u povratima nije pokazano.
#Sharper omjer je mjera uspješnosti ulaganja, a on je veći kod McDonaldsa pa bi se po tome moglo pretpostaviti da je bolje ulagati u McDonalds nego u Coca-Colu.
#Analogno i za Traynerov omjer koji ispituje koliki su povrati investicije u odnosu na njenu rizičnost, te za Jensenov alfa koji je čak i negativan u slučaju Coca-Cole.


#Računamo log-povrate:
l_mcd=log(mcd[2:n]/mcd[1:(n-1)])
l_mcd
l_ko=log(ko[2:n]/ko[1:(n-1)])
l_ko
l_dji=log(dji[2:n]/dji[1:(n-1)])
l_dji
  

plot(l_dji,type="l",col="green",ylab="log-povrati")
lines(l_mcd,col="red")
lines(l_ko,col="blue")
#Možemo primjetiti da je graf sličan kao i kod ne-logaritmiranih podataka.


#Testiranje normalnosti podataka:

hist(l_mcd,probability=T, breaks=50, col = "green", ylim=c(0,25))
#Kod tvrtke McDonald's malo teže možemo primjetiti da ima zvonolik oblik, ali odskače nekoliko stupića što nam otežava da pretpostavimo da je zvonolikog oblika.
hist(l_ko,probability=T, breaks=50, col = "green", ylim=c(0,25))
#Kod tvrtke The Coca-Cola možemo lijepo primjetiti da je zvonolikog oblika.
#Dakle, lošiji je histogram kod McDonald'sa. No, kako bi mogli doći do nekog zaključka testirati ćemo normalnost podataka.

#Prvo ćemo testirati normalnim vjerojatnosnim grafom.

qqnorm(l_mcd,xlab="Teorijski kvantili", ylab="Uzorački kvantili",ylim=c(-0.2,0.2),main="McDonald's")
abline(mean(l_mcd),sd(l_mcd),col="blue",lwd=2)
#Dobiveni graf za McDonald's izgleda kao pravac, ali dobro se grupira oko pravca, mali odmaci od pravca na početku i kraju.
qqnorm(l_ko,xlab="Teorijski kvantili", ylab="Uzorački kvantili",ylim=c(-0.2,0.1),main="The Coca-Cola")
abline(mean(l_ko),sd(l_ko),col="blue",lwd=2)
#Dobiveni graf za The Coca-Cola malo se lošije grupira oko pravca, te također odskače najviše na početku i kraju. 
#Iz ovih grafova ne možemo naslutiti da su podaci normalno distribuirani jer dosta točaka ne leži na pravcu, te smo malo sigurniji kod Coca-Cola da bi mogli odbaciti normalnu distribuiranost, no to su samo slutnje.
#Provest ćemo testove kojima ćemo na nekoj razini značajnosti moći zaključiti jesu li podaci normalno distribuirani ili nisu.
#Dakle, nul hipoteza svih idućih testova je da su podaci normalno distribuirani. 

#Provodimo KS test:

ks.test(l_mcd,"pnorm",mean(l_mcd),sd(l_mcd))
#p-vrijednost=0.6332 pa na svakoj standardnoj razini značajnosti ne odbacujemo hipotezu H0, tj prihvaćamo hipotezu da su podaci normalno distribuirani za tvrtku McDonald's.
ks.test(l_ko,"pnorm",mean(l_ko),sd(l_ko))
#p-vrijednost=0.09371 pa na razini značajnosti manjoj od 9,3% ne odbacujemo hipotezu H0, tj prihvaćamo hipotezu da su podaci za tvrtku The Coca-Cola normalno distribuirani.
#Dakle, kada bi uzeli razinu značajnosti 5% (0.05), p-vrijednost kod svih podataka je veća od 0.05, te bi na razini značajnosti 5% mogli zaključiti da ne odbacujemo hipotezu H0 da su podaci za tvrtke normalno distribuirani.


#Provodimo Lillie inačicu KS testa:
library("nortest")
lillie.test(l_mcd)
#p-vrijednost=0.2117 pa na svakoj standardnoj razini značajnosti ne odbacujemo hipotezu H0, tj prihvaćamo hipotezu da su podaci za tvrtku McDonald0s normalno distriburani
lillie.test(l_ko)
#p-vrijednost=0.0008482 što je vrlo malo pa na svakoj standardnoj razini značajnosti(1%,5% i 10%) odbacujemo hipotezu da su podaci za tvrtku The Coca-Cola normalno distribuirani.
#Dakle, s Lillie testom možemo zaključiti da su na svakoj standardnoj razini značajnosti podaci za The Coca-Cola nisu normalno distribuirani, dok za McDonald's jesu.


#Provodimo Shapiro test:
shapiro.test(l_mcd) #p-vrijednost=0.06412
shapiro.test(l_ko) #p-vrijednost=4.912e-05
#Dakle, sa Shapiro testom na svakoj standardnoj razini značajnosti za tvrtku The Coca-Cola odbacujemo hipotezu da su podaci normalno distribuirani, dok za McDonald's na razini značajnosti 5% ne odbacujemo hipotezu da su podaci normalno distribuirani.


#Provodimo Jarque Bera test:
#install.packages("tseries")
library("tseries")
jarque.bera.test(l_mcd) #p-vrijednost=0.003293
jarque.bera.test(l_ko) #p-vrijednost=< 2.2e-16
#Dakle, sa Jarque Bera testom na svakoj standardnoj razini značajnosti tvrtke odbacujemo hipotezu da su podaci normalno distribuirani.

#S obzirom da nam je KS test na razini značajnosti 5% dao da su svi podaci normalno distribuirani, onda možemo pretpostaviti da su podaci normalno distribuirani i provesti iduće testove.


#Nulta hipoteza je je li varijanca log povrata Mcdonaldsa i varijanca log povrata The Coca-Cola su jednaki. Alternativna hipoteza je da nisu.
var.test(l_mcd,l_ko)
#p-vrijednost=0.3926 što je veće od bilo koje standardne razine značajnosti, pa ne odbacujemo hipotezu H0, tj prihvaćamo hipotezu da je varijanca log povrata od The Coca-Cola i varijanca log povrata od McDonald's jednaki na bilo kojoj standardnoj razini značajnosti.


#Također, s obzirom na KS test, pretpostavljamo da su podaci normalno distribuirani te možemo provesti idući test.
#Nulta hipoteza je je li očekivanje log povrata Mcdonaldsa i očekivanje log povrata The Coca-Cola su jednaki. Alternativna hipoteza je da nisu.
t.test(l_mcd,l_ko)
#p-vrijednost=0.3748 što je veće od bilo koje standardne razine značajnosti, pa ne odbacujemo hipotezu H0, tj prihvaćamo hipotezu da je očekivanje log povrata od The Coca-Cola i očekivanje log povrata od McDonald's jednaki na bilo kojoj standardnoj razini značajnosti.




#U slučaju kada prihvatimo ostale testove koji pokazuju na svim standardnim razinama značajnosti da podaci nisu normalno distribuirani, imamo alternativne testove za testiranje je li očekivanje podataka jednako.
#Zato ćemo uzeti Wilcoxonov test za testiranje.
#help(wilcox.test)
wilcox.test(l_mcd,l_ko)
#p=0.4113 što je veće od bilo koje standardne razine značajnosti, te potvrđuje t.test koji isto nije odbacio hipotezu H0, tj prihvaćamo hipotezu da je očekivanje log povrata od The Coca-Cola i očekivanje log povrata od Mcdonalds jednaki na bilo kojoj standardnoj razini značajnosti.