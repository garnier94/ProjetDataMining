Prediction de la consommation electrique
========================================================
author: Camille Palmier & Arnaud Valladier
date: 16 mars 2017
autosize: true
font-family: 'Helvetica'

## Sommaire :

- Statistiques Descriptives
- Modeles de prediction 
- Conclusion et ouvertures



Nos donnees de consommation et tendance
========================================================


<img src="PalmierValladier-figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

Saisonnalites
========================================================

![plot of chunk unnamed-chunk-3](PalmierValladier-figure/unnamed-chunk-3-1.png)![plot of chunk unnamed-chunk-3](PalmierValladier-figure/unnamed-chunk-3-2.png)

Annees
========================================================
<img src="PalmierValladier-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />

Consommation par mois
========================================================
![plot of chunk unnamed-chunk-5](PalmierValladier-figure/unnamed-chunk-5-1.png)![plot of chunk unnamed-chunk-5](PalmierValladier-figure/unnamed-chunk-5-2.png)

Consommation par heure 
========================================================
<img src="PalmierValladier-figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />

Consommation selon le type de jour
========================================================
![plot of chunk unnamed-chunk-7](PalmierValladier-figure/unnamed-chunk-7-1.png)![plot of chunk unnamed-chunk-7](PalmierValladier-figure/unnamed-chunk-7-2.png)

Consommation et temperatures
========================================================
<img src="PalmierValladier-figure/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />


Modele de prediction - choix des stations
========================================================







```r
for(i in c(1:NStation)){
  for(j in c(1:i)){
    eq                   <- as.formula(paste("Zone2~Time+",Station[j,i],sep=""))
    reg.station          <- gam(eq, data=data0a)
    reg.forecast         <- predict(reg.station, data0b)
    R.squ[i,j]           <- summary(reg.station)$r.sq
    fit.err[i,j]         <- rmse(data0a$Zone2 - reg.station$fitted)
    forecast.err[i,j]    <- rmse(data0b$Zone2 - reg.forecast)
    fit.map[i,j]         <- mape(data0a$Zone2, reg.station$fitted )
    forecast.map[i,j]    <- mape(data0b$Zone2, reg.forecast)
  }
}
```

Modele de prediction - choix des stations
========================================================

<img src="PalmierValladier-figure/unnamed-chunk-13-1.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />


Modele de prediction - gam
========================================================


```r
eq <- as.formula(paste("Zone2~Time + s(Hour, by=daytype) + s(Toy,bs='cc')+",formule.gam.double, sep=""))
reg       <- gam(eq, data=data0a)
predict   <- predict(reg, data0b)

R              <- summary(reg)$r.sq                  #0.82  
fit.map        <- mape(data0a$Zone2, reg$fitted )    #6.94
forecast.map   <- mape(data0b$Zone2, predict)        #7.35  
```

Modele de prediction - gam
========================================================

![plot of chunk unnamed-chunk-16](PalmierValladier-figure/unnamed-chunk-16-1.png)![plot of chunk unnamed-chunk-16](PalmierValladier-figure/unnamed-chunk-16-2.png)

Residus du modele gam
========================================================
![plot of chunk unnamed-chunk-17](PalmierValladier-figure/unnamed-chunk-17-1.png)![plot of chunk unnamed-chunk-17](PalmierValladier-figure/unnamed-chunk-17-2.png)

Etude des Residus - Modele SARIMA
========================================================

 

```r
for(i in 1:24){
  model.h[[i]] <- arima(res.h[,i], order = c(1,2,2), seasonal = list(order=c(4,0,4), period=7), method='CSS')}
```
Etude des Residus - Modele SARIMA
========================================================

```
$pred
Time Series:
Start = 8396 
End = 8402 
Frequency = 1 
[1]  -5507.177 -10513.466  -5524.448 -11771.784 -12633.611 -15976.618
[7]  -2396.663

$se
Time Series:
Start = 8396 
End = 8402 
Frequency = 1 
[1] 11434.84 14537.14 17555.25 19980.01 22187.62 24180.42 26026.53
```

Previsions sur l'annee 2008
========================================================

```
Zone2 ~ Time + s(Hour, by = daytype) + s(Toy, bs = "cc") + s(Station5, 
    k = 10, bs = "cr") + s(Station11, k = 10, bs = "cr")
```


<img src="PalmierValladier-figure/unnamed-chunk-22-1.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />

Previsions sur l'annee 2008
========================================================
<img src="PalmierValladier-figure/unnamed-chunk-23-1.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />

Conclusion et ouvertures
========================================================

- Script de prevision recursif
- Valeurs manquantes
- Methode de bootstrap pour quantifier l'erreur (intervalle de confiance)
- Integrer des variables explicatives (luminosite, l'humidite)





