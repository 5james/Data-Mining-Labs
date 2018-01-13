#   Laboratorium 5

#   Autor: Jakub Guzek

#   Załadowanie bibliotek
library(party)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)

#   Zbiorem analizowanym w ramach tego laboratorium będzie zbiór win (białych).
#   http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineWhite_ds = read.table("wine_white.csv", header = TRUE, sep=";", na.strings= "*")

#   Każde wino charakteryzuje się następującymi atrybutami:
#   - fixed acidity -> stała kwasowość = kwasy, które są związane z winem (nie parują łatwo)
#   - volatile acidity - kwasowość lotna, związana często z kwasem octowym w winie,
#     którego zbyt duża ilość może prowadzić do smaku octu
#     http://waterhouse.ucdavis.edu/whats-in-wine/volatile-acidity
#   - citric acid - kwas cytrynowy
#   - residual sugar - resztkowa zawartość cukru, który pozostał po fermentacji;
#     wina z zawartością powyżej 45g/L uważa się za słodkie
#     https://en.wikipedia.org/wiki/Sweetness_of_wine
#   - chlorides - chlorki, a dokładniej zawartość chlorków (główne soli - NaCl)
#     w winie
#   - free sulfur dioxide (FSO2) - wolny dwutlenek siarki (odgrywa ważną rolę w
#     zachowaniu wina przed zmianami utleniającymi i niektórymi mikroorganizmami)
#     https://www.cdrfoodlab.com/foods-beverages-analysis/free-sulfur-dioxide-wine/
#   - total sulfur dioxide - całkowita zawartość dwutlenku siarki w winie, ma duży
#     wpływ na smak wina
#   - density - gęstość
#   - pH - pH
#   - sulphates - siarczany, w tym dwutlenki siarki; istnieją jako dodatek
#     do wina w celu regulacji poziomu dwutlenków siarki; podobnie jak wolny 
#     dwutlenek siarki ich celem jest zachowanie wina przed zmianami utleniającymi
#     i niektórymi mikroorganizmami
#   - alcohol - zawartość alkoholu w winie
#   Powyższe atrybuty zostały zmierzone dzięki badaniom chemiczno-fizycznym.
#
#   Każde przebadane wino zostało także poddane ocenie, którego wynikiem jest
#   atrybut 'quality'. W przeciwieństwie do poprzednich atrybutów 'quality' 
#   zostałoprzypisane dzięki danym sensorycznym - odczuciowym -> każde wino
#   zostało ocenione przez 3 niezależnych ekspertów i została przypisana mediana.
#   Skala ocen wina jest od 0 do 10. Jednakże, z obserwacji otrzymanych danych
#   nie występuje wino o ocenie gorszej niż 3, a także lepszej niż 9.

#   Następnym zadaniem jest wyznaczenie atrybutu klasy i wyliczenie jest ich 
#   wyróżnionych w zadaniu. Można byłoby spróbować stworzyć drzewo klasyfikacji
#   dla któregoś z atrybutów zmierzonych dzięki badaniom chemiczno-fizycznym.
#   Na przykład: zależność zawartości siarczanów od wolnych dwutlenków siarki
#   i całkowitej zawartości dwutlenku siarki lub innych własności 
#   chemiczno-fizycznym, ale te będą raczej dość przewidywalne z punktu
#   widzenia chemicznego. Byłyby też ograniczone co do atrybutów, które
#   by miały wpływ na atrybut nominalny. Bardziej interesujące są oceny ekspertów
#   na temat (białych) win. Z tego też powodu w tym skrypcie atrybutem nominalnym
#   klasy będzie 'quality'. Jest on jednak w miarę rozbieżny, ponieważ
#   przewidywane oceny mogą przyjmować wartości <0,10>.
#   10 poziomów to dosyć dużo i dla osoby, która po prostu chce kupić wino
#   wystarczą 3-5 poziomów ocen.

summary(wineWhite_ds$quality)

#   Problem z podziałem jest taki, że danych z quality=3 lub 9 jest bardzo
#   mało w porównaniu do całości win z quality=4, 8 lub 7 jest już więcej,
#   ale mimo tego przytłaczająca większość ocen skupia sią wokół 5 lub 6.
#   Nie znając dane, można byłoby podzielić oceny na: słabe <0,4),
#   przeciętne <4,8) i dobre <8,10>. Na szczęście znając zbiór wartości
#   kolumny 'quality' oraz jego liczność można podzielić inaczej.
#   Mając zbiór <3,9> skupiający się wokół 5,6 ciężko będzie podzielić na
#   więcej niż 3-4 grupy. Jedną z ważniejszych decyzji jest to, czy rozdzielić
#   5 i 6, czy też nie. Z punktu widzenia rozwiązywania zadania ciekawiej
#   byłoby, gdyby były osobno, lecz z drugiej strony przy takim podziale 
#   5 musiałaby sama stanowić jedną grupę. Decydując się na podział na 3 grupy
#   inną ważną kwestią jest czy quality=7 dodać do grupy środkowej 
#   (najliczniejszej), czy też może dodać do grupy lepszych win.
#   Na początku spróbuję podzielić atrybut quality na następujące grupy:
#   <3,5), <5,8), <8,9). 

wineWhite_ds[["qualityDiscretized"]] <- ordered(cut(wineWhite_ds[["quality"]],
                                          c(-Inf, 4, 7 ,Inf)),
                                      labels = c("bad", "average", "premium"))

#   Oceny jakości klasyfikatora (na pierwszy rzut oka) będę dokonywał
#   dzięki macierzy pomyłek przede wszystkim dla danych treningowych,
#   a następnie dla danych testowych. 
#   Kolejno jako dokładniejszejszego (liczbowego) wymiernika jakości 
#   klasyfikatora będę używał współczynnika precyzji (precision),
#   współczynnika odzysku (recall), które będą się składały na
#   f-miarę (f-measure). Dodatkowo będzie można użyć współczynnika
#   dokładności (accuracy) z funkcji confusionMatrix z pakiety caret.

#   Cel ogólny, czyli potencjalne zastosowanie zbudowanego w ramach
#   tego laboratorium drzewa klasyfikacji jest następujący: każda
#   winiarnia lub każda osoba, która hobbystycznie zajmuje się
#   robieniem / przyrządzaniem własnego wina lub też smakosz wina
#   z łatwością mogą się zaopatrzeć w elektroniczne mierniki lub 
#   inne urządzenia, które będą dawały wynik właściwości wina, dzięki
#   którym zostały opisane wina w naszym testowanym zbiorze.
#   Na przykład do badania pH można kupić specjalny miernik, a do
#   badania kwasowości także można kupić tzw. 'acid testing kit'.
#   Nie wszyscy jednak mają tak wyrafinowane kubki smakowe i 
#   nie wszyscy umieją określić 'znakomitość' wina. Jest to też bardzo
#   subiektywne odczucie. Dlatego jeśli winiarnia lub ktoś kto
#   hobbystycznie przyrządza wino chcieliby określić jak dobre wino
#   im wyszło nie muszą polegać wyłącznie na sobie. Mogą wtedy zbadać
#   przyrządzone wino za pomocą (mniej zawodnych niż niedoświadczone 
#   kubki smakowe) urządzeń / kompletu testóW pomiarowych, wprowadzić
#   do wytrzonego w ramach tego laboratorium drzewa klasyfikacji i 
#   określić, czy wyszło im słabe, przeciętne, czy wino premium.


#   Na początku trzeba podzelić dane (przetestowane białe wina) na
#   treningowe, dzięki którym będzie tworzone drzewo klasyfikacji
#   oraz testowe, dzięki którym te drzewa będą testowane.
#   Ważne przy tym podziale jest to, aby zachować jak najlepszy
#   współczynnik proporcji win o konkretnej jakości (quality) do
#   wszystkich win. Na szczęście współczynnik jakości został 
#   zdyskretyzowany i podział jest trochę prostszy.
ilosc_danych <- dim(wineWhite_ds)[1]
participation_bad <- length(wineWhite_ds[wineWhite_ds == "bad"]) / ilosc_danych
participation_average <- length(wineWhite_ds[wineWhite_ds == "average"]) / ilosc_danych
participation_premium <- length(wineWhite_ds[wineWhite_ds == "premium"]) / ilosc_danych
#   Pierwsza próba podziału podziału
set.seed(1235)
sam <- sample(2, nrow(wineWhite_ds), replace=TRUE, prob=c(0.8, 0.2))
#sam
trainData <- wineWhite_ds[sam==1,]
testData <- wineWhite_ds[sam==2,]
ilosc_danych_train <- dim(trainData)[1]
participation_bad_train <- length(trainData[trainData == "bad"]) / ilosc_danych_train
participation_average_train <- length(trainData[trainData == "average"]) / ilosc_danych_train
participation_premium_train <- length(trainData[trainData == "premium"]) / ilosc_danych_train

print(participation_bad_train - participation_bad)
print(participation_average_train - participation_average)
print(participation_premium_train - participation_premium)
remove(participation_bad_train, participation_average_train, participation_premium_train, 
       participation_bad, participation_average, participation_premium, sam, ilosc_danych,
       ilosc_danych_train)

#   Podział danych jest zadowalający - żadna klasa z qualityDiscretized
#   nie została zaniedbana.


######################################################################################################################
#   Eksperyment 1
#   Eksperyment pierwszy ma na celu stworzenie drzewa klasyfikacji za pomocą
#   funkcji rpart (Recursive Partitioning and Regression Trees).
#   Drzewo to będzie przewidywało jakość wina (qualityDiscretized) w zależności
#   od reszty atrybutów, a dokładniej mówiąc właściwościom chemiczno-fizycznym
#   wina, które można zmierzyć.
#   Pierwszy eksperyment nie będzie korzystał w ogóle ze zmiennych, dzięki
#   którym można kontrolować rpart. Chcę zobaczyć, jak będzie wyglądało takie
#   drzewo (zobaczyć czego mu brakuje), a także zestawić podstawowy wzorzec eksperymentu

#   Niestety nie można użyć skróconego zapisu "~ .", ponieważ istnieje
#   jeszcze kolumna quality, która jest niezdyskretyzowaną wartością jakości
#   wina i nie chcemy na jej podstawie budować drzewa. 
myFormula <- qualityDiscretized ~ fixed.acidity + volatile.acidity +
              citric.acid +  residual.sugar + chlorides +  
              free.sulfur.dioxide + total.sulfur.dioxide + 
              density + pH + sulphates + alcohol  

rpTree <- rpart(myFormula,  method="class", data=trainData)


#   klasyfikacja danych treningowych - macierz błędów
trainPred = predict(rpTree,trainData,type = "class")
train_confusion_table <- table(trainData$qualityDiscretized,trainPred)
train_confusion_table
#             bad   average   premium
#   bad       30    121       0
#   average   11    3605      0
#   premium    2    136       0
cm <- confusionMatrix(trainPred, trainData$qualityDiscretized)
cm

#   Sprawdzenie współczynników dla słabych win 
train_bad_true <- dim(trainData[trainData$qualityDiscretized == "bad",])[1]
train_bad_predict <- length(trainPred[trainPred == "bad"])

train_predict_true_bad <- c()
for (tmp in 1:dim(trainData)[1]) {
  if (trainData$qualityDiscretized[tmp] == "bad" && trainPred[tmp] == "bad") {
    train_predict_true_bad <- c(train_predict_true_bad, 1)
  } else {
    train_predict_true_bad  <- c(train_predict_true_bad, 0)
  }
}
train_bad_precyzja <- sum(train_predict_true_bad) / train_bad_predict
train_bad_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'bad' = 0.6976744
train_bad_odzysk <- sum(train_predict_true_bad) / train_bad_true
train_bad_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'bad' = 0.1986755
#   Do obliczenia F-Miary będę używał uproszczonego wzoru, w którym beta
#   jest równa 1, ponieważ równie bardzo zależy mi na prcyzji i odzysku:
#   2*(wsp.precyzji * wsp.odzysku)/(wsp.precyzji + wsp.odzysku).
train_bad_f_miara <- 2 * train_bad_precyzja * train_bad_odzysk / (train_bad_precyzja + train_bad_odzysk)
train_bad_f_miara
#   F-miara dla qualityDiscretized == 'bad' = 0.3092784

#   Podsumowując współczynniki i f-miarę dla słabych win, są one bardzo niezadowalające.
#   Można się tego spodziewać wiedząc, że słabych win jest w porównaniu do wszystkich
#   w danych treningowych niewielki procent, a dodakowo do rpart nie zostały dostarczone
#   żadne współczynniki kontrolne.

#   Sprawdzenie współczynników dla przeciętnych win
train_average_true <- dim(trainData[trainData$qualityDiscretized == "average",])[1]
train_average_predict <- length(trainPred[trainPred == "average"])

train_predict_true_average <- c()
for (tmp in 1:dim(trainData)[1]) {
  if (trainData$qualityDiscretized[tmp] == "average" && trainPred[tmp] == "average") {
    train_predict_true_average <- c(train_predict_true_average, 1)
  } else {
    train_predict_true_average  <- c(train_predict_true_average, 0)
  }
}
train_average_precyzja <- sum(train_predict_true_average) / train_average_predict
train_average_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'average' = 0.9334542
train_average_odzysk <- sum(train_predict_true_average) / train_average_true
train_average_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'average' = 0.996958
train_average_f_miara <- 2 * train_average_precyzja * train_average_odzysk / (train_average_precyzja + train_average_odzysk)
train_average_f_miara
#   F-miara dla qualityDiscretized == 'average' = 0.9641615

#   Podsumowując współczynniki i f-miarę dla przeciętnych win, są one całkiem zadowalające
#   F-miara to ponad 96%, lecz można było się tego spodziewać, skoro przeciętne wina
#   stanowią bardzo bardzo dużą część danych treningowych.


#   Sprawdzenie współczynników dla premium win
train_premium_true <- dim(trainData[trainData$qualityDiscretized == "premium",])[1]
train_premium_predict <- length(trainPred[trainPred == "premium"])

train_predict_true_premium <- c()
for (tmp in 1:dim(trainData)[1]) {
  if (trainData$qualityDiscretized[tmp] == "premium" && trainPred[tmp] == "premium") {
    train_predict_true_premium <- c(train_predict_true_premium, 1)
  } else {
    train_predict_true_premium  <- c(train_predict_true_premium, 0)
  }
}
train_premium_precyzja <- sum(train_predict_true_premium) / train_premium_predict
train_premium_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'premium' = NaN
train_premium_odzysk <- sum(train_predict_true_premium) / train_premium_true
train_premium_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'premium' = 0
train_premium_f_miara <- 2 * train_premium_precyzja * train_premium_odzysk / (train_premium_precyzja + train_premium_odzysk)
train_premium_f_miara
#   F-miara dla qualityDiscretized == 'premium' = NaN

#   Podsumowując współczynniki i f-miarę dla win premium, nie ma ich.
#   Jest to spowodowane tym, że drzewo klasyfikacji nie przewidziało
#   wyjścia (liścia), w którym wynikiem będzie wino premium.
#   Może to być spowodowane bardzo małą ilością win typu premium i 
#   ustawieniami domyślnymi rpart.


#   Normalnie w tym miejscu powinno się także przeprowadzić analizę
#   dla danych testowych analogicznie do przeprowadzonej przed chwilą
#   dla danych treningowych. Nie ma to jednak sensu dla eksperymentu 1,
#   który jest jedynie rozpoczęciem całego badania, ponieważ skoro dla
#   danych dla których się uczył (treningowych) macierz wygląda tak źle
#   to dla danych, ktrych wcześniej nie widział także raczej nie powinno
#   wygenerowane w tym eksperymencie drzewo klasyfikacji działać.




#   graficzna prezentacja drzewa  - rpart
plot(rpTree, uniform=TRUE,     main="Classification for wines quality")
text(rpTree, use.n=TRUE, all=TRUE, cex=.7)
#?prp
boxcols <- c("pink", "yellow", "palegreen3")[rpTree$frame$yval]
prp(rpTree, faclen = 0, cex = 0.7, extra = 1, box.col = boxcols, main="Classification for wines quality")


######################################################################################################################
#   Eksperyment 2
#   W tym eksperymencie spróbuję poprawić działanie algorytmu rpart
#   poprzez spróbowanie wysterowania odpowiednimi współczynnikami
#   kontrolnymi rpart.control

myFormula <- qualityDiscretized ~ fixed.acidity + volatile.acidity +
  citric.acid +  residual.sugar + chlorides +  
  free.sulfur.dioxide + total.sulfur.dioxide + 
  density + pH + sulphates + alcohol 

#   W tym eksperymencie spróbuję tak ustawić rpart.control,
#   że współczynnik cp nastawię na 0, minsplit na 3 oraz
#   minbucket na 3, tak aby powstałe drzewo klasyfikacji
#   mogło się rozrastać bez żadnych ograniczeń.
myControl <- rpart.control(cp = 0, minsplit = 3, minbucket = 1)
rpTree <- rpart(myFormula,  method="class", data=trainData, control = myControl)

#   klasyfikacja danych treningowych - macierz błędów
trainPred = predict(rpTree,trainData,type = "class")
train_confusion_table <- table(trainData$qualityDiscretized,trainPred)
train_confusion_table
#             bad   average   premium
#   bad       144   7         0
#   average   1     3615      0
#   premium   2     5         131
cm <- confusionMatrix(trainPred, trainData$qualityDiscretized)
cm

#   Już po samej macierzy błędów dla danych treningowych widać 
#   bardzo duży postęp. Jest to spowodowane tym, że drzewo klasyfikacji
#   mogło utworzyć tyle gałęzi, ile potrzebowało ...
nodes <- as.numeric(rownames(rpTree$frame))
max(rpart:::tree.depth(nodes))
#   ... a dokładniej drzewo ma głębokość = 27.

#   Niestety jednak ograniczając głębokość drzewa, a także
#   zwiększając cp, minsplit lub minbucket traci się wykrywalność
#   w drzewie klasyfikacji win słabych i premium. Pomimo, iż ogólna 
#   ocena klasyfikatora może by wzrosła (biorąc pod uwagę to ile 
#   jest przeciętnych win w stosunku do premium i słabych), to 
#   traci się możliwośc wykrywania właśnie tych ciekawych win.


#   klasyfikacja danych testowych
testPred = predict(rpTree,testData,type = "class")
test_confusion_table <- table(testData$qualityDiscretized,testPred)
test_confusion_table
#             bad   average   premium
#   bad       7     25        0
#   average   27    860       32
#   premium   0     22        20

cm <- confusionMatrix(testPred, testData$qualityDiscretized)
cm
#   Dokładność = 0.8933


#   Sprawdzenie współczynników dla słabych win 
test_bad_true <- dim(testData[testData$qualityDiscretized == "bad",])[1]
test_bad_predict <- length(testPred[testPred == "bad"])

test_predict_true_bad <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "bad" && testPred[tmp] == "bad") {
    test_predict_true_bad <- c(test_predict_true_bad, 1)
  } else {
    test_predict_true_bad  <- c(test_predict_true_bad, 0)
  }
}
test_bad_precyzja <- sum(test_predict_true_bad) / test_bad_predict
test_bad_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'bad' = 0.2058824
test_bad_odzysk <- sum(test_predict_true_bad) / test_bad_true
test_bad_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'bad' = 0.21875
test_bad_f_miara <- 2 * test_bad_precyzja * test_bad_odzysk / (test_bad_precyzja + test_bad_odzysk)
test_bad_f_miara
#   F-miara dla qualityDiscretized == 'bad' = 0.2121212

#   Podsumowując współczynniki i f-miarę dla słabych win, nie są one zadowalające
#   F-miara na poziomie 21 procent oznacza, że ten klasyfikator nie radzi
#   sobie z klasyfikacją słabych win. Częściej będzie je zaliczał do przeciętnych
#   co widać na macierzy pomyłek.

#   Sprawdzenie współczynników dla przeciętnych win
test_average_true <- dim(testData[testData$qualityDiscretized == "average",])[1]
test_average_predict <- length(testPred[testPred == "average"])

test_predict_true_average <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "average" && testPred[tmp] == "average") {
    test_predict_true_average <- c(test_predict_true_average, 1)
  } else {
    test_predict_true_average  <- c(test_predict_true_average, 0)
  }
}
test_average_precyzja <- sum(test_predict_true_average) / test_average_predict
test_average_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'average' = 0.9481808
test_average_odzysk <- sum(test_predict_true_average) / test_average_true
test_average_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'average' = 0.9357998
test_average_f_miara <- 2 * test_average_precyzja * test_average_odzysk / (test_average_precyzja + test_average_odzysk)
test_average_f_miara
#   F-miara dla qualityDiscretized == 'average' = 0.9419496

#   Podsumowując współczynniki i f-miarę dla przeciętnych win, są dobre.
#   F-miara na poziomie 94 procent oznacza, że z klasyfikacją win
#   przeciętnych ten klasyfikator nie ma problemu. Dodatkowo dopowiadając
#   że większość badanych win jest przeciętna ten klasyfikator będzie
#   dobry.

#   Sprawdzenie współczynników dla premium win
test_premium_true <- dim(testData[testData$qualityDiscretized == "premium",])[1]
test_premium_predict <- length(testPred[testPred == "premium"])

test_predict_true_premium <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "premium" && testPred[tmp] == "premium") {
    test_predict_true_premium <- c(test_predict_true_premium, 1)
  } else {
    test_predict_true_premium  <- c(test_predict_true_premium, 0)
  }
}
test_premium_precyzja <- sum(test_predict_true_premium) / test_premium_predict
test_premium_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'premium' = 0.3846154
test_premium_odzysk <- sum(test_predict_true_premium) / test_premium_true
test_premium_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'premium' = 0.4761905
test_premium_f_miara <- 2 * test_premium_precyzja * test_premium_odzysk / (test_premium_precyzja + test_premium_odzysk)
test_premium_f_miara
#   F-miara dla qualityDiscretized == 'premium' = 0.4255319

#   Podsumowując współczynniki i f-miarę dla win premium, są słąbe, lecz 
#   lepsze niż w przypadku słabych win. Nadal jednak ten klasyfikator 
#   częściej się myli niż odpowiada poprawnie na pytanie, czy
#   dane wino ma jakość premium.


#   Niestety, wynik dla danych testowych nie jest już tak wspaniały, 
#   jak dla danych treningowych. Może to być spowodowane przeuczeniem 
#   się drzewa, albo dużą rozbieżnością własności win typu premium
#   oraz bad. 
#   1) W przypadku przeuczenia się drzewa jest to problem, który można 
#   rozwiązać odpowiednio sterując parametrami rpart.
#   2) Jeżeli jednak problem leży w dużej rozbieżności właściwości win
#   typu premium oraz bad, to jednym z rozwiązań może być zmienienie
#   danych treningowych i testowych. To znaczy - zmniejszenie stosunku
#   danych treningowych do testowych oraz załadowanie do danych
#   treningowych większej ilości win premium i słabych.


#   graficzna prezentacja drzewa  - rpart
plot(rpTree, uniform=TRUE,     main="Classification for wines quality")
text(rpTree, use.n=TRUE, all=TRUE, cex=.2)

boxcols <- c("pink", "yellow", "palegreen3")[rpTree$frame$yval]
prp(rpTree, faclen = 1, extra = 1, box.col = boxcols, main="Classification for wines quality")

#   Niestety przy tak dużym drzewie nie widać nic dokładnie.
#   Można jedynie dostrzec stosunek liści bad, average oraz premium
#   do całości.


######################################################################################################################
#   Eksperyment 3
#   W tym eksperymencie spróbuję dodać więcej win typu premium oraz
#   słabych do danych treningowych, podczas gdy obetnę ilość win średnich.
#   W tym celu trzeba stworzyć nowe dane treningowe i testowe.

#   Druga próba podziału podziału (więcej premium i bad)
set.seed(1235)
#   Dla każdego typu wina tworzę osobne proporcję
#   Wina słabe
bad_wines = wineWhite_ds[wineWhite_ds$qualityDiscretized == 'bad', ]
sam <- sample(2, nrow(bad_wines), replace=TRUE, prob=c(0.90, 0.10))
trainData_2 <- bad_wines[sam==1,]
testData_2 <- bad_wines[sam==2,]
#   Wina przeciętne
average_wines = wineWhite_ds[wineWhite_ds$qualityDiscretized == 'average', ]
sam <- sample(2, nrow(average_wines), replace=TRUE, prob=c(0.8, 0.2))
trainData_2 <- rbind(trainData_2, average_wines[sam==1,])
testData_2 <- rbind(testData_2, average_wines[sam==2,])
#   Wina premium
premium_wines = wineWhite_ds[wineWhite_ds$qualityDiscretized == 'premium', ]
sam <- sample(2, nrow(premium_wines), replace=TRUE, prob=c(0.90, 0.10))
trainData_2 <- rbind(trainData_2, premium_wines[sam==1,])
testData_2 <- rbind(testData_2, premium_wines[sam==2,])

myFormula <- qualityDiscretized ~ fixed.acidity + volatile.acidity +
  citric.acid +  residual.sugar + chlorides +  
  free.sulfur.dioxide + total.sulfur.dioxide + 
  density + pH + sulphates + alcohol 
myControl <- rpart.control(cp = 0, minsplit =2)

rpTree <- rpart(myFormula,  method="class", data=trainData_2, control = myControl)
cm <- confusionMatrix(testPred, testData$qualityDiscretized)
cm
#   Dokładność = 0.8933

#   klasyfikacja danych treningowych - macierz błędów
trainPred = predict(rpTree,trainData_2,type = "class")
train_confusion_table <- table(trainData_2$qualityDiscretized,trainPred)
train_confusion_table
#            bad average premium
#   bad      164       0       0
#   average    0    3625       0
#   premium    0       0     155


#   klasyfikacja danych testowych - macierz błędów
testPred = predict(rpTree,testData_2,type = "class")
test_confusion_table <- table(testData_2$qualityDiscretized,testPred)
test_confusion_table
#           bad average premium
#   bad      10       9       0
#   average  23     858      29
#   premium   0      13      12

cm_test <- confusionMatrix(testPred, testData_2$qualityDiscretized)
cm_test


#   Sprawdzenie współczynników dla słabych win 
test_bad_true <- dim(testData_2[testData_2$qualityDiscretized == "bad",])[1]
test_bad_predict <- length(testPred[testPred == "bad"])

test_predict_true_bad <- c()
for (tmp in 1:dim(testData_2)[1]) {
  if (testData_2$qualityDiscretized[tmp] == "bad" && testPred[tmp] == "bad") {
    test_predict_true_bad <- c(test_predict_true_bad, 1)
  } else {
    test_predict_true_bad  <- c(test_predict_true_bad, 0)
  }
}
test_bad_precyzja <- sum(test_predict_true_bad) / test_bad_predict
test_bad_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'bad' = 0.3030303
test_bad_odzysk <- sum(test_predict_true_bad) / test_bad_true
test_bad_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'bad' = 0.5263158
test_bad_f_miara <- 2 * test_bad_precyzja * test_bad_odzysk / (test_bad_precyzja + test_bad_odzysk)
test_bad_f_miara
#   F-miara dla qualityDiscretized == 'bad' = 0.3846154

#   Podsumowując współczynniki i f-miarę dla słabych win, nadal jest miernie
#   Ewidentnie z F-miarą na poziomie 38% nie można powiedzieć, że
#   drzewo klasyfikacji będzie w stanie poradzić sobie z określeniem
#   wina słabego.

#   Sprawdzenie współczynników dla przeciętnych win
test_average_true <- dim(testData_2[testData_2$qualityDiscretized == "average",])[1]
test_average_predict <- length(testPred[testPred == "average"])

test_predict_true_average <- c()
for (tmp in 1:dim(testData_2)[1]) {
  if (testData_2$qualityDiscretized[tmp] == "average" && testPred[tmp] == "average") {
    test_predict_true_average <- c(test_predict_true_average, 1)
  } else {
    test_predict_true_average  <- c(test_predict_true_average, 0)
  }
}
test_average_precyzja <- sum(test_predict_true_average) / test_average_predict
test_average_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'average' = 0.975
test_average_odzysk <- sum(test_predict_true_average) / test_average_true
test_average_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'average' = 0.9428571
test_average_f_miara <- 2 * test_average_precyzja * test_average_odzysk / (test_average_precyzja + test_average_odzysk)
test_average_f_miara
#   F-miara dla qualityDiscretized == 'average' = 0.9586592

#   Podsumowując współczynniki i f-miarę dla przeciętnych win, są dobre.
#   F-miara jest na poziomie 95 procent (o 1% lepiej, niż w poprzednim 
#   eksperymencie). To oznacza, że z klasyfikacją win przeciętnych
#   ten klasyfikator nie ma problemu. Dodatkowo dopowiadając
#   że większość badanych win jest przeciętna ten klasyfikator będzie
#   dobry.

#   Sprawdzenie współczynników dla premium win
test_premium_true <- dim(testData_2[testData_2$qualityDiscretized == "premium",])[1]
test_premium_predict <- length(testPred[testPred == "premium"])

test_predict_true_premium <- c()
for (tmp in 1:dim(testData_2)[1]) {
  if (testData_2$qualityDiscretized[tmp] == "premium" && testPred[tmp] == "premium") {
    test_predict_true_premium <- c(test_predict_true_premium, 1)
  } else {
    test_predict_true_premium  <- c(test_predict_true_premium, 0)
  }
}
test_premium_precyzja <- sum(test_predict_true_premium) / test_premium_predict
test_premium_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'premium' = 0.2926829
test_premium_odzysk <- sum(test_predict_true_premium) / test_premium_true
test_premium_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'premium' = 0.48
test_premium_f_miara <- 2 * test_premium_precyzja * test_premium_odzysk / (test_premium_precyzja + test_premium_odzysk)
test_premium_f_miara
#   F-miara dla qualityDiscretized == 'premium' = 0.3636364

#   Podsumowując współczynniki i f-miarę dla przeciętnych premium, są 
#   bardzo słabe. Ponownie wynik jest poniżej 50%. 

#   Jak widać, klasyfikacja win przeciętnych idzie całkiem nieźle, ale 
#   z winami słabymi i premium nadal nie radzi sobie za dobrze.
#   Widać to dla samych macierzy błędów, dlatego też nie 
#   potrzebne są dane liczbowe.
#   Klasyfikacja danych treningowych dla drzew bez ograniczeń idzie 
#   bardzo dobrze, ale przy danych testowych częściej się myli
#   (biorąc pod uwagę bad i premium) niż przewiduje dobrze.
#   Być może błąd został popełniony jeszcze przed eksperymentem 1.
#   To znaczy przy dyskretyzacji. Być może, dyskretyzując ten zbiór
#   decyzja o połączeniu win o ocenie 5 i 6 (najczęstszych) była 
#   błędem i tylko tylko o tych winach jest wystarczająco dużo wiedzy,
#   żeby je poprawnie klasyfikować.



######################################################################################################################
#   Eksperyment 4
#   W tym eksperymencie spróbuję oprócz współczynników kontrolnych
#   z rpart.control posłużyć się jeszcze macierzą kosztów pomyłek.
myFormula <- qualityDiscretized ~ fixed.acidity + volatile.acidity +
  citric.acid +  residual.sugar + chlorides +  
  free.sulfur.dioxide + total.sulfur.dioxide + 
  density + pH + sulphates + alcohol 
myControl <- rpart.control(cp = 0.002, minsplit = 6, minbucket = 3)

#   Tworzenie macierzy kosztóW pomyłek. 
#   Na pewno nie chcemy aby wina słąbe byłY klasyfikowane
#   jako premium i na odwrót. "Odległość" między jakościami
#   win powinna być max 1, dlatego n_2 = 5*n
n = 1
n_2 = 5*n
lossM=matrix(c(0,n,n_2,n,0,n,n_2,n,0), byrow=TRUE, nrow=3)
rpTree <- rpart(myFormula,  method="class", control = myControl, data=trainData, parms = list(loss = lossM ))

#   klasyfikacja danych treningowych - macierz błędów
trainPred = predict(rpTree,trainData,type = "class")
train_confusion_table <- table(trainData$qualityDiscretized,trainPred)
train_confusion_table
#            bad average premium
#   bad      147       4       0
#   average    1    3614       1
#   premium    0       3     135
cm <- confusionMatrix(testPred, testData$qualityDiscretized)
cm

#   klasyfikacja danych testowych - macierz błędów
testPred = predict(rpTree,testData,type = "class")
test_confusion_table <- table(testData$qualityDiscretized,testPred)
test_confusion_table
#           bad average premium
#   bad      10      22       0
#   average  27     866      26
#   premium   0      22      20

#   Dla pierwszych wartości n i n_2 drzewo klasyfikacji daje
#   rezultaty podobne do tych, z poprzednich eksperymentóW.



#   Sprawdzenie współczynników dla słabych win 
test_bad_true <- dim(testData[testData$qualityDiscretized == "bad",])[1]
test_bad_predict <- length(testPred[testPred == "bad"])

test_predict_true_bad <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "bad" && testPred[tmp] == "bad") {
    test_predict_true_bad <- c(test_predict_true_bad, 1)
  } else {
    test_predict_true_bad  <- c(test_predict_true_bad, 0)
  }
}
test_bad_precyzja <- sum(test_predict_true_bad) / test_bad_predict
test_bad_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'bad' = 0.2702703
test_bad_odzysk <- sum(test_predict_true_bad) / test_bad_true
test_bad_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'bad' = 0.3125
test_bad_f_miara <- 2 * test_bad_precyzja * test_bad_odzysk / (test_bad_precyzja + test_bad_odzysk)
test_bad_f_miara
#   F-miara dla qualityDiscretized == 'bad' = 0.2898551

#   Podsumowując współczynniki i f-miarę dla słabych win, nadal jest miernie
#   F-miarą na poziomie 28% jest aż o 10% gorsza, niż w poprzednim eksperymencie.
#   Co nie zmienia faktu, że obie były poniżej 50%

#   Sprawdzenie współczynników dla przeciętnych win
test_average_true <- dim(testData[testData$qualityDiscretized == "average",])[1]
test_average_predict <- length(testPred[testPred == "average"])

test_predict_true_average <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "average" && testPred[tmp] == "average") {
    test_predict_true_average <- c(test_predict_true_average, 1)
  } else {
    test_predict_true_average  <- c(test_predict_true_average, 0)
  }
}
test_average_precyzja <- sum(test_predict_true_average) / test_average_predict
test_average_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'average' = 0.9516484
test_average_odzysk <- sum(test_predict_true_average) / test_average_true
test_average_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'average' = 0.9423286
test_average_f_miara <- 2 * test_average_precyzja * test_average_odzysk / (test_average_precyzja + test_average_odzysk)
test_average_f_miara
#   F-miara dla qualityDiscretized == 'average' = 0.9469656

#   Podsumowując współczynniki i f-miarę dla przeciętnych win, są dobre.
#   F-miara jest na poziomie 94 procent (o 1% mniej niż w poprzednim 
#   eksperymencie).

#   Sprawdzenie współczynników dla premium win
test_premium_true <- dim(testData[testData$qualityDiscretized == "premium",])[1]
test_premium_predict <- length(testPred[testPred == "premium"])

test_predict_true_premium <- c()
for (tmp in 1:dim(testData)[1]) {
  if (testData$qualityDiscretized[tmp] == "premium" && testPred[tmp] == "premium") {
    test_predict_true_premium <- c(test_predict_true_premium, 1)
  } else {
    test_predict_true_premium  <- c(test_predict_true_premium, 0)
  }
}
test_premium_precyzja <- sum(test_predict_true_premium) / test_premium_predict
test_premium_precyzja
#   Współczynnik precyzji dla qualityDiscretized == 'premium' = 0.4347826
test_premium_odzysk <- sum(test_predict_true_premium) / test_premium_true
test_premium_odzysk
#   Współczynnik odzysku dla qualityDiscretized == 'premium' = 0.4761905
test_premium_f_miara <- 2 * test_premium_precyzja * test_premium_odzysk / (test_premium_precyzja + test_premium_odzysk)
test_premium_f_miara
#   F-miara dla qualityDiscretized == 'premium' = 0.4545455

#   Podsumowując współczynniki i f-miarę dla przeciętnych premium, są 
#   słabe, lecz sporo lepsze niż w poprzednim eksperymencie.
#   Nadal są mniejsze niż 50%, które chciałbym osiągnąć dla win
#   słabych i premium, ale jest już bardzo blisko.


#   Spróbuję przyciąć takie drzewo
pRpTree<- prune(rpTree, cp = rpTree$cptable[which.min(rpTree$cptable[,"xerror"]),"CP"])

#   klasyfikacja danych treningowych - macierz błędów
trainPred = predict(pRpTree,trainData_2,type = "class")
train_confusion_table <- table(trainData_2$qualityDiscretized,trainPred)
train_confusion_table


#   klasyfikacja danych testowych - macierz błędów
testPred = predict(pRpTree,testData_2,type = "class")
test_confusion_table <- table(testData_2$qualityDiscretized,testPred)
test_confusion_table

#   Przycięcie drzewa spowodowało, że wszystkie liście ze
#   słąbymi winami i winami premium zostały obcięte.
#   To oznacza, że dla całości drzewa nie opłaca się trzymać
#   liści, które są małymi odsetkami i  na dodatek wprowadzają 
#   częściej w błąd niż dobrze przewidują. 



######################################################################################################################
#   Wnioski:
#   Wnioski są przede wszystkim zamieszczone na końcu każdego eksperymentu
#   oraz w trakcie obliczeń współczynników i f-miary.
#   F-miara dla win przeciętnych zawsze wahała się w okolicy 95%.
#   Dla win słąbych i premium nie przekraczała właściwie 50%, przy czym 
#   najlepsze były wyniki w ostatnim eksperymencie. Mogło to być spowodowane
#   decyzją jeszcze przed rozpoczęciem pierwszego eksperymentu, a dokłądniej
#   dyskretyzacją. Być może decyzja o nie wliczaniu do win premium win o 
#   quality=7 spowodowała tak kiepskie wyniki dla f-miar win premium, a 
#   może wystarczyło podzielić na więcej przedziałów (na przykłąd 3-4; 5; 6; 7-9).
#   Co ciekawe większość drzew klasyfikacji dla zbioru z dyskretyzacją
#   na wina słabe, przeciętne i premium myliło się maksymalnie o 
#   jedną odległość - to znaczy, że wina słabe nie były klasyfikowane
#   jako premium, a premium jako słabe.