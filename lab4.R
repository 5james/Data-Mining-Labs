#   MED 2017Z Lab4
#   autor 1: Jakub Guzek
#   autor 2: Carlos Zaldivar



###################################################################################################################
#                                                                                                                 #
#   Cel: Badanie zdarzeń, które występują wśród epizodów / pomiarów, które są niebezpieczne dla życia ludzkiego.  #
#   Opis: Zamierzamy zbadać jakie elementy poprzedzają (lub ewentualnie następują po) takie epizody/pomiary,      #
#    które są niebezpieczne dla życia ludzkiego. Epizody w przypadku badanego tutaj zbioru to symptomy            #
#    hipoglikemii (code=id_65), czyli niedocukrzenia - za mały poziom cukru w ograniźmie. Normalnie powinno się   #
#    także przeszukać elementy wokół symptomów hiperglikemii, czyli stanu, w którym poziom cukru we krwi wzrasta  #
#    ponad prawidłowy poziom, ale niestety nie znajduje się w badanej bazie danych. To były jednak tylko          #
#    symptomy. Równie ważne, albo może nawet ważniejsze są pomiary poziomu glukozy we krwi. Powinno się           #
#    zbadać takie pomiary, które zagrażają życiu to pomiary krwi, czyli które znacznie odbiegają od normy.        #
#    To znaczy, że wszystkie pomiary poziomu glukozy we krwi muszą być albo wyższe od przeciętnych, albo niższe.  #
#   Wydarzenia: To implikuje, że u nas wykrywanymi wydarzeniami są: kod id_65 (symptomy hipoglikemii) oraz pary   #
#    (kod, wartość) dla wszystkich możliwych pomiarów krwi, w których wartość znacznie odbiega od normy.          #
#    Wydarzeniami wokół są wszystkie możliwe pary (kod, wartość) z dostępnych w bazie danych, z tym wyjątkiem,    #
#    że czasami wartość jest równa 0. Wtedy wartość nie ma znaczenia, ale dla jednolitości zostawiamy wartość     #
#    (choć nie ma wpływu na wyniki).                                                                              #
#   Praktyczne wykorzystanie: Znalezione sekewencje reguł mogą służyć lekarzom oraz pacjentom (cukrzykom) w       #
#    znalezieniu wzorca zachowań lub pomiarów, które poprzedzają  hipoglikemię lub zbyt nisko / wysoki poziom     #
#    cukru w pomiarach glukozy we krwi. Dzięki temu lekarze będą mogli uświadomić pacjentów (lub sami będą        #
#    mogli o tym się dowiedzieć), jakie sytuacje muszą wzbudzać u nich powyższony stopień czujności dot. poziomu  #
#    glukozy we krwi. Wtedy można uprzedzić symptomy hipoglikemii (lub hiperglikemii), ponieważ lekarz może       #
#    wtedy powiedzieć pacjentowi co powinien uczynić. To znaczy, że jeśli znajdziemy sekwencję, po której jest    #
#    duże prawdopodobieństwo wykrycia hipoglikemii, lekarz może zalecić zjedzenie cukierka jeszcze przed tymi     #
#    wydarzeniami, albo jeśli są to pomiary to szybkie sprawdzenie poziomu glukozy we krwi i jeśli rzeczywiście   #
#    będzie obniżone może zalecić jakiś posiłek.                                                                  #
#                                                                                                                 #
###################################################################################################################

library(arulesSequences)

#https://archive.ics.uci.edu/ml/datasets/Diabetes

#   pobranie danych
#download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')

#   wczytanie danych do ramki danych
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
#View(diab.df)

#	  Omówienie rodzajów możliwych zarejestrowanych sytuacji, ich wartości oraz przyjęte założenia co do wartości

#	  33 = Regular insulin dose 
#	  regularna insulina - zaczyna działać po około 0.5 godziny, czas trwania około 8 godzin
#	  Wartości: ilość insuliny - od 0 do 21, 
#	  0 - pacjent wpisał 0 dla regularności, ale nie wziął insuliny
#	  99, 344 - ekstremalne wypadki

#	  34 = NPH insulin dose
#	  insulina NPH - zaczyna działać po 1-4 godzinach, szczyt 6-10, czas trwania 10-16 godzin
# 	Wartości: ilość insuliny - od 1 do 37
# 	94, 110, 136, 143, 163, 240, 279, 296, 388 - ekstremalne wypadki

#	  35 = UltraLente insulin dose
#	  insulina UltraLente - zaczyna działać po 4-6 godzinach, szczyt 14-24, czas trwania 28-36 godzin
#	  Wartości: ilość insuliny - od 3 do 30

#	  48 = Unspecified blood glucose measurement
#	  Nieokreślony pomiar glukozy we krwi 
#	  Zakładamy, że przed jakimś wydarzeniem
#	  Wartości: od 28 do 421 (2 razy nieokreślony - NA)

# 	57 = Unspecified blood glucose measurement
# 	Nieokreślony pomiar glukozy we krwi 
# 	Zakładamy, że po jakimś wydarzeniu
# 	Wartości: od 19 do 501 (1 raz nieokreślony - NA)

# 	58 = Pre-breakfast blood glucose measurement
# 	Pomiar glukozy we krwi przed śniadaniem
# 	Wartości: od 23 do 461  

# 	59 = Post-breakfast blood glucose measurement
# 	Pomiar glukozy we krwi po śniadaniu
# 	Wartości: od 59 do 339

# 	60 = Pre-lunch blood glucose measurement
# 	Pomiar glukozy we krwi przed obiadem
# 	Wartości: od 15 do 452 (3 razy nieokreślony - NA)

# 	61 = Post-lunch blood glucose measurement
# 	Pomiar glukozy we krwi po obiedzie
# 	Wartości: od 25 do 476

# 	62 = Pre-supper blood glucose measurement
# 	Pomiar glukozy we krwi przed kolacją
# 	Wartości: od 28 do 436 (2 razy nieokreślony - NA)

# 	63 = Post-supper blood glucose measurement
# 	Pomiar glukozy we krwi po kolacji
# 	Wartości: od 37 do 413

#   64 = Pre-snack blood glucose measurement
# 	Pomiar glukozy we krwi przed przekąską
# 	Wartości: od 28 do 461 (1 błąd pomiarowy - Wartości=0)

# 	65 = Hypoglycemic symptoms
# 	Pojawienie się symptomów hipoglikemii
# 	Wszystkie wartości równe 0

# 	66 = Typical meal ingestion
# 	Przeciętny posiłek
# 	Wszystkie wartości równe 0

# 	67 = More-than-usual meal ingestion
# 	Posiłek większy od przeciętnego
# 	Wszystkie wartości równe 0

# 	68 = Less-than-usual meal ingestion
# 	Posiłek mniejszy od przeciętnego
# 	Wszystkie wartości równe 0

#	  69 = Typical exercise activity
#	  Zwykłe ćwiczenia fizyczne
#	  Wszystkie wartości równe 0
#
# 	70 = More-than-usual exercise activity
# 	Ćwiczenia fizyczne o ponad przeciętnej trudności
# 	Wszystkie wartości równe 0
#
#	  71 = Less-than-usual exercise activity
# 	Ćwiczenia fizyczne o trudności poniżej przeciętnej
#	  Wszystkie wartości równe 0

# 	72 = Unspecified special event
#	  Niezidentyfkowane wydarzenie
#	  Wszystkie wartości równe 0

#   36 = ???
#   Niesprecyzowane - trzeba się pozbyć
diab.df <- subset(diab.df, !(diab.df['code']=='id_36'))

#   Następnie trzeba się pozbyć wszystkich wartości NA w kolumnie value, które i tak się nie przydadzą
diab.df <- subset(diab.df, !is.na(diab.df$value))

#   W celu uznaliśmy, że z badanych wydarzeń interesują nas hipoglikemia oraz pomiary krwi, w których 
#   poziom glukozy wykracza ponad skalę lub jest zbyt mały. Aby nie bawić się w cyframi trzeba
#   ustalić tę "skalę" z góry. Następnie zdyskretyzować kolumnę value w bazie danych. 
#   W tym celu posłużyliśmy się pojęciem średniej i odchyleniem standardowym. 
#   Dla każdego rodzaju kodu osobno wyznaczyliśmy 5 przedziałów - 'Low','Below Average','Average',
#   'Above Average' oraz 'High'. Są one wyznaczone odpowiednio dla każdego kodu między wartościami: 
#   -nieskończoność, średnia wartość z value-odchylenie stand., średnia wartość z value-0.5*odchylenie stand.,
#   średnia wartość, średnia wartość z value+ 0.5*odchylenie stand., średnia wartość z value+odchylenie stand., 
#   +nieskończoność. Ponadto dla kodów, w których wartość kolumny value jest równa 0, wartość zdyskretyzowana
#   jest równa 'Unspecified'.

codes <- unique(diab.df$code)
sub_tmp <- NA
for (tmp in seq(1, length(codes))) {
  sub <- subset(diab.df, diab.df$code == codes[tmp])
  if (mean(sub$value) != 0){
    sub["value_discretized"] <- cut(sub$value, c(-Inf,mean(sub$value)-sd(sub$value), mean(sub$value)-0.5*sd(sub$value), 
                                                 mean(sub$value)+0.5*sd(sub$value), mean(sub$value)+sd(sub$value), +Inf), 
                                    labels=c('Low','Below average','Average','Above average','High'))
  }
  else {
    sub["value_discretized"] <- 'Unspecified'
  }
  sub_tmp <- merge(sub, sub_tmp, all = TRUE)
}

#   Następnie wszystkie wartości przypisać spowrotem do diab.df
diab.df['value_discretized'] <- sub_tmp['value_discretized']

#   czyszczenie niepotrzebnych rzeczy
remove(codes)
remove(tmp)
remove(sub)
remove(sub_tmp)

#   Dodatkowo dla regularnej insuliny trzeba zrobić dodatkowe wartości 'None' dla wartośći równej 0
#   oraz dla wartośći powyżej 99 (włącznie) wartość zdyskretyzowaną 'Extreme high'
diab.df[which(diab.df$code == 'id_33' & diab.df$value == 0),]$value_discretized <- 'None'
diab.df[which(diab.df$code == 'id_33' & diab.df$value >= 99),]$value_discretized <- 'Extreme high'

#   Tak samo postąpić dla insuliny NPH dla wartości powyżej 94 (włącznie)
diab.df[which(diab.df$code == 'id_34' & diab.df$value >= 94),]$value_discretized <- 'Extreme high'


#   Następnie trzeba przekształcić dane do postaci transakcyjnej. W wydarzeniach u nas 
#   kod jest silnie związany z jego wartośćią, dlatego nie można pozwolić, aby roztałY
#   rozdzielone. W tym celu utworzona została nowa kolumna 'code_value'.
#   Reszta kolumn nie jest właściwie potrzebna, dlatego mogą zostać usunięte.
diab.df$code_value = paste(diab.df$code,diab.df$value_discretized)
diab.df$code=NULL
diab.df$value=NULL
diab.df$value_discretized=NULL

write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#   Wczytanie danych w postaci transkacji 
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
#View(as(diabSeq,"data.frame"))


#   Seria eksperymentów 1
#
#   W tej serii eksperymentóW zajęto się badaniem wydarzeń pomiarów krwi, w których wartość 
#   poziomu glukozy odbiega od normy - jest zbyt wysoka lub zbyt niska. 
#   W tym celu trzeba będzie znaleźć wszystkie reguły sekwencji, których następnikiem jest
#   kod dowolnego pomiaru krwi oraz wartość 'Low' albo 'High.'
#   Założenia co do parametrów SPparameter: 
#     - mingap = 300, czyli minimalny czas odstępu między wydarzeniami - 5 minut. Jest to
#       wystarczająco dużo, żeby zjeść posiłek. W czasie mniejszym niż 5 minut nic nie
#       powinno się wydarzyć.
#     - maxgap = 151200, czyli maxymalny czas między sekwencją wydarzeń - 42 godziny.
#       42 godziny to maxymalny czas od wzięcia najdłuższej działąjącej insuliny - UltraLente
#       do końca jej działania (zaczyna działać po max 6 godzinach i działa max do 36 godzin)
#     - maxsize - nie ma znaczenia, ponieważ jest u nas tylko 1 kolumna.
#   Początkowe ustawienia parametry SPparameter to współczynnik wsparcia = 0.2 oraz maksymalna
#   długość reguły (maxlen) = 4.
#   Najlepszymi regułami w tej serii eksperymentów będą te o najwyższych współczynnikach
#   wsparcia i zaufania. Zakładamy, że przy współczynniku wsparcia (względnego) większym niż 20%
#   ważniejszym współczynnikiem będzie zaufanie przy ocenie reguł.
#   Zaś ciekawe reguły w tej serii eksperymentów będą sekwencjami wydarzeń, które niekoniecznie
#   mają najwyższy współczynnik wsparcia (chociaż zaufanie powinno nadal być dosyć wysokie),
#   następnikiem jest jeden z zagrażających życiu pomiaróW, a poprzednikiem jest nietypowa
#   sekwencja wydarzeń (ciężko określić z góry jaka).
#   Z góry można za to określić, które reguły są nieciekawe oraz nieprzydatne - wszystkie takie, 
#   które mają w poprzednikach tylko następnik. To znaczy, że mają pomiar krwi z tym samym wynikiem 
#   (wysoki   lub niski) lub poprzednik składa się tylko z pomiaru krwi (innego niż w następniku) 
#   z tym samym wynikiem. 
param1 = new ("SPparameter", support = 0.2, maxsize = 2, mingap = 300, maxgap = 151200, maxlen = 4)

#   Następnie należy wykonać algorytm CSpace.
patternSeq1 = cspade(diabSeq,param1, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
length(patternSeq1)
summary(patternSeq1)
#itemFrequency(patternSeq1)
#   Znaleziono 48736 sekwencji dla wyżej wymienionych nastawów SPParameter, z których najczęstszym 
#   wydarzeniem jest "id_33 Average", czyli wzięcie zwykłej dawki regularnej insuliny, czego można 
#   było się spodziewać - przeciętni pacjenci do swoich raportóW wpisują przede wszystkim kiedy i 
#   ile insuliny wzięli. Mało komu chce się wpisywać kiedy regularnie zjadł, a pomiary krwi  
#   rozjbijają się nie na jeden kod tylko na dziewięć, dlatego częstość tych wydarzeń jest odpowiednio 
#   mniejsza. Co jest mniej do przewidzenia drugim najczęściej występującym wydarzeniem jest wzięcie 
#   mniejszej dawki insuliny niż przeciętna. Drugim bardzo ciekawym zjawiskiem jest to, że badani
#   pacjenci najczęściej badają krew przed śniadaniem (id_58), czyli po spaniu oraz przed kolacją.
#   W powyższym podsumowaniu widać także, że współczynnik wsparcia znalezionych sekwencji trzyma się
#   blisko wartości troszkę powyżej minimalnej zadanej (która jest równa 0.2), mimo iż maksymalna 
#   wartość współczynnika wsparcia jest równa 0.8989.

#   Odkrycie wszystkich reguł dla minimalnego współczynnika zaufanie = 0.30
seq_rules1 = ruleInduction(patternSeq1,confidence = 0.30)
length(seq_rules1)
summary(seq_rules1)
#   Odkryto 48 515 takich sekwencjo-reguł. Analizowane tutaj zaufanie ma się bardzo podobnie (jak nie tak
#   samo) do przebadanych przed chwilą sekwencji. Co do współczynnika zaufania to można zauważyć, że go
#   nie doceniliśmy. Minimalny jest 0.3, ale za to w pierwszym kwantyli jes tjuż 0.5517. Średnia zaufania
#   jest równa 0.6849. To oznacza, że większość sekwencjo-reguł będzie w miarę wiarygodna.

#   Eksperyment 0 / Serii 1
#   Na początku chcemy się przekonać z jaką ilością reguł mamy do czynienia, jakiego są typu (ogólny przegląd),
#   dlatego jako poprzednik nie będzie żadnego ograniczenia, a jako następnik może być dowolny pomiar krwi,
#   w którym pomiar glukozy wykraczał poza normę.
#   Podejrzewamy, że spora ilość tych reguł będzie posiadała w poprzedniku następnik - <rhs (+ewentualnie coś)> => rhs.
#   Z tego powodu najprawdopodobniej ten eksperyment będzie tylko służył ogólnemu rozejrzeniu się po regułach i ich współczynnikach.

seqRules0_1 <- subset(seq_rules1, (rhs(x) %pin% c('id_48 High') | rhs(x) %pin% c('id_48 Low') | rhs(x) %pin% c('id_57 High') | rhs(x) %pin% c('id_57 Low') | 
                                  rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                  rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                  rhs(x) %pin% c('id_60 High') | rhs(x) %pin% c('id_60 Low') | rhs(x) %pin% c('id_61 High') | rhs(x) %pin% c('id_61 Low') |  
                                  rhs(x) %pin% c('id_62 High') | rhs(x) %pin% c('id_62 Low') | rhs(x) %pin% c('id_63 High') | rhs(x) %pin% c('id_63 Low') | 
                                  rhs(x) %pin% c('id_64 High') | rhs(x) %pin% c('id_64 Low')))
summary(seqRules0_1)
#   Łącznie jest 9360 takich reguł. 
#   Wartość analizowanych wcześniej współczynnika wsparcia i zaufania pozostają zbliżone.

inspect(head(sort(seqRules0_1, by = 'confidence'), 50))
#   Co się okazało, następniki występują w poprzednikach, ale nie aż tak często i nie zawsze mają ten sam wynik pomiaru.
#   Co jednak bardzo zaburza pogląd na sekwencjo-reguły to poprzedniki, których pomiary / dawki są w granicach normy. 
#   Skoro pomiary lub dawki są w granicach tolerancji to nie są to nadzwyczajne wydarzenia. Powinniśmy je pominąć, 
#   dlatego następny badany podzbiór będzie odrzucał wszystkie reguły, których poprzedniki mają pomiary i dawki przeciętne.
#   Trzeba wspomnieć, że wartości są podzielone na 5 podzbiorów. W tym przypadku odrzucony został tylko ten środkowy.
#   Lekko ponad i lekko poniżej średniej wartości nadal są brane pod uwagę, gdyż są to już wartości wykraczające ponad normę.

seqRules0_2 <- subset(seq_rules1, ((rhs(x) %pin% c('id_48 High') | rhs(x) %pin% c('id_48 Low') | rhs(x) %pin% c('id_57 High') | rhs(x) %pin% c('id_57 Low') | 
                                    rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                    rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                    rhs(x) %pin% c('id_60 High') | rhs(x) %pin% c('id_60 Low') | rhs(x) %pin% c('id_61 High') | rhs(x) %pin% c('id_61 Low') |  
                                    rhs(x) %pin% c('id_62 High') | rhs(x) %pin% c('id_62 Low') | rhs(x) %pin% c('id_63 High') | rhs(x) %pin% c('id_63 Low') | 
                                    rhs(x) %pin% c('id_64 High') | rhs(x) %pin% c('id_64 Low')) 
                                    & !(lhs(x) %pin% c('Average'))))

summary(seqRules0_2)
#   Łącznie jest już tylko 1589 takich reguł. 
#   Wartość analizowanych wcześniej współczynnika wsparcia i zaufania nadal pozostają w miarę zbliżone (chociaż już mniej niż poprzedni).

#   Żeby uznać regułę za interesującą, powinna ona mieć współczynnik podniesienia większy od 1, ponieważ
#   oznacza to, że wysąpienie lewej strony reguły zwiększa prawdopodobieństwo wystąpienia prawej.
#   Bez zwrócenia uwagi na współczynnik podniesienia możliwe jest wykrycie pozornie bardzo poprawnych 
#   reguł (o wysokim zaufaniu i wsparciu), których "poprawność" wynika jednak jedynie z dużej częstotliwości
#   występowania następnika, która niepowiązana jest z poprzednikiem.
seqRules0_2 <- seqRules0_2[seqRules0_2@quality$lift > 1.0]

summary(seqRules0_2)
#   Takich reguł pozostało jedynie 63.
#   Wartość analizowanego wcześniej współczynnika wsparcia pozostaje bez zmian, lecz wartość analizowanego
#   współczynnika zaufania jest minimalnie podwyższona w stosunku do badanego przed chwilą sekwencjo-reguł seqRules0_2.

#   Można przejrzeć wszystkie, ponieważ jest ich stosunkowo mało
inspect(sort(seqRules0_2, by = 'support'))
#   Te wyniki wyglądają jeszcze ciekawiej ciekawiej:
#
#   1 <{"id_33 Below average"}> => <{"id_58 Low"}>           0.6969697  0.8679245 1.004965 
#   2 <{"id_33 Below average"}> => <{"id_58 High"}>          0.6969697  0.8679245 1.060797 
#   3 <{"id_33 Below average"}> => <{"id_62 High"}>          0.6818182  0.8490566 1.018868 
#   4 <{"id_33 Below average"},                                 
#      {"id_33 Below average"}> => <{"id_58 High"}>          0.6515152  0.8431373 1.030501 
#   5 <{"id_34 Below average"},                                 
#
#   Można zauważyć, że najczęściej występującym poprzednikiem dla tych sekwencjo-reguł
#   o najwyższym wsparciu jest "id_33 Below average", czyli dawka insuliny, która jest
#   trochę poniżej przeciętnej dawki. Czasami występuje także insulina NPH, która także
#   została podana w nieco mniejszej ilości, niż najczęściej. Co jest jeszcze 
#   ciekawsze te mniejsze dawki są poprzednikami dla zbyt wyskich, jak i zbyt małych
#   pomiarów glukozy we krwi. Szczególnie widoczne jest to w pierwszych dwóch
#   wynikach.

#   Z powodu bardzo rozbierznych następników przy podobnych poprzednikach może warto 
#   zajrzeć, jak się mają reguły posortowane wg. współczynnika podniesienia.
inspect(sort(seqRules0_2, by = 'lift'))
#   Znowu troszkę przeszkadzają w poprzednikach występowanie następników, ale 
#   pomijając ten fakt można zauważyć bardzo ciekawą regułę:
#   2 <{"id_34 Below average"},                                 
#   {"id_67 Unspecified"}>   => <{"id_64 High"}>          0.2121212  0.7368421 1.621053 
#   Jest to reguła o wysokim zaufaniu, całkiem dobrym wsparciu i bardzo dobrym współczynniku podniesienia.
#   Mówi ona, że jeśli pacjent wziął mniejszą niż przeciętną dawkę insuliny NPH, po czym zjadł obfity
#   posiłek, to jest duże prawdopodobieństwo, że następny pomiar glukozy we krwi może być zbyt wysoki.

#   Fakt, że w poprzednikach sekwencjo-reguł bardzo często występują bardzo często pomiary lub dawki,
#   które tylko trochę odbiegają od normy kusi, żeby zbadać sytuacje, kiedy te pomiary lub dawki 
#   znacznie odbiegają od normy.
#   Dlatego zostanie zrobiony jeszcze jeden mini-eksperyment na ogóle danych. Tym razem
#   jednak z poprzedników wyrzucamy nie tylko 'uśrednione pomiary lub dawki, ale także te, 
#   które tylko trochę 'Average', ale także 'Below average' oraz 'Above average', czyli w 
#   skrócie 'average'
seqRules0_3 <- subset(seq_rules1, ((rhs(x) %pin% c('id_48 High') | rhs(x) %pin% c('id_48 Low') | rhs(x) %pin% c('id_57 High') | rhs(x) %pin% c('id_57 Low') | 
                                      rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                      rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                      rhs(x) %pin% c('id_60 High') | rhs(x) %pin% c('id_60 Low') | rhs(x) %pin% c('id_61 High') | rhs(x) %pin% c('id_61 Low') |  
                                      rhs(x) %pin% c('id_62 High') | rhs(x) %pin% c('id_62 Low') | rhs(x) %pin% c('id_63 High') | rhs(x) %pin% c('id_63 Low') | 
                                      rhs(x) %pin% c('id_64 High') | rhs(x) %pin% c('id_64 Low')) 
                                   & !(lhs(x) %pin% c('Average') | lhs(x) %pin% c('average'))))

#   Oczywiście nadal nas interesują sekwencjo-reguły o współczynniku podniesienia > 1.0
seqRules0_3 <- seqRules0_3[seqRules0_3@quality$lift > 1.0]

summary(seqRules0_3)
#   Jest tylko 10 takich reguł.

inspect(sort(seqRules0_3, by = 'lift'))
#   Nadal występuje problem z występowaniem następników w poprzednikach.
#   Najciekawszymi regułami z tego zbioru sekwencjo-reguł są:
#   2 <{"id_67 Unspecified"},                               
#      {"id_67 Unspecified"}> => <{"id_64 High"}>        0.2121212  0.5384615 1.184615 
#   3 <{"id_67 Unspecified"}> => <{"id_64 High"}>        0.2727273  0.5294118 1.164706, 
#   chociaż są zbyt podobne do tej, która została określona wcześniej.
#   Z kolei występują także bardzo bezsensowne wyniki, jak:
#   6 <{"id_57 Low"}>         => <{"id_57 High"}>        0.2121212  0.4117647 1.087059 
#   Na przykład w tym przykładzie niski pomiar jest poprzednikiem wysokiego wyniku pomiaru glukozy we krwi...

#   Fakt występowania w poprzednikach następników i to, że ciężko jest manipulować poprzednikami, 
#   kiedy nie wiadomo jaki jest konkretny następnik zmusił nas do przeprowadzenia dla każdego kodu
#   osobnego eksperymentu. Ponadto uznaliśmy, że warto byłoby rozszerzyć bazę sekwencjo-reguł.
#   To oznacza, że należałoby zmniejszyć support do 0.15  oraz zwiększyć maxlen do 5.
param2 = new ("SPparameter", support = 0.15, maxsize = 2, mingap = 300, maxgap = 151200, maxlen = 5)

#   Następnie należy wykonać ponownie algorytm CSpace.
#   !!! UWAGA - algorytm może wykonywać się około 2 minut !!!
patternSeq2 = cspade(diabSeq,param2, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
length(patternSeq2)
summary(patternSeq2)
#itemFrequency(patternSeq2)
#   Znaleziono tym razem 555 385 (w porównaniu do 48 736) sekwencji dla wyżej wymienionych nastawów
#   SPParameter. Elementy najczęściej występujące w znalezionych sekwencjach pozostają bardzo podobne.
#   Jednynie zmienia się ich liczność. Co do współczynnika wsparcia, to wyraźnie się opuścił, 
#   zgodnie z oczekiwaniami.

#   Odkrycie wszystkich reguł dla minimalnego współczynnika zaufanie = 0.30
seq_rules2 = ruleInduction(patternSeq2,confidence = 0.30)
#   Interesują nas tylko reguły z lift > 1.0
seq_rules2 <- seq_rules2[seq_rules2@quality$lift > 1.0]
length(seq_rules2)
summary(seq_rules2)
#   Odkryto 202 882 takich sekwencjo-reguł. Analizowane tutaj zaufanie ma się bardzo podobnie do 
#   przebadanych przed chwilą sekwencji. Zaufanie po wyeliminowaniu wszystkich sekwencjo-reguł
#   z współczynnikiem podniesienia mniejszym niż 1.0 jest całkiem wysokie. Co prawda minimalne 
#   zaufanie jest równe 0.3, ale w pierwszym kwantylu jest 0.8333, a w trzecim aż 1.0.

#   Dla przyszłych celów warto zrobić listę wszystkich pomiarów krwi z podziałem na wartości
pomiary_high = c('id_48 High', 'id_57 High', 'id_58 High', 'id_59 High', 'id_60 High', 'id_61 High', 'id_62 High', 'id_63 High', 'id_64 High')
pomiary_low = c('id_48 Low', 'id_57 Low', 'id_58 Low', 'id_59 Low', 'id_60 Low', 'id_61 Low', 'id_62 Low', 'id_63 Low', 'id_64 Low')
pomiary_average = c('id_48 Average', 'id_57 Average', 'id_58 Average', 'id_59 Average', 'id_60 Average', 'id_61 Average', 'id_62 Average', 'id_63 Average', 'id_64 Average')
pomiary_above_average = c('id_48 Above average', 'id_57 Above average', 'id_58 Above average', 'id_59 Above average', 'id_60 Above average', 'id_61 Above average', 'id_62 Above average', 'id_63 Above average', 'id_64 Above average')
pomiary_below_average = c('id_48 Below average', 'id_57 Below average', 'id_58 Below average', 'id_59 Below average', 'id_60 Below average', 'id_61 Below average', 'id_62 Below average', 'id_63 Below average', 'id_64 Below average')
pomiary_wszystkie = c(pomiary_high, pomiary_above_average, pomiary_average, pomiary_below_average, pomiary_low)

#   Eksperyment 1 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza  odbiegające od normy  nieokreślone pomiary 
#   glukozy we krwi, czyli wydarzenia z kodem 'id_48' i 'id_57'. Są zebrane razem, ponieważ niewiadomo 
#   do czego odwołują się dane pomiary, mimo iż są klasyfikowane jako dwa różne rodzaje wydarzeń.

#   Najpierw zajęliśmy się sekwencjo-regułami z kodem 'id_48'. W tym celu trzeba
#   znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_48'.
seqRules1_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_48 High' | rhs(x) %pin% 'id_48 Low'))
length(seqRules1_1)
#   Jest 2887 takich reguł.
inspect(head(sort(seqRules1_1, by='lift'), 10))
#   Można zauważyć, że wszystkie z początkowych są dla 'id_48 Low'. Z tego powodu, aby przeanalizować
#   również 'id_48 High', może warto rozdzielić na 'id_48 Low' i 'id_48 High'.
seqRules1_1_high <- subset(seq_rules2, (rhs(x) %pin% 'id_48 High'))
seqRules1_1_low <- subset(seq_rules2, (rhs(x) %pin% 'id_48 Low'))


#   Na początku zajmiemy się wysokim pomiarem glukozy we krwi.
length(seqRules1_1_high)
#   Jest 1368 sekwencjo-reguł.
inspect(head(sort(seqRules1_1_high, by='support'), 30))
inspect(head(sort(seqRules1_1_high, by='lift'), 30))
#   W obu przypadkach (sort by lift i support) najczęstszymi poprzednikami są nieciekawe pomiary Average
#   albo dawki averaki lub pomiary High. Pozbądźmy się ich, ponieważ jak już wcześniej tłumaczyliśmy, 
#   są one nieciekawe - albo nic się nie dzieje (average), więc nie ma sensu reagować, albo
#   pomiar jest już po innym pomiarze, z którego już się dowiedzieliśmy o zbyt wysokim stężeniu glukozy
#   we krwi. Warto też wyrzucić poprzednika.
seqRules1_1_high <- subset(seqRules1_1_high, !(lhs(x) %pin% c('id_48 High')))
seqRules1_1_high <- subset(seqRules1_1_high, !(lhs(x) %pin% c('Average')))
#   Zostało jedynie 18 reguł
inspect(sort(seqRules1_1_high, by='lift'))
#   Większość z nich nich zawiera w poprzedniku id_33 Below average, na przykład:
#   1 <{"id_48 Below average"},                                 
#      {"id_33 Below average"},                                 
#      {"id_33 Below average"}> => <{"id_48 High"}>          0.1666667  1.0000000 3.666667 
#   2 <{"id_48 Below average"},                                 
#      {"id_33 Below average"}> => <{"id_48 High"}>          0.1666667  0.9166667 3.361111 
#   Nawet pomimo troszkę za niskiemu wynikowi pomiaru glukozy we krwi w sekwencjo-regule 1
#   okazało się że w przeciągu 42 godzin sytuacje może się diametralnie zmienić. Niemałe
#   prawdopodobieństwo wskazuje na za niską dawkę insuliny.
#   Teraz sprawdźmy, jakie reguły pozostaną bez 'id_33 Below average'.
seqRules1_1_high <- subset(seqRules1_1_high, !(lhs(x) %pin% c('id_33 Below average')))
#   Większość (jak nie wszystkie) poprzedniki to po prostu inne pomiary, które wskazywały
#   na podwyższony lub za wysoki poziom glukozy we krwi. Wyczyśćmy te wyniki
for (tmp in pomiary_high) {
  seqRules1_1_high <- subset(seqRules1_1_high, !(lhs(x) %pin% tmp))
}
#   Teraz już gołym okiem widać, że wszystkie (za wyjątkiem <{"id_48 Low"}> => <{"id_48 High"}>)
#   poprzedniki to pomiary poziomu glukozy we krwi, które wykazały wynik lekko ponad normę.

#   Wnioski z tej ćwiartki eksperymentu: zbyt niska dawka insuliny może prowadzić do
#   zbyt wysokiego stężenia glukozy we krwi.

#   Teraz zajmiemy się niskim pomiarem glukozy we krwi.
length(seqRules1_1_low)
#   Jest 1519 takich sekwencjo-reguł.
inspect(head(sort(seqRules1_1_low, by='support'), 30))
inspect(head(sort(seqRules1_1_low, by='lift'), 30))
#   Tutaj także należy dokonać czyszczenia.
seqRules1_1_low <- subset(seqRules1_1_low, !(lhs(x) %pin% c('id_48 Low')))
seqRules1_1_low <- subset(seqRules1_1_low, !(lhs(x) %pin% c('Average')))
length(seqRules1_1_low)
#   Została jedynie 1 reguła
inspect(seqRules1_1_low)
#   A na dodatek jest mało ciekawa, dlatego brak wniosków z tej ćwiartki eksperymentu.

#   Teraz zajmiemy się sekwencjo-regułami z kodem 'id_57'. W tym celu trzeba
#   znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_57'.
seqRules1_2 <- subset(seq_rules2, (rhs(x) %pin% 'id_57 High' | rhs(x) %pin% 'id_57 Low'))
length(seqRules1_2)
#   Jest ich tylko 108 takich reguł.
inspect(head(sort(seqRules1_2, by='lift'), 50))
#   Jak widać znowu duża część tych reguł nie wykazuje nic ciekawego
#   Albo pomiary wskazują Average, albo dawki są Average.
#   Dlatego warto przeczyścić dane sekwencjo-reguły.
seqRules1_2 <- subset(seqRules1_2, !(lhs(x) %pin% 'Average'))
length(seqRules1_2)
#   Zostało ich jedynie 14, dlatego nie warto podejmować takiej dogłębnej analizy,
#   jak poprzedniej połowy eksperymentu. Można ręcznie przejrzeć każdą sekwencjo-regułę.
inspect(sort(seqRules1_2, by='lift'))
#   Najciekawszymi znalezionymi regułami sa:
#   6 <{"id_33 Below average"},                                 
#      {"id_57 Below average"},                                 
#      {"id_33 Below average"}> => <{"id_57 Low"}>           0.1666667  0.6111111 1.186275 
#   13 <{"id_34 Below average"},                                 
#      {"id_33 Below average"},                                 
#      {"id_65 Unspecified"},                                   
#      {"id_33 Below average"}> => <{"id_57 Low"}>           0.1666667  0.5238095 1.016807 
#   Obie reguły mają wysoki współczynnik zaufania.
#   Pierwsza reguła mówi o tym, że jeśli pacjent zażyje lekko zaniżoną dawkę insuliny zwykłej,
#   a następnie pomiar poziomu glukozy we krwi będzie zbyt mały, po czym kolejna dawka insuliny
#   będzie lekko zaniżona, to istnieje wysokie ryzyko zbyt małego stężenia glukozy we krwi, czyli
#   hipoglikemi. Rozwiązaniem może być tutaj większa dawka insuliny lub posiłek - to już kwestia
#   lekarza do rozstrzygnięcia. Druga reguła jest ciekawa ze względu na to, że posiada w
#   poprzednikach wydarzenie 'id_65', czyli symptomy hipoglikemii. Przełożyły się one bowiem 
#   wraz ze zbyt małymi dawkami insulin: regularnej i NPH, na potwierdzoną hipoglikemię.
#   Wnioski z tej połowy eksperymentu: Należy uważać dawki insulin - czy to regularnej, czy
#   NPH - (szczególnie, kiedy następują symptomy hipoglikemii), ponieważ może to prowadzić 
#   do prawdziwej hipoglikemii.

#   Dodatkowo przeprawdzimy podeksperyment sprawdzający wpływ jedzenia i aktywności fizycznej
#   na pomiary poziomu glukozy we krwi.

seqRules1_3 <- subset(seq_rules2, (rhs(x) %pin% 'id_48 High' | rhs(x) %pin% 'id_48 Low' | rhs(x) %pin% 'id_57 High' | rhs(x) %pin% 'id_57 Low' ))
seqRules1_3_food <- subset(seqRules1_3, (lhs(x) %pin% 'id_66' | lhs(x) %pin% 'id_67' | lhs(x) %pin% 'id_68'))
length(seqRules1_3_food)
#   Brak reguł jedzenie => nienaturalny pomiar glukozy we krwi
seqRules1_3_activity <- subset(seqRules1_3, (lhs(x) %pin% 'id_69' | lhs(x) %pin% 'id_70' | lhs(x) %pin% 'id_71'))
length(seqRules1_3_activity)
#   Brak reguł aktywność fizyczna => nienaturalny pomiar glukozy we krwi


#   Wnioski z całego eksperymentu: Zbyt niska dawka insuliny może prowadzić do zbyt wysokiego
#   poziomu glukozy we krwi, ponieważ organizm nie będzie mógł przetworzyć zebranej glukozy.
#   Zbyt niska dawka może także wiązać się ze skrajnie przeciwnym wydarzeniem, czyli hipoglikemii.
#   Szczególnie nie należy tego lekceważyć, jeśli nastąpiły symptomy hipoglikemii.


#   #   Eksperyment 2 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza odbiegające od normy pomiary glukozy we krwi
#   przed śniadaniem, czyli wydarzenia z kodem 'id_58'. 

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_58'.
seqRules2_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_58 High' | rhs(x) %pin% 'id_58 Low'))
length(seqRules2_1)
#   Jest 681 takich reguł.
inspect(head(sort(seqRules2_1, by='lift', decreasing = TRUE), 10))
#   Już na samym początku widać bardzo ciekawą regułę:
#1  <{"id_33 Average"},                                       
#    {"id_67 Unspecified"},                                   
#    {"id_58 High"},                                          
#    {"id_62 Below average"}> => <{"id_58 High"}>          0.1515152          1 1.222222 
#   Zawiera ona w poprzednikach obfity posiłek oraz pomiar glukozy przed kolacją poniżej normy,
#   a także przeciętną dawkę insuliny, czego winikiem jest hiperglikemia, czyli zbyt wysoki
#   poziom glukozy we krwi.

#   Podzielmy wyniki na zbyt wysoki i zbyt niski poziom cukru we krwi.
seqRules2_1_high <- subset(seqRules2_1, (rhs(x) %pin% 'id_58 High'))
seqRules2_1_low <- subset(seqRules2_1, (rhs(x) %pin% 'id_58 Low'))

#   Na początku zajmiemy się wysokim pomiarem glukozy we krwi.
length(seqRules2_1_high)
#   Jest 1368 sekwencjo-reguł.
inspect(head(sort(seqRules2_1_high, by='support'), 30))
inspect(head(sort(seqRules2_1_high, by='lift'), 30))
#   Znowu w obu przypadkach (sort by lift i support) najczęstszymi poprzednikami są nieciekawe pomiary Average
#   albo dawki averaki lub pomiary High. Pozbądźmy się ich, ponieważ jak już wcześniej tłumaczyliśmy, 
#   są one nieciekawe - albo nic się nie dzieje (average), więc nie ma sensu reagować, albo
#   pomiar jest już po innym pomiarze, z którego już się dowiedzieliśmy o zbyt wysokim stężeniu glukozy
#   we krwi. Warto też wyrzucić poprzednika.
seqRules2_1_high <- subset(seqRules2_1_high, !(lhs(x) %pin% c('id_58 High')))
seqRules2_1_high <- subset(seqRules2_1_high, !(lhs(x) %pin% c('Average')))
length(seqRules2_1_high)
#   Zostało jedynie 25 reguł
inspect(sort(seqRules2_1_high, by='lift'))
#   Większość z nich nich zawiera w zbyt małą dawkę insuliny, jak w poprzednim badaniu,
#   lecz tutaj dawka jest bardzo mała, np:
#   1 <{"id_35 Low"},                                           
#      {"id_35 Low"}>           => <{"id_58 High"}>          0.1515152  1.0000000 1.222222 
#   Pokazuje to, że zbyt mała dawka insulihy Ultralente może prowadzić do hiperglikemii.
#   Równie ciekawą regułą jest:
#   16 <{"id_67 Unspecified"},                                   
#       {"id_60 High"},                                          
#       {"id_33 Below average"},                                 
#       {"id_33 Below average"}> => <{"id_58 High"}>          0.1515152  0.8333333 1.018519 
#   Pokazuja ta sekwencjo-reguła, że zbyt wysoki posiłek wraz ze zbyt wysokim pomiarem, po 
#   czym danie zbyt małej dawki insuliny może prowadzić do hiperglikemii.


#   Teraz spróbujmy się pozbyt typowego poprzednika, jaki jest 'id_33 Below average'.
seqRules2_1_high <- subset(seqRules2_1_high, !(lhs(x) %pin% c('id_33 Below average')))
#   Ciekawą, wcześniej niezauważoną regułą jest:
#   8 <{"id_64 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_65 Unspecified"},                                   
#      {"id_34 Below average"}> => <{"id_58 High"}>          0.1515152  0.9090909 1.111111 
#   Pokazuje ona zbyt mały poziom glukozy we krwi oraz zbyt małe dawki insuliny NPH mogą dawać
#   symptomy hipoglikemii, a w rzeczywistości cała sytuacja może prowadzić do hiperglikemii.
#   Niestety przedział czasowy sekwencji jest zbyt duży i nie można jednoznacznie określić, 
#   czy symptomy hipoglikemii mogą wskazywać w rzeczywistości na hiperglikemie.
#   Może też było tak, że pacjent nie zanotował bardzo obfitego posiłku, po czym bardzo
#   urósł poziom cukru. Niemniej jednak jest to ciekawa reguła

#   Teraz  zajmiemy się niskim pomiarem glukozy we krwi.
length(seqRules2_1_low)
#   Jest 1368 sekwencjo-reguł.
inspect(head(sort(seqRules2_1_low, by='support'), 10))
inspect(head(sort(seqRules2_1_low, by='lift'), 10))
#   Już pierwsza reguła jest warta wspomnienia:
#   1 <{"id_58 Average"},                                       
#      {"id_64 Below average"},                                 
#      {"id_33 Average"},                                       
#      {"id_65 Unspecified"}>   => <{"id_58 Low"}>           0.1515152          1 1.157895
#   Podobne przypadki z objawami hipoglikemii zostały już wcześniej omówione przy okazji 
#   esperymentu 1 / serii 1. Jedyną różnicą jest to, że dawka insuliny była w normie, a mimo
#   to pacjent poczuł symptomy hipoglikemii. Możliwe, że uprawiał wcześniej sport. Możliwe
#   jest też to, że pomiar przed przekąską był zbyt mały. Można gdybać, że jeśli potencjalnie
#   pomiar przed przekąską w nocy był zbyt mały, a przekąska była zbyt skromna, to po nocy
#   (po zażyciu insuliny) mogły nastąpić sympotmy hipoglikemii, co się sprawdziło w pomiarze
#   gluzkozy we krwi przed śniadaniem.

#   Jednak znowu w obu przypadkach (sort by lift i support) najczęstszymi poprzednikami są nieciekawe pomiary 
#   Average albo dawki averaki lub pomiary High. Pozbądźmy się ich, ponieważ jak już wcześniej tłumaczyliśmy, 
#   są one nieciekawe - albo nic się nie dzieje (average), więc nie ma sensu reagować, albo
#   pomiar jest już po innym pomiarze, z którego już się dowiedzieliśmy o zbyt wysokim stężeniu glukozy
#   we krwi. Warto też wyrzucić poprzednika.
seqRules2_1_low <- subset(seqRules2_1_low, !(lhs(x) %pin% c('id_58 Low')))
seqRules2_1_low <- subset(seqRules2_1_low, !(lhs(x) %pin% c('Average')))
length(seqRules2_1_low)
#   Zostało jedynie 39 reguł
#   Taką ilość można ręcznie przeglądać
inspect(sort(seqRules2_1_low, by='lift'))
#   Występują tutaj typowe poprzedniki - za mało insuliny regularnej lub NPH, symptomy hipoglikemii.
#   Ciekawe są trzy dwa wyniki ze względu na to, że mają zaufanie = 1.0:
#   1 <{"id_58 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_62 Above average"}> => <{"id_58 Low"}>           0.1666667  1.0000000 1.157895 
#   2 <{"id_34 Below average"},                                 
#      {"id_58 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_62 Above average"}> => <{"id_58 Low"}>           0.1666667  1.0000000 1.157895 
#   3 <{"id_58 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_62 Above average"},                                 
#      {"id_34 Below average"}> => <{"id_58 Low"}>           0.1666667  1.0000000 1.157895 
#   Wszystkie są podobne i można je tak opisać: Dawki insuliny NPH są trochę mniejsze niż przeciętne,
#   podczas dnia pomiary glukozy we krwi wskazują na lekki brak cukru, no i w końcu na koniec dnia
#   lekko podwyższony poziom cukru (niezła przekąska na koniec?). To może oznaczać, że tacy ludzie mają 
#   bardzo wysokie zapotrzebowanie na cukier. Może to też mieć związek z niską dawka insuliny NPH, gdyż
#   tylko taka w tych przykładach jest zażywana.

#   Teraz spróbujmy się pozbyt typowego poprzednika, jaki jest 'id_33 Below average'.
seqRules2_1_low <- subset(seqRules2_1_low, !(lhs(x) %pin% c('id_33 Below average')))
summary(seqRules2_1_low)
inspect(seqRules2_1_low)
#   Zostało 12 reguł i Wszystkie z nich są dobre patrząc na współczynniki zaufania, a nawet i wsparcia.
#   Jednak znowu, większość ma zbyt małą dawkę insuliny NPH. Niektóre (co ciekawsze) mają wydarzenie
#   sympotomów hipoglikemii.


#   Dodatkowo przeprawdzimy podeksperyment sprawdzający wpływ jedzenia i aktywności fizycznej
#   na pomiary poziomu glukozy we krwi.

seqRules2_2 <- subset(seq_rules2, (rhs(x) %pin% 'id_58 High' | rhs(x) %pin% 'id_58 Low' | rhs(x) %pin% 'id_59 High' | rhs(x) %pin% 'id_59 Low' ))
seqRules2_2_food <- subset(seqRules2_2, (lhs(x) %pin% 'id_66' | lhs(x) %pin% 'id_67' | lhs(x) %pin% 'id_68'))
length(seqRules2_2_food)
#   Jak widać są 32 reguły.
inspect(sort(seqRules2_2_food, by='lift'))
#   Analizując je, można przypuścić, że większość z nich to wpływ id_67, czyli dużego posiłku.
#   CO gorsze, wpływ posiłku nie wpływa jedynie na wysoki poziom cukru. Jest to najprawdopodobniej spowodowane
#   zbyt dużym maxgap (przestrzenią czasową między tymi wydarzeniami). Niektóre reguły są jednak warte uwagi:
#   5 <{"id_33 Average"},                                       
#      {"id_67 Unspecified"},                                   
#      {"id_33 Average"},                                       
#      {"id_33 Below average"}> => <{"id_58 High"}>          0.2727273  0.9000000 1.100000 


seqRules2_2_activity <- subset(seqRules2_2, (lhs(x) %pin% 'id_69' | lhs(x) %pin% 'id_70' | lhs(x) %pin% 'id_71'))
length(seqRules2_2_activity)
inspect(seqRules2_2_activity)
#   1 reguła, na dodatek raczej sprzeczna (zły maxgap).

# Wnioski z powyższego eksperymentu znajdują się pod koniec każdego podeksperymentu.

#   #   Eksperyment 3 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza odbiegające od normy pomiary glukozy we krwi
#   po śniadaniu, czyli wydarzenia z kodem 'id_59'. 

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_59'.
seqRules3_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_59 High' | rhs(x) %pin% 'id_59 Low'))
length(seqRules3_1)
#   Brak takich reguł - koniec eksperymentu


#   Eksperyment 4 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza odbiegające od normy pomiary glukozy we krwi
#   przed obiadem, czyli wydarzenia z kodem 'id_60'. 

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_60'.
seqRules4_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_60 High' | rhs(x) %pin% 'id_60 Low'))
length(seqRules4_1)
#   Jest 882 takich reguł.
inspect(head(sort(seqRules4_1, by='lift', decreasing = TRUE), 10))

#   Podzielmy wyniki na zbyt wysoki i zbyt niski poziom cukru we krwi.
seqRules4_1_high <- subset(seqRules4_1, (rhs(x) %pin% 'id_60 High'))
seqRules4_1_low <- subset(seqRules4_1, (rhs(x) %pin% 'id_60 Low'))

#   Na początku zajmiemy się wysokim pomiarem glukozy we krwi.
length(seqRules4_1_high)
#   Jest 374 sekwencjo-reguł.
inspect(head(sort(seqRules4_1_high, by='support'), 10))
inspect(head(sort(seqRules4_1_high, by='lift'), 10))
#   Znowu w obu przypadkach (sort by lift i support) najczęstszymi poprzednikami są nieciekawe pomiary Average
#   albo dawki averaki lub pomiary High. Pozbądźmy się ich, ponieważ jak już wcześniej tłumaczyliśmy, 
#   są one nieciekawe - albo nic się nie dzieje (average), więc nie ma sensu reagować, albo
#   pomiar jest już po innym pomiarze, z którego już się dowiedzieliśmy o zbyt wysokim stężeniu glukozy
#   we krwi. Warto też wyrzucić poprzednika.
seqRules4_1_high <- subset(seqRules4_1_high, !(lhs(x) %pin% c('id_60 High')))
seqRules4_1_high <- subset(seqRules4_1_high, !(lhs(x) %pin% c('Average')))
length(seqRules4_1_high)
#   Zostało jedynie 13 reguł
inspect(sort(seqRules4_1_high, by='lift'))
#   Sporo z nich jest ciekawych, chociażby pierwsze:
#   1 <{"id_33 Above average"},                                 
#      {"id_33 Above average"},                                 
#      {"id_33 Above average"},                                 
#      {"id_33 Above average"}> => <{"id_60 High"}>          0.1666667  0.9166667 1.210000 
#   2 <{"id_60 Low"},                                           
#      {"id_34 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_33 Below average"}> => <{"id_60 High"}>          0.1969697  0.8125000 1.072500 
#   3 <{"id_71 Unspecified"},                                   
#      {"id_33 Below average"},                                 
#      {"id_34 Below average"}> => <{"id_60 High"}>          0.1818182  0.8000000 1.056000 
#   Pierwsza nie była wcześniej spotkana. Jest tutaj bowiem trochę zbyt duże i częste 
#   dawkowanie insuliny regularnej. 
#   Drugą można interpretować tak, że dzień wcześniej był zbyt mały pomiar glukozy,
#   dlatego pacjent mógł zmniejszyć dawkę insulin (NPH oraz regularnej) i trochę więcej
#   zjeść. Co jest bardziej zastanawiające to ilość tych dawek insulin.
#   Trzecia wiąże się z lekką aktywnością fizyczną. To mogło spowodować, że pacjent
#   wcześniej coś zjadł (i nie zanotować). Po tym zbyt mało przyjął insuliny.

#   Teraz spróbujmy się pozbyt typowego poprzednika, jaki jest 'id_33 Below average'.
seqRules4_1_high <- subset(seqRules4_1_high, !(lhs(x) %pin% c('id_33 Below average')))
#   Zostało jedynie 7 reguł - przejrzyjmy je ręcznie
inspect(seqRules4_1_high)
#   Wszystkie są ciekawe, chociaż ich zaufanie jest poniżej 80%. Wszystkie
#   zostały także już także mniej lub więcej omówione w ramach poprzednich eksperymentóW.

#   Teraz  zajmiemy się niskim pomiarem glukozy we krwi.
length(seqRules4_1_low)
#   Jest 1368 sekwencjo-reguł.
inspect(head(sort(seqRules4_1_low, by='support'), 10))
inspect(head(sort(seqRules4_1_low, by='lift'), 10))
#   Na pierwszy rzut oka nic ciekawego nie widać, dlatego ograniczmy zbiór.
seqRules4_1_low <- subset(seqRules4_1_low, !(lhs(x) %pin% c('id_60 Low')))
seqRules4_1_low <- subset(seqRules4_1_low, !(lhs(x) %pin% c('Average')))
length(seqRules4_1_low)
#   Zostało jedynie 34 reguł
#   Taką ilość można ręcznie przeglądać
inspect(sort(seqRules4_1_low, by='lift'))
#   Nie występują tutaj nadzwyczaj ciekawe reguły, które nie zostały by już
#   omówione w ramach innych eksperymentóW. Jedyne, co się zmienia to pora pomiarów,
#   a co za tym idzie inne pomiary także przesunęły się w czasie.

#   Dodatkowo przeprawdzimy podeksperyment sprawdzający wpływ jedzenia i aktywności fizycznej
#   na pomiary poziomu glukozy we krwi.

seqRules4_2 <- subset(seq_rules2, (rhs(x) %pin% 'id_60 High' | rhs(x) %pin% 'id_60 Low' | rhs(x) %pin% 'id_60 High' | rhs(x) %pin% 'id_60 Low' ))
seqRules4_2_food <- subset(seqRules4_2, (lhs(x) %pin% 'id_66' | lhs(x) %pin% 'id_67' | lhs(x) %pin% 'id_68'))
length(seqRules4_2_food)
#   34 reguły.
inspect(sort(seqRules4_2_food, by='lift'))
#   Analizując je, można przypuścić, że wszystkie z nich to wpływ id_67, czyli dużego posiłku.
#   CO gorsze, wpływ posiłku nie wpływa jedynie na wysoki poziom cukru. Jest to najprawdopodobniej spowodowane
#   zbyt dużym maxgap (przestrzenią czasową między tymi wydarzeniami). Niektóre reguły są jednak warte uwagi:
#   4 <{"id_62 Average"},                                       
#      {"id_58 Average"},                                       
#      {"id_33 Below average"},                                 
#      {"id_67 Unspecified"}>   => <{"id_60 Low"}>           0.1969697  0.8666667 1.121569 


seqRules4_2_activity <- subset(seqRules4_2, (lhs(x) %pin% 'id_69' | lhs(x) %pin% 'id_70' | lhs(x) %pin% 'id_71'))
length(seqRules4_2_activity)
#   15 reguł
inspect(seqRules4_2_activity)
#   Te o większym zaufaniu dotyczą obniżonego poziomu cukru (te o podwyższony mogą być wynikiem błędu maxgap).
#   Większość z nich wiąże się jednak tylko z lekką aktywnością - lekkie bieganie po śniadaniu?

# Wnioski z powyższego eksperymentu znajdują się pod koniec każdego podeksperymentu.


#   Eksperyment 5 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza odbiegające od normy pomiary glukozy we krwi
#   po obiadzie, czyli wydarzenia z kodem 'id_61'. 

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_61'.
seqRules5_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_61 High' | rhs(x) %pin% 'id_61 Low'))
length(seqRules5_1)
#   0 reguł. Pacjenci raczej nie chcą mierzyć poziomu krwi po obiedzie

# Wnioski:
# * tak jak można by się spodziewać, niska dawka insuliny i/lub większy niż normalnie posiłek powodują zwiększenie stężenia cukru we krwi przed przekąską
# * nie istnieje analogiczna reguła dla zbyt niskiego stężenia - dane nie są wystarczająco jednozanacze i nie udało się znaleźć interesujących reguł

#   Eksperyment 6 / Serii 1
#   W tym eksperymencie badano co prowadzi do zbyt wysokich lub zbyt niskich pozimów glukozy we krwi,
#   wykrytych w trakcie badania przed zjedzeniem przekąski (id_64).  

high_1_1 <- subset(seq_rules1, !(lhs(x) %pin% c("id_64 High")) & rhs(x) %pin% c("id_64 High"))
summary(high_1_1)
inspect(head(sort(high_1_1, by='confidence'), 10))

# Uzyskano 330 reguł prowadzących do wysokich poziomów glukozy we krwi, z których większość ma długość 4 (a więc maksymalną), wskazuje
# to na możliwość zwiększenia dopuszczalnej długości reguł dla uzyskania lepszych wyników, niestety wiązałoby się to ze zbyt dużym
# zwiększeniem czasu wykonania.
# Z usyzkanych reguł 4 posiadają stosunkowo wysokie zaufanie (większe od 0.7), a więc uznano je za interesujące, reguły te to:
#
# lhs                           rhs                          support confidence     lift 
# <{"id_34 Below average"}                                  
# {"id_33 Average"}                                        
# {"id_67 Unspecified"}>    => <{"id_64 High"}>           0.2121212  0.7777778 1.711111 
# 2 <{"id_34 Below average"}                                  
# {"id_67 Unspecified"}>    => <{"id_64 High"}>           0.2121212  0.7368421 1.621053 
# 3 <{"id_64 Below average"}                                  
# {"id_58 Average"}                                        
# {"id_33 Average"}>        => <{"id_64 High"}>           0.2121212  0.7368421 1.621053 
# 4 <{"id_34 Below average"}                                  
# {"id_67 Unspecified"}                                    
# {"id_33 Average"}>        => <{"id_64 High"}>           0.2121212  0.7368421 1.621053 

# W trzech na cztery z nich powtarza się przyjęcie niższej niż zwykle dawki insuliny NPH. Również trzykrotnie powtarza się wcześniejsze
# zjedzenie ponad przeciętnie obfitego posiłku. Eksperyment ten wskazuje, że są to najważniejsze zdarzenia prowadzące do problemów
# ze zbyt wysokim pozimoem glukozy we krwi.

low_1_1 <- subset(seq_rules1, !(lhs(x) %pin% c("id_64 Low")) & rhs(x) %pin% c("id_64 Low"))
summary(low_1_1)
inspect(head(sort(low_1_1, by='confidence'), 10))

# Uzyskaliśmy mniej, bo 154, reguły prowadzące do niskich poziomów glukozy we krwi. Posiadają one też mniejsze zaufanie - żadna reguła
# nie posiada zaufania przekraczającego 0.7, a ta z największym ma długość 3 oraz wygląda w sposób następujący:
# lhs                           rhs                          support confidence     lift 
# <{"id_64 Average"}                                        
# {"id_33 Average"}>        => <{"id_64 Low"}>            0.3333333  0.6470588 1.220168
# Oznacza ona, że niski poziom glukozy pojawia się po wcześniejszym zjedzeniu przekąski oraz przyjęciu przeciętnej dawki zwykłej insuliny.
# Dosyć zaskakujące, że tak prozaiczne zdarzenia doprowadzić mogą do problemów zdrowotnych, jednak możliwe, że reguła nie jest po prostu
# wiarygodna.
# W poszukiwaniu ciekawszych reguł można by zmniejszyć wymagane wsparcie, niestety czas wykonania obliczeń wydłuża się w tym wypadku zbyt
# znacznie.

remove(low_1_1)
remove(high_1_1)

# Wnioski:
# Nie znaleziono wystarczająco dobrych reguł - współczynnik podniesienia okazał się dla nich zbyt niski.


#   Eksperyment 7 / Serii 1
#   W tym eksperymencie badano co prowadzi do zbyt wysokich lub zbyt niskich pozimów glukozy we krwi,
#   wykrytych w trakcie badania przed zjedzeniem kolacji (id_62).

high_1_2 <- subset(seq_rules1, !(lhs(x) %pin% c("id_62 High")) & rhs(x) %pin% c("id_62 High"))
summary(high_1_2)
inspect(head(sort(high_1_2, by='confidence'), 10))

# Uzyskano dużo, bo ponad 1200 reguł tłumaczących przyczyny wysokiego stężenia glukozy we krwi, z których 190 ma zaufanie większe
# lub równe 0.7, a 7 nawet bardzo duże zaufanie, większe od 0.9. Niestety ich współczynnik podniesienia nie jest zbyt wysoki
# (maximum wynosi 1.1294), co rodzi wątpliwości na ile reguły te są realnie użyteczne. Wątpliwości te pogłębiają się po spojrzeniu
# na regułę o jednocześnie największej ufności oraz współczynniku podniesienia:
#
# lhs                          rhs                         support confidence     lift 
# <{"id_48 Average"},                                       
#  {"id_33 Average"}>       => <{"id_62 High"}>          0.2424242  0.9411765 1.129412 
#
# Reguła ta oznacza, że uzyskanie przeciętnych wyników w poprzednim badaniu krwi oraz przyjęcie przeciętnej dawki zwykłej insuliny
# powoduje lub przynajmniej silnie koreluje ze zbyt dużą ilością glukozy we krwi w badaniu przed kolacją. Ponieważ oba te wydarzenia wydają
# się niewinne, powstaje podejrzenie, że wykryta w tym badaniu reguła i jej podobne nie są do końca godne zaufania.

low_1_2 <- subset(seq_rules1, !(lhs(x) %pin% c("id_62 Low")) & rhs(x) %pin% c("id_62 Low"))
summary(low_1_2)
inspect(head(sort(low_1_2, by='lift'), 10))

# Podobnie jak w poprzednim badaniu wykryto tu regułu o wysokim zaufaniu, ale niewiele wyższym od 1 współczynniku podniesienia. Postanowiono
# nie uznać wyszukanych tu reguł za przydatne.

remove(low_1_2)
remove(high_1_2)

#   Eksperyment 8 / serii 1
#   Ten eksperyment ma na celu zbadanie co poprzedza odbiegające od normy pomiary glukozy we krwi
#   po kolacji, czyli wydarzenia z kodem 'id_63'. 

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają następnik 'id_63'.
seqRules8_1 <- subset(seq_rules2, (rhs(x) %pin% 'id_63 High' | rhs(x) %pin% 'id_63 Low'))
length(seqRules8_1)
#   Jest tlko 47 takich reguł, dlatego można je ręcznie przejrzeć
inspect(sort(seqRules8_1, by='lift', decreasing = TRUE))
#   Wszystkie dotyczą zbyt wysokiego poziomu cukru po kolacji. Większość też wiąże się
#   wokół przeciętnymi dawkami insuliny. Czasami poprzednikami są wysokie poprzednie
#   pomiary, ale to nic nie daje w przełożeniu na praktykę, ponieważ pacjent
#   już wie o podwyższonym poziomie glukozy we krwi, a my mamy bardziej
#   przewidzieć takie sytuacje.


#   Seria eksperymentów 2
#
#   W tej serii eksperymentów zajęto się badaniem przyczyn pojawienia się hipoglikemii - stanu niebezpiecznie obniżonego
#   cukru we krwi. Hipoglikemia w przypadku analizowanych danych rozpoznawana jest po jej symptomach, a nie po wynikach
#   badań krwi (wartość pomiarów zawsze wynosi "Unspecified").
#   W celu znalezienia przyczyn hipoglikemii należy znaleźć reguły sekwencji, których następnikiem jest wydarzenie o kodzie "id_65".
#   Założenia co do parametrów SPparameter: 
#     - mingap = 300 - podobnie jak w poprzedniej serii eksperymentów parametr tej zostaje ustalony na 5 minut
#     - maxgap = 151200 - podobnie jak w poprzedniej serii ustawiamy ten parametr na 42 godziny
#     - maxsize - ponownie nie ma znaczenia
#   Początkowe ustawienia parametru SPparameter to współczynnik wsparcia = 0.2 oraz maksymalna
#   długość reguły (maxlen) = 4 (maksymalną długość reguły ustalono na tę wartość, ponieważ wyższa znacznie wydłuża
#   czas obliczeń).
#   Ponieważ hipoglikemia jest niebezpiecznym zdarzeniem (mogącym doprowadzić nawet do śmierci), nawet reguły o niskiej
#   pewności mogą okazać się przydatne (lepiej być przesadnie ostrożnym niż przesadnie beztroskim). Z tego powodu uznajemy,
#   że minimalna pewność interesującej reguły wynosi 0.3. Bardzo ważne jest również by współczynnik podniesienia reguły
#   wynosił co najmniej 1.
#   Za najlepsze reguły w tej serii eksperymentów uzna się te o najwyższej pewności.

#   Eksperyment 1 / Serii 2
#   Znajdowanie reguł dotyczących hipoglikemii z wykorzystaniem bazowych parametrów.

param_2_1 = new ("SPparameter", support = 0.2, mingap = 300, maxgap = 151200, maxlen = 4)
patternSeq2_1 = cspade(diabSeq,param_2_1, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
seq_rules_2_1 = ruleInduction(patternSeq2_1,confidence = 0.30)

hipo <- subset(seq_rules_2_1, !(lhs(x) %pin% c("id_65")) & rhs(x) %pin% c("id_65"))
summary(hipo)
inspect(head(sort(hipo, by='confidence'), 10))

# lhs                          rhs                         support confidence     lift 
# 1 <{"id_34 Below average"},                                 
# {"id_58 Average"},                                       
# {"id_33 Below average"}> => <{"id_65 Unspecified"}>   0.2878788  0.7600000 1.355676 
# 2 <{"id_58 Average"},                                       
# {"id_34 Below average"},                                 
# {"id_34 Below average"}> => <{"id_65 Unspecified"}>   0.2878788  0.7307692 1.303534 
# 3 <{"id_33 Average"},                                       
# {"id_58 Average"},                                       
# {"id_34 Below average"}> => <{"id_65 Unspecified"}>   0.2878788  0.7307692 1.303534 
# 4 <{"id_34 Below average"},                                 
# {"id_34 Below average"},                                 
# {"id_33 Below average"}> => <{"id_65 Unspecified"}>   0.2878788  0.7307692 1.303534 
# 5 <{"id_58 Average"},                                       
# {"id_34 Below average"},                                 
# {"id_33 Below average"}> => <{"id_65 Unspecified"}>   0.2878788  0.7307692 1.303534 

# Wyszukując reguły z użyciem podanych wcześniej parametrów uzyskano 592 reguł sekwencyjnych. Z pięciu nich o najwyższej pewności
# wszystkie bez wyjątku zawierają wydarzenia oznaczające aplikacje mniejszej niż normalnie ilości insuliny (zwykłej lub NPH),
# w 4/5 przypadków tego typu wydarzenia pojawiają się nawet co najmniej dwukrotnie w poprzedniku reguły.

#   Eksperyment 2
#   Spróbujemy teraz wyszukać reguły dla mniejszego wsparcia - może uzyskamy w ten sposób coś ciekawszego
#   niż w poprzednim eksperymencie.

param_2_1_2 = new ("SPparameter", support = 0.1, mingap = 300, maxgap = 151200, maxlen = 4)
patternSeq2_1_2 = cspade(diabSeq,param_2_1_2, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
seq_rules_2_1_2 = ruleInduction(patternSeq2_1_2,confidence = 0.30)

hipo_2 <- subset(seq_rules_2_1_2, !(lhs(x) %pin% c("id_65")) & rhs(x) %pin% c("id_65"))
summary(hipo_2)
inspect(head(sort(hipo_2, by='confidence'), 10))

# lhs                          rhs                         support confidence     lift 
# 1 <{"id_67 Unspecified"},                                   
# {"id_64 High"},                                          
# {"id_64 High"}>          => <{"id_65 Unspecified"}>   0.1060606  1.0000000 1.783784 
# # 2 <{"id_64 Below average"},                                 
# {"id_60 Average"},                                       
# {"id_60 Above average"}> => <{"id_65 Unspecified"}>   0.1060606  1.0000000 1.783784 
# 3 <{"id_58 High"},                                          
# {"id_72 Unspecified"},                                   
# {"id_58 Below average"}> => <{"id_65 Unspecified"}>   0.1212121  1.0000000 1.783784 
# 4 <{"id_63 Below average"},                                 
# {"id_33 Below average"},                                 
# {"id_33 Below average"}> => <{"id_65 Unspecified"}>   0.1060606  1.0000000 1.783784 
# 5 <{"id_34 Below average"},                                 
# {"id_64 High"},                                          
# {"id_67 Unspecified"}>   => <{"id_65 Unspecified"}>   0.1515152  0.9090909 1.621622 

# Po zmniejszeniu wsparcia odkryto około 5-6 więcej reguł, w tym 4 takie, które mają zaufanie równe 1 (największe z możliwych) oraz wysoki
# współczynnik podniesienia. Co ciekawe, większość z najlepszych reguł sekwencyjnych nie zawiera zdarzeń powiązanych z przyjmowaniem insuliny,
# tak jak to było w poprzednim badaniu. Sekwencje uzyskane w tych wynikach pokazują raczej znaczenie odpowiedniej diety (w tym
# unikania nadmiernie obfitych posiłków) dla diabetyków.

#   Eksperyment 3 / Seria 2
#   Na koniec postanowiono jeszcze sprawdzić co może wpłynąć na wywołanie hipoglikemii w krótszym czasie - zmniejszono maxgap do 12 godzin.
param_2_1_3 = new ("SPparameter", support = 0.1, mingap = 300, maxgap = 43200, maxlen = 4)
patternSeq2_1_3 = cspade(diabSeq,param_2_1_3, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
seq_rules_2_1_3 = ruleInduction(patternSeq2_1_3,confidence = 0.30)

hipo_3 <- subset(seq_rules_2_1_3, !(lhs(x) %pin% c("id_65")) & rhs(x) %pin% c("id_65"))
summary(hipo_3)

inspect(head(sort(hipo_3, by='lift'), 10))
# Jakość reguł znacznie spadła (jedynie około 3 mają współczynnik podniesienia oraz pewność na tyle wysoką by móc być brane pod uwagę),
# a treść tych reguł podobna jest do tych z pierwszego badania - podkreślany jest wpływ zbyt niskich dawek insuliny zwykłej oraz NPH.

# Wnioski:
# W eksperymentach 1 i 2 wykryto wiele nadających się do wykorzystania reguł (wsparcie większe lub równe 0.1, pewność większa od 0.7,
# współczynnik podniesienia wyraźnie większy od 1). Treść tych reguł nie była jednak zaskakująca - sprowadzają się one w gruncie rzeczy
# do dwóch zasad pomagających uniknąć hipoglikemii:
# * należy przyjmować odpowiednio duże dawki insuliny zwykłej lub/i NPH
# * należy unikać sporzywania zbyt dużych posiłków
# Jeżeli osoba chora skrupulatnie notuje przyjmowane dawki insuliny oraz sporzywane posiłki, wykryte reguły mogą
# ostrzec ją przed niedocukrzeniem zanim osiągnie ono niebezpieczne wartości.




#   Seria eksperymentóW 3
#   W tej serii eksperymentóW sprawdzimy wpływ aktywności fizycznej, posiłków na zbyt wysoki lub niski poziom
#   glukozy we krwi. Te badania będą przeprowadzone dla niskiego przedziału czasowego.

param3 = new ("SPparameter", support = 0.05, maxsize = 2, mingap = 300, maxgap = 7200, maxlen = 4)

patternSeq3 = cspade(diabSeq,param3, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
length(patternSeq3)
summary(patternSeq3)
#itemFrequency(patternSeq2)
#   Znaleziono  129 sekwencji dla wyżej wymienionych nastawów SPParameter.

#   Odkrycie wszystkich reguł dla minimalnego współczynnika zaufanie = 0.30
seq_rules3 = ruleInduction(patternSeq3,confidence = 0.30)
length(seq_rules3)
#   W wyniku otrzymano jedynie 5 reguł. Na dodatek nieciekawych. 
#   Prawdopodobnie maxgap = 7200 to wartość zbyt niska. Trzeba jeszcze
#   raz puścić algorytm dla większego przedziału czasowego.
param3 = new ("SPparameter", support = 0.05, maxsize = 2, mingap = 300, maxgap = 14400, maxlen = 4)

patternSeq3 = cspade(diabSeq,param3, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
length(patternSeq3)
summary(patternSeq3)
#itemFrequency(patternSeq2)
#   Znaleziono  129 sekwencji dla wyżej wymienionych nastawów SPParameter.

#   Odkrycie wszystkich reguł dla minimalnego współczynnika zaufanie = 0.30
seq_rules3 = ruleInduction(patternSeq3,confidence = 0.30)
length(seq_rules3)
#   W wyniku otrzymano jedynie już 73 reguł. Jest wśród nich trochę
#   ciekawych, ale na wszelki wypadek jeszcze zwiększmy maxgap i zmniejszmy support
param3 = new ("SPparameter", support = 0.01, maxsize = 2, mingap = 300, maxgap = 28800, maxlen = 6)

#   !!! UWAGA - algorytm może się wykonywać około 70 sekund.
patternSeq3 = cspade(diabSeq,param3, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
length(patternSeq3)
summary(patternSeq3)
#itemFrequency(patternSeq2)
#   Znaleziono  292 329 sekwencji dla wyżej wymienionych nastawów SPParameter.
#   Jest to już bardzo zadowalająca liczba reguł

#   Odkrycie wszystkich reguł dla minimalnego współczynnika zaufanie = 0.30
seq_rules3 = ruleInduction(patternSeq3,confidence = 0.30)
length(seq_rules3)
#   Z czego powstało 268 496 sekwencjo-reguł. Z taką liczbą można już spokojnie
#   przeprowadzić jakiś eksperyment

#   Na samym początku ograniczmy się jedynie do wysokich i niskich pomiarów glukozy
seq_rules3 <- subset(seq_rules3, (rhs(x) %pin% c('id_48 High') | rhs(x) %pin% c('id_48 Low') | rhs(x) %pin% c('id_57 High') | rhs(x) %pin% c('id_57 Low') | 
                                       rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                       rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                       rhs(x) %pin% c('id_60 High') | rhs(x) %pin% c('id_60 Low') | rhs(x) %pin% c('id_61 High') | rhs(x) %pin% c('id_61 Low') |  
                                       rhs(x) %pin% c('id_62 High') | rhs(x) %pin% c('id_62 Low') | rhs(x) %pin% c('id_63 High') | rhs(x) %pin% c('id_63 Low') | 
                                       rhs(x) %pin% c('id_64 High') | rhs(x) %pin% c('id_64 Low')))
seq_rules3 <- seq_rules3[seq_rules3@quality$lift > 1.0]
length(seq_rules3)
#   Wynikiem są 40 239 sekwencjo-reguły, których przedział czasowy jest mniejszy niż 4 godziny (maxgap = 28800).


#   Eksperyment 1 / serii 3
#   Wpływ spożycia posiłku na podwyższony lub obniżony poziom glukozy.

#   Na samym początku sprawdźmy reguły z przeciętnym posiłkiem
seqRules_3_1_food <- subset(seq_rules3, (lhs(x) %pin% 'id_66') & !(lhs(x) %pin% 'id_67' | lhs(x) %pin% 'id_68') )
length(seqRules_3_1_food)
#   5746 reguł.
inspect(head(sort(seqRules_3_1_food, by='lift')))
#   Zbyt wiele reguł z przeciętną dawką insuliny
seqRules_3_1_food <- subset(seqRules_3_1_food, !(lhs(x) %pin% 'id_33 Average'))
length(seqRules_3_1_food)
#   Zaskakująco bardzo się nie zmieniła liczba reguł = 5188
inspect(head(sort(seqRules_3_1_food, by='lift')))
#   Jest to duża liczba reguł. Ponadto mają małe wsparcie, ale można z tych reguł
#   wyciągnąc całkiem ciekawe sekwencje:
#   1 <{"id_35 Average"},                                       
#      {"id_60 Average"},                                       
#      {"id_66 Unspecified"}>   => <{"id_48 High"}>          0.01515152          1 3.666667 
#   Jak wiadomo to wszystko zadziało się w przeciągu niedużego odstępu czasu. To znaczy, że ta
#   reguła mówi, że jest niemałe prawdopodobieństo, że jeśli pacjentużywa insuliny typu
#   UltraLente (zaczyna działać po długim czasie) i mimo iż wynik pomiaru glukozy we krwi
#   po zażyciu insuliny UltraLente będzie ok, to po spożyciu posiłku poziom glukozy we krwi
#   może się bardzo podwyższyć, co może prowadzić do hiperglikemii.
#   Można sprawdzić ile reguł dotyczy obniżony poziom glukozy we krwi.
seqRules_3_2_food <- subset(seqRules_3_1_food, !(lhs(x) %pin% 'Average'))
length(seqRules_3_2_food)
#   Nadal 2893 reguł
inspect(head(sort(seqRules_3_2_food, by='lift'), 10))
inspect(head(sort(seqRules_3_2_food, by='support'), 10))
#   Przykładowa ciekawa reguła z dobrymi współczynnikami wsparcia, zaufania i podniesienia:
#   2 <{"id_66 Unspecified"},                                   
#      {"id_33 High"}>          => <{"id_57 Low"}>           0.03030303  1.0000000 1.941176 
#   Posiłek + zbyt duża dawka insuliny może prowadzić do zbyt małego poziomu glukozy we krwi.
#   Jednak należy jeszcze trochę przeczyścić dane - wyrzućić wszystkie pomiary średnie.
#   Aby to zrobić, najpierw trzeba podzielić na wysokie i niskie stężenie glukozy we krwi.
seqRules_3_2_food_high <- subset(seqRules_3_2_food, rhs(x) %pin% "High")
seqRules_3_2_food_low <- subset(seqRules_3_2_food, rhs(x) %pin% "Low")
#   Następnie pozbądźmy się wszystkich 
for (tmp in pomiary_high) {
  seqRules_3_2_food_high <- subset(seqRules_3_2_food_high, !(lhs(x) %pin% tmp))
}
for (tmp in pomiary_wszystkie) {
  seqRules_3_2_food_low <- subset(seqRules_3_2_food_low, !lhs(x) %pin% tmp)
}
inspect(head(seqRules_3_2_food_high))
#   Wszystkie eksperymenty stąd są w miarę dobre, tylko jest ich bardzo dużo i mają mały support.
#   Można zobacyzć taką regułę, jak:
#   5 <{"id_69 Unspecified"},                                   
#      {"id_66 Unspecified"},                                   
#      {"id_69 Unspecified"}>   => <{"id_57 High"}>          0.01515152        1.0 2.64 
#   która mówi, że jedzenie przed i po wysiłku prowadzi do podwyższonego poziomu glukozy we krwi.

inspect(head(seqRules_3_2_food_low))
#   Bardziej jednak są ciekawe reguły z wynikiem obniżonego poziomu glukozy po posiłku:
#   5 <{"id_65 Unspecified"},                                   
#      {"id_33 High"},                                          
#      {"id_33 Above average"},                                 
#      {"id_33 Above average"},                                 
#      {"id_66 Unspecified"}>   => <{"id_57 Low"}>           0.01515152          1 1.941176 
#    <{"id_35 High"},                                          
#      {"id_33 Above average"},                                 
#      {"id_33 Above average"},                                 
#      {"id_66 Unspecified"}>   => <{"id_57 Low"}>           0.01515152          1 1.941176 
#   Obie wybrane reguły przykładowe charakteryzują się wysokią dawką i częstotliwością przyjmowania
#   insulin regularnych lub UltraLenta


#   Teraz sprawdźmy reguły dla bardzo obfitego posiłku
seqRules_3_3_food <- subset(seq_rules3, (lhs(x) %pin% 'id_67') & !(lhs(x) %pin% 'id_66' | lhs(x) %pin% 'id_68') )
length(seqRules_3_3_food)
#   4379 reguł.
inspect(head(sort(seqRules_3_3_food, by='lift')))
#   Sporo wymienionych reguł jest wartych uwagi
#   Przykładową regułą może tutaj być:
#   1 <{"id_67 Unspecified"},                                   
#      {"id_33 Below average"},                                 
#      {"id_57 Below average"},                                 
#      {"id_34 Average"},                                       
#      {"id_69 Unspecified"}>   => <{"id_61 High"}>          0.03030303          1 9.428571 
#   Ma bardzo wysoki współczynnik podniesienia i zaufania, ale support tylko 0.03.
#   Wydarzeniami prowadzącymi do zbyt wysokiego poziomu glukozy we krwi były tutaj:
#   zbyt uboga dawka insuligu regularnej, regularny wysiłek fizyczny przy obfitym posiłku.
#   Pomijając wszystkie regularne insuliny ...
seqRules_3_3_food <- subset(seqRules_3_3_food, !(lhs(x) %pin% 'Average'))
length(seqRules_3_3_food)
#   ... okroiliśmy prawie do 1/4 reguł
#   Podzielmy znowu na dwie kategorie
seqRules_3_3_food_high <- subset(seqRules_3_3_food, rhs(x) %pin% "High")
seqRules_3_3_food_low <- subset(seqRules_3_3_food, rhs(x) %pin% "Low")
#   Następnie pozbądźmy się wszystkich 
for (tmp in pomiary_high) {
  seqRules_3_3_food_high <- subset(seqRules_3_3_food_high, !(lhs(x) %pin% tmp))
}
for (tmp in pomiary_wszystkie) {
  seqRules_3_3_food_low <- subset(seqRules_3_3_food_low, !lhs(x) %pin% tmp)
}
inspect(head(seqRules_3_3_food_high))
#   524 reguł seqRules_3_3_food_high
#   Sporo związanych ze zdarzeniem o kodzie id_72 - wyczyśćmy, bo nie wiadomo co to jest
seqRules_3_3_food_high <- subset(seqRules_3_3_food_high, !(lhs(x) %pin% 'id_72'))
inspect(head(sort(seqRules_3_3_food_high, by='support')))
#   Ponownie sporo ciekawych reguł, lecz wszystkie o małym wsparciu <= 6%
#   Na przykład takie wydarzenia:
#   5 <{"id_34 High"},                                          
#      {"id_65 Unspecified"},                                   
#      {"id_67 Unspecified"}>   => <{"id_64 High"}>          0.04545455       1.00 2.20 
#   6 <{"id_58 Low"},                                           
#      {"id_65 Unspecified"},                                   
#      {"id_67 Unspecified"}>   => <{"id_64 High"}>          0.04545455       0.75 1.65 
#   które mówią, że jeśli połączymy symptomy hipoglikemi, a nawet potwierdzimy to badaniem
#   poziomu glukozy we krwi, to w przypadku gdy zażyjemy insulinę UltraLente i zjemy bardzo
#   obfity posiłek, to może nie zacząć jeszcze działać i poziom glukozy we krwi może drastycznie
#   się podwyższyć - z hipoglikemii w hiperglikemię.

inspect(head(sort(seqRules_3_3_food_low, by='support')))
#   Ponownie większość eksperymentów jest dobra (choć oczywista), ale ma mały
#   współczynnik wsparcia <= 1.5% (oprócz jednego)
#   1 <{"id_70 Unspecified"},                                   
#      {"id_67 Unspecified"}>   => <{"id_59 Low"}>           0.03030303        0.4 5.280000 
#   2 <{"id_33 Below average"},                                 
#      {"id_70 Unspecified"},                                   
#      {"id_67 Unspecified"}>   => <{"id_64 Low"}>           0.01515152        1.0 1.885714 
#   3 <{"id_67 Unspecified"},                                   
#      {"id_68 Unspecified"},                                   
#      {"id_35 Low"}>           => <{"id_64 Low"}>           0.01515152        1.0 1.885714 
#   4 <{"id_67 Unspecified"},                                   
#      {"id_68 Unspecified"}>   => <{"id_62 Low"}>           0.01515152        1.0 1.222222 
#   5 <{"id_33 Below average"},                                 
#      {"id_34 Below average"},                                 
#      {"id_67 Unspecified"},                                   
#      {"id_65 Unspecified"}>   => <{"id_62 Low"}>           0.01515152        1.0 1.222222 
#   6 <{"id_69 Unspecified"},                                   
#      {"id_34 Below average"},                                 
#      {"id_67 Unspecified"},                                   
#      {"id_65 Unspecified"}>   => <{"id_62 Low"}>           0.01515152        1.0 1.222222 
#   Typowe objawy, oprócz obfitego posiłku to: nadmierna aktywność fizyczna,
#   niedostatecznie duża dawka insulin (regularnej, NPH i UltraLente).

#   Eksperyment 2 / serii 3
#   Wpływ aktywności fiz na pomiary glukozy we krwi

#   Można zmniejszyć maxgap
param3 = new ("SPparameter", support = 0.01, maxsize = 2, mingap = 300, maxgap = 15000, maxlen = 6)
patternSeq3 = cspade(diabSeq,param3, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
seq_rules3 = ruleInduction(patternSeq3,confidence = 0.30)
#   Na samym początku ograniczmy się jedynie do wysokich i niskich pomiarów glukozy
seq_rules3 <- subset(seq_rules3, (rhs(x) %pin% c('id_48 High') | rhs(x) %pin% c('id_48 Low') | rhs(x) %pin% c('id_57 High') | rhs(x) %pin% c('id_57 Low') | 
                                    rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                    rhs(x) %pin% c('id_58 High') | rhs(x) %pin% c('id_58 Low') | rhs(x) %pin% c('id_59 High') | rhs(x) %pin% c('id_59 Low') | 
                                    rhs(x) %pin% c('id_60 High') | rhs(x) %pin% c('id_60 Low') | rhs(x) %pin% c('id_61 High') | rhs(x) %pin% c('id_61 Low') |  
                                    rhs(x) %pin% c('id_62 High') | rhs(x) %pin% c('id_62 Low') | rhs(x) %pin% c('id_63 High') | rhs(x) %pin% c('id_63 Low') | 
                                    rhs(x) %pin% c('id_64 High') | rhs(x) %pin% c('id_64 Low')))
seq_rules3 <- seq_rules3[seq_rules3@quality$lift > 1.0]
length(seq_rules3)

#   Nie będziemy rozdzielać na poszczególne rodzaje aktywności (małe, duże, przeciętne)
seqRules_3_1_physical <- subset(seq_rules3, (lhs(x) %pin% 'id_69' | lhs(x) %pin% 'id_70' | lhs(x) %pin% 'id_71') )
summary(seqRules_3_1_physical)
#   Reguły o wysokim zaufaniu, ale niskim wsparciu
seqRules_3_1_physical_high <- subset(seqRules_3_1_physical, rhs(x) %pin% "High")
seqRules_3_1_physical_low <- subset(seqRules_3_1_physical, rhs(x) %pin% "Low")

#   Reguły, których wynikiem jest obniżony poziom glukozy we krwi po ćwiczeniach jest dosyć oczywisty
inspect(head(sort(seqRules_3_1_physical_low, by='lift')))


#   Bardziej ciekawe są te reguły, w których pomimo ćwiczeń występuje hiperglikemia
#   których o dziwo jest więcej niż reguł związanych z ćwiczeniami i hipoglikemią
inspect(head(sort(seqRules_3_1_physical_high, by='lift')))
#   1 <{"id_57 Low"},                                       
#      {"id_33 Average"},                                   
#      {"id_69 Unspecified"}> => <{"id_57 High"}>        0.01515152          1 2.64 
#   2 <{"id_66 Unspecified"},                               
#      {"id_57 Low"},                                       
#      {"id_33 Average"},                                   
#      {"id_69 Unspecified"}> => <{"id_57 High"}>        0.01515152          1 2.64 
#   3 <{"id_33 Average"},                                   
#      {"id_66 Unspecified"},                               
#      {"id_57 Low"},                                       
#      {"id_33 Average"},                                   
#      {"id_69 Unspecified"}> => <{"id_57 High"}>        0.01515152          1 2.64 
seqRules_3_1_physical_high_1 <- subset(seqRules_3_1_physical_high, lhs(x) %pin% "High")



#   Seria eksperymentów 4
#   Uznaliśmy, że warto byłoby także przejrzeć wartości 'Extreme' przy dawkach insuliny zwykłej i NPH (id_33 i id_34)

#   Najpierw należy znaleźć wszystkie sekwencjo-reguły, które mają poprzednik 'id_33 Extreme high'. Ponieważ zdarzeń
#   z zaaplikowaniem dawki "Extreme high" jest bardzo mało, wybieramy zbiór reguł z bardzo niskim wymaganym wsparciem.

seq_rules_ex33 <- subset(seq_rules3, lhs(x) %pin% 'id_33 Extreme high')
summary(seq_rules_ex33)
inspect(head(sort(seq_rules_ex33, by='confidence'), 10))

#   Znaleziono dwie reguły, obie przewidują wysoki pomiar cukru przed kolacją.

#   Teraz znajdujemy wszystkie sekwencjo-reguły, które mają poprzednik 'id_34 Extreme high'.
seq_rules_ex34 <- subset(seq_rules3, lhs(x) %pin% 'id_34 Extreme high')
summary(seq_rules_ex34)
inspect(head(sort(seq_rules_ex34, by='support'), 186))
#   Tych znaleziono dużo więcej - 186. W każdym przypadku znalezione reguły przewidują anormalne stężenie cukru we krwi
#   po przyjęciu tak dużej dawki insuliny. Może to być zarówno wysokie jak i niskie stężenie. Ze względu na brak wiedzy
#   medycznej trudno nam spekulować co jest tego przyczyną, prosta intuicja mówi, że duża dawka insuliny powinna powodować
#   jedynie niskie stężenie cukru. Tak czy inaczej, zarówno niskie jak i wysokie stężenie jest szkodliwe, tak więc z eksperymentu
#   wynika jednoznacznie, że należy unikać przyjmowania bardzo dużych dawek insuliny.




#   Wnioski / wyniki końcowe:
#   W ramach skryptu wykryto wiele reguł sekwencyjnych dotyczących przyczyn problemów zdrowotnych ludzi z cukrzycą,
#   większość z tych reguł zgodna jest z intuicją. Mianowicie, często powtarzającym się schematem w regułach jest
#   obfity posiłek, po którym następują problemy ze zbyt wysokim stężeniem cukru albo niska dawka insuliny dająca
#   podobne efekty. Zdarzenia w tych regułach mogły występować w dużych odstępach czasu, nawet do kilkudziesięciu
#   godzin. Pokazuje to, jak ważne jest przykładanie wagi do regularnego dostarczania odpowiedniej ilości insuliny.
#   Z czwartej serii eksperymentów wynika jednak jednoznacznie, że przyjęcie jej zbyt dużych dawek prowadzi do negatywnych
#   skutków w postaci znacznie obniżonego lub podwyższonego poziomu cukru.
#   Jeśli chodzi o wpływ na poziom glukozy we krwi w krótkim odstępie czasu, to największy wpływ mają aktywność fizyczna i 
#		spożyty posiłek. Co ciekawe więcej jest reguł związanych z aktywnością fizyczną i podwyższoną zawartością glukozy
#		we krwi niż obniżoną, co wynika z Eksperymentu 2 z serii eksperymentów 3. Wpływ posiłku jest oczywisty, ale w 
#		połączeniu z seriami aktywności fizycznej już nie bardzo. W takim przypadku należy badać konkretne sekwencjo-reguły 
#		z serii eksperymentów 3.
#
#		Jeśli chodzi o dokładniejsze wnioski to każdy eksperyment w każdej z serii eksperymentów ma własne wnioski, które są
#		najczęściej związane z konkretnymi sekwencjo-regułami. Bardzo częśto pokrywają się ze wspomnianą przed chwilą intuicją.
#		Jeżeli więc lekarz chciałby zrobić bazę poprzedników dla podwyższonych lub obniżonych poziomów glukozy we krwi, powinien
#		przejrzeć każdy eksperyment z każdej serii i wybrać takie sekwencjo-reguły z najwyższymi współczynnikami i takie,
#		które nie koniecznie składają się z regularnych / przeciętnych poprzedników. Często jednak spotka się
#		z regułami opisanymi w poprzednim akapicie (zgodnymi z intuicja).