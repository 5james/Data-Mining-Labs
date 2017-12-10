# Sprawozdanie z laboratorium 2 
# Jakub Guzek
# 27.11.2017

?Groceries
data(Groceries)
summary(Groceries)
str(Groceries)
inspect(head(Groceries))

###################################################################################################################
#                                                                                                                 #
#   CEL: Reorganizacja artykułów w sklepie, z którego pochodza dane z Groceries                                   #
#   Cel można różnie interpretować, ale wszystkie interpretacje wiaża sie z przesuwaniem produktow po             #
#   polkach sklepowych.                                                                                           #
#   Jedna z interpretacji może być szukanie wszystkich zależnosci miedzy wszystkimi produktami, lecz wedlug mnie  #
#   takie podejscie jest zbyt ogolne i osoba po otrzymaniu takich wynikow mialaby trudnosci w reorganizacji       #
#   artykułów w sklepie. Ja spróbuje podejsc do tego troszke inaczej - po zauwazeniu tego, że produkty składaja   #
#   sie w grupy oraz w jeszcze wieksze grupy (level 2 oraz level 1), można spróbować zreorganizować położenie     #
#   arktykułów w mikro-skali oraz makro-skali. To znaczy odpowiednio wpływ tego, że kupimy produkt z kategorii X  #
#   po zakupie innego artukuły z kategorii X oraz tego, że kupujac dowolny produkt z kategorii X kupimy produkt z #
#   kategorii Y. Kategorie można rozpatrywać na dwóch poziomach - level2 oraz level1.                             #
#   Dla mikro-skali szukamy wiec wszystkich reguł produkt_A_z_kategorii_X => produkt_B_z_kategorii_X, zas dla     #
#   makro-skali szukamy wszystkich reguł produkt_C_z_kategorii_X => produkt_D_z_kategorii_Y.                      #
#   Najbardziej interesuja nas reguły o najwyższym współczynniku wsparcia oraz współczynniku podniesienia.        #
#   Po otrzymaniu wyników w postaci reguł osoba (wlasciciel) bedzie znala powiazanie produktow w pojedynczej      #
#   alejce lub serii alejek, w których sa produkty należace do tej samej kategorii (level2, level1). Bedzie także #
#   znała zaleznosc miedzy konkretnymi kategoriami produktow, czyli to, ktore produkty sa kupowane razem.         #
#   To co można potem zrobić to inna sprawa. Własciciel może zblizyc do siebie takie produkty majac na wzgledzie  #
#   wygode klienta. Moim zdaniem powinien jednak postapic odwrotnie, czyli oddalić od siebie produkty w skali     #
#   mikro oraz poprzestawiac polozenie regałów z produktami konkretnych kategorii w taki sposób, żeby klient      #
#   musiał przejsc kawalek ogladajac w miedzyczasie produkty z innych kategorii z nadzieja, że może jakis produkt #
#   lub promocja produktu skusi go i dokupi produkty ekstra.                                                      #
#                                                                                                                 #
#   W ramach wyjasnien - nie wiem jak sa pogrupowane produkty (level1, level2). Żeby ustalić wielkosciowo:        # 
#   Artukyły sa ustawione na półkach; półki sa w alejce; alejka jest w serii alejek; seria alejek tworzy sklep.   #
###################################################################################################################


#   Na poczatku chwile spedzilem, żeby zebrac wszystkie informacje dotyczace ilosci produktów, kategorii, ich
#   licznosci oraz przynależnosci prodkutow do kategorii
#   Wszystkie produkty:
labelsAll <- Groceries@itemInfo$labels
labelsAll

#   Wszystkie kategorie (level2 - mniejszy podział, czyli wiecej kategorii z mniejsza iloscia artykułów)
labelsLevel2 <- Groceries@itemInfo$level2
labelsLevel2_unique <- unique(labelsLevel2)
labelsLevel2_unique

#   Podział produktów na kategorie (level2) 
tmp_lvl3 = c()
tmp_lvl2 = labelsLevel2[1]
index=1
labelsAll_categorized_by_level2 = c()
for (i in 1:length(labelsAll)) {
  if (labelsLevel2[i] == tmp_lvl2) {
    tmp_lvl3 <- c(tmp_lvl3, labelsAll[i])
    if (i == length(labelsAll)) {
      labelsAll_categorized_by_level2[[index]] <- c(tmp_lvl3)
    }
  }
  else {
    labelsAll_categorized_by_level2[[index]] <- c(tmp_lvl3)
    index = index + 1
    tmp_lvl3 <- c(labelsAll[i])
    tmp_lvl2 = labelsLevel2_unique[index]
  }

}
#labelsAll_categorized_by_level2

#   Wszystkie kategorie (level1 - wiekszy podział, czyli mniej kategorii z wieksza iloscia artykułów) #
labelsLevel1 <- Groceries@itemInfo$level1
labelsLevel1_unique <-unique(labelsLevel1)
labelsLevel1_unique

#   Podział produktów na kategorie (level1)
tmp_lvl3 = c()
tmp_lvl1 = labelsLevel1[1]
index=1
labelsAll_categorized_by_level1 = c()
for (i in 1:length(labelsAll)) {
  if (labelsLevel1[i] == tmp_lvl1) {
    tmp_lvl3 <- c(tmp_lvl3, labelsAll[i])
    if (i == length(labelsAll)) {
      labelsAll_categorized_by_level1[[index]] <- c(tmp_lvl3)
    }
  }
  else {
    labelsAll_categorized_by_level1[[index]] <- c(tmp_lvl3)
    index = index + 1
    tmp_lvl3 <- c(labelsAll[i])
    tmp_lvl1 = labelsLevel1_unique[index]
  }
  
}
#labelsAll_categorized_by_level1


#   Zakładajac, że level2 już zbiera produkty w kategorie, level1 grupuje w jeszcze bardziej  
#   ogólne kategorie. Idac tym tropem można uznać, że level2 to podział artykułów na półki / aleje  
#   sklepowe, a level1 to to podział w sklepie konkretnych grup pułek sklepowych.           
#   Z tego powodu można byłoby spróbować wyróżnić jeszcze jeden podział - wiekszy niż ten w   
#   mikro-skali, ale mniejszy niż ten w makro-skali. Mógłby służyć do przestawiania pułek
#   wzgledem siebie w konkretnej kategorii produktów (level1).
tmp_lvl2 = c()
index=1
tmp_lvl1 = labelsLevel1[index]
labelsLevel2_categorized_by_level1 = c()
for (i in 1:length(labelsLevel2)) {
  if (labelsLevel1[i] == tmp_lvl1) {
    tmp_lvl2 <- c(tmp_lvl2, levels(labelsLevel2)[labelsLevel2[i]])
    if (i == length(labelsLevel2)) {
      labelsLevel2_categorized_by_level1[[index]] <- unique(c(tmp_lvl2))
    }
  }
  else {
    labelsLevel2_categorized_by_level1[[index]] <- unique(c(tmp_lvl2))
    index = index + 1
    tmp_lvl2 <- c(levels(labelsLevel2)[labelsLevel2[i]])
    tmp_lvl1 = labelsLevel1_unique[index]
  }
  
}
remove(tmp_lvl1)
remove(tmp_lvl2)
remove(tmp_lvl3)
remove(index)
remove(i)
#labelsLevel2_categorized_by_level1

#   Tutaj zostaly zebrane wyniki z badań kategorii:
labelsAll_categorized_by_level2
labelsAll_categorized_by_level1
labelsLevel2_categorized_by_level1


#   Funkcja sluzaca znalezieniu listy konkretnych produktow należacych do kategorii (level2)
find_labels_level2_by_level3 <- function(label_level_3) {
  for (i in 1:length(labelsAll_categorized_by_level2)) {
   # for (j in 1:length(labelsAll_categorized_by_level2[[i]])) {
      if (label_level_3 %in% labelsAll_categorized_by_level2[[i]]) {
        return(list(index_i = i, list_of_products = unlist(labelsAll_categorized_by_level2[i])))
      }
    #}
    }
  remove(i)
  remove(j)
}
#print('butter milk' %in% find_labels_level2_by_level3('whole milk')$list_of_products)

#   Funkcja sluzaca znalezieniu listy konkretnych produktow należacych do kategorii (level1)
find_labels_level1_by_level3 <- function(label_level_3) {
  for (i in 1:length(labelsAll_categorized_by_level1)) {
    #for (j in 1:length(labelsAll_categorized_by_level1[i])) {
      if (label_level_3 %in% labelsAll_categorized_by_level1[[i]]) {
        return(list(index_i = i, list_of_products = unlist(labelsAll_categorized_by_level1[i])))
      }
    #}
  }
  remove(i)
  remove(j)
}
#print('butter milk' %in% find_labels_level1_by_level3('whole milk')$list_of_products)

#   SERIA EKSPERYMENTÓW 1
#   Tak jak napisałem we wstepie - na samym poczatku chcialbym znalezc zaleznosci na poziomie konkretnych produktow.
#   Oczywiscie produkty pochodza z tej samej grupy (level2).
#   W tym celu przeprowadze eksperyment - wygeneruje wszystkie reguly "produkt_A_kategoria_X => produkt_B_kategoria_X"
#   za pomoca algorytmu apriori, a nastepnie przejrze wszystkie wygenerowane w ten sposob reguly i bede sprawdzal, czy
#   należa do tej samej grupy - jesli tak, to taka regula jest dodawana.
#   Tworzac ten skrypt zauważyłem, że sa tutaj ustalane lhs oraz rhs na poziomie level2. Biorac to pod uwage, mozna
#   pokusic sie stworzenie zaleznosci miedzy produktami z innych kategorii na poziomie level2. Trzeba je jednak w 
#   jakis sposob posegregowac. Można byloby stworzyc n = length(labelsLevel2_unique) zmiennych nazwanych po konkretnych
#   kategoriach i w nich przechowywac reguły, ale wedlug mnie lepszym rozwiazaniem bedzie stworzenie dwuwymiarowej tablicy.
#   Oczywiscie tablica taka na jednej przekatnej bedzie pusta (lista reguł "produkt_A_kategoria_X => produkt_B_kategoria_X")
#   oraz bedzie symetryczna wzgledem tej przekatnej.
#   Eksperyment ten w sumie bedzie miał dwa wyniki - powiazanie produktów w skali jednej alei oraz powiazanie alei w skali sklepu.
#   Na poczatku spróbuje przeprowadzic eksperyment dla wspolczynnika wsparcia = 0.02 oraz wspolczynnika zaufania = 0.1
productsLevel3_by_level2 <- apriori(Groceries, parameter = list(support=0.02, confidence = 0.10, minlen = 2, maxlen = 2, target = "rules"))
productsLevel3_by_level2_same_category_index = c()
productsLevel3_by_level2_other_category_index <- matrix(list(), nrow=length(labelsLevel2_unique), ncol=length(labelsLevel2_unique))
for (tmp in seq(1, length(productsLevel3_by_level2), by=2)) {
  
  lhs <- labelsAll[productsLevel3_by_level2[tmp]@lhs@data@i+1]
  lhs_research <- find_labels_level2_by_level3(lhs)
  lhs_level2 <- lhs_research$list_of_products
  lhs_index <- lhs_research$index_i
  rhs <- labelsAll[productsLevel3_by_level2[tmp]@rhs@data@i+1]
  rhs_research <- find_labels_level2_by_level3(rhs)
  rhs_level2 <- rhs_research$list_of_products
  rhs_index <- rhs_research$index_i
  
  if (rhs %in% lhs_level2 | lhs %in% rhs_level2) {
    productsLevel3_by_level2_same_category_index <- c(productsLevel3_by_level2_same_category_index, tmp)
  }
  else {
    if (is.null(productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]]) 
        | is.null(productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]])) {
      productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]] <- productsLevel3_by_level2[tmp]
      productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]] <- productsLevel3_by_level2[tmp]
      
    }
    productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]] <- union(productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]], productsLevel3_by_level2[tmp])
    productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]] <- union(productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]], productsLevel3_by_level2[tmp])
  }
}
remove(lhs)
remove(lhs_level2)
remove(lhs_research)
remove(lhs_index)
remove(rhs)
remove(rhs_level2)
remove(rhs_research)
remove(rhs_index)
remove(tmp)
productsLevel3_by_level2_same_category <- productsLevel3_by_level2[productsLevel3_by_level2_same_category_index]
remove(productsLevel3_by_level2_same_category_index)
#   Koniec eksperymentu. Wyniki:
#   Powiazane produkty na mikro-skali na tych samych półkach / alejach (na tym samym level2).
inspect(sort(productsLevel3_by_level2_same_category, by='support'))
length(productsLevel3_by_level2_same_category)

#   Powiazanie miedzy produktami z innych kategorii (półek/alei).
print(labelsLevel2_unique)
productsLevel3_by_level2_other_category_index
productsLevel3_by_level2_other_category_index[[1,7]]

#   Wygenerowano w ten sposob jedynie 9 reguł. Ta liczba jest zbyt mała.
#   Eksperyment należy powtórzyć dla wiekszej liczby wygenerowanych reguł. To oznacza, że
#   trzeba zmniejszyć współczynnik wsparcia do 0.005 oraz zaufania do 0.05.
#   UWAGA: skrypt może sie chwile wykonywac ze wzgledu na duża liczbe wygenerowanych reguł,
#   ponieważ badamy każda regułe z osobna
productsLevel3_by_level2 <- apriori(Groceries, parameter = list(support=0.005, confidence = 0.05, minlen = 2, maxlen = 2, target = "rules"))
productsLevel3_by_level2_same_category_index = c()
productsLevel3_by_level2_other_category_index <- matrix(list(), nrow=length(labelsLevel2_unique), ncol=length(labelsLevel2_unique))
for (tmp in seq(1, length(productsLevel3_by_level2), by=2)) {
  
  lhs <- labelsAll[productsLevel3_by_level2[tmp]@lhs@data@i+1]
  lhs_research <- find_labels_level2_by_level3(lhs)
  lhs_level2 <- lhs_research$list_of_products
  lhs_index <- lhs_research$index_i
  rhs <- labelsAll[productsLevel3_by_level2[tmp]@rhs@data@i+1]
  rhs_research <- find_labels_level2_by_level3(rhs)
  rhs_level2 <- rhs_research$list_of_products
  rhs_index <- rhs_research$index_i
  
  if (rhs %in% lhs_level2 | lhs %in% rhs_level2) {
    productsLevel3_by_level2_same_category_index <- c(productsLevel3_by_level2_same_category_index, tmp)
  }
  else {
    if (is.null(productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]]) 
        | is.null(productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]])) {
      productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]] <- productsLevel3_by_level2[tmp]
      productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]] <- productsLevel3_by_level2[tmp]
      
    }
    productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]] <- union(productsLevel3_by_level2_other_category_index[[lhs_index, rhs_index]], productsLevel3_by_level2[tmp])
    productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]] <- union(productsLevel3_by_level2_other_category_index[[rhs_index, lhs_index]], productsLevel3_by_level2[tmp])
  }
}
remove(lhs)
remove(lhs_level2)
remove(lhs_research)
remove(lhs_index)
remove(rhs)
remove(rhs_level2)
remove(rhs_research)
remove(rhs_index)
remove(tmp)
productsLevel3_by_level2_same_category <- productsLevel3_by_level2[productsLevel3_by_level2_same_category_index]
remove(productsLevel3_by_level2_same_category_index)
#   Koniec eksperymentu. Wyniki:
#   Powiazane produkty na mikro-skali na tych samych półkach / alejach (na tym samym level2).
inspect(sort(productsLevel3_by_level2_same_category, by='support'))
length(productsLevel3_by_level2_same_category)

#   Powiazanie miedzy produktami z innych kategorii (półek/alei).
print(labelsLevel2_unique)
productsLevel3_by_level2_other_category_index
productsLevel3_by_level2_other_category_index[[1,7]]

#   Eksperyment można byłoby wykonać dla jeszcze mniejszego wsparcia (zaufania lepiej już nie zmniejszać) na przykład 0.003.
productsLevel3_by_level2 <- apriori(Groceries, parameter = list(support=0.003, confidence = 0.05, minlen = 2, maxlen = 2, target = "rules"))
#   Wtedy liczba wygenerowanych reguł zwiekszylaby sie prawie dwukrotnie (z lekko ponad 1000 do prawie 1900),
#   ale zdecydowałem sie go tutaj nie zamieszczac ze wzgledu na czas jego wykonywania.
#   Powyzszy skrypt wygenerował 37 reguł, które mówia o czestym wystepowaniu produktu A i produktu B (jesli wygenerowana 
#   zostala regula produkt_A => produkt_B) w jednej liscie zakupowej. W regulach o najwyzszych wsparciach czesto 
#   wystepuje produkt 'whole milk' oraz 'tropical fruit'. Wszystkie z wymienionych reguł sa wedlug mnie wazne,
#   ale jesli wystapilby konflikt, to należy wybrać regułe z wiekszym wsparciem, a jesli wsparcie bedzie podobne
#   to patrzec na zaufanie.
inspect(sort(productsLevel3_by_level2_same_category, by='confidence'))
#   Reguly zawierajace produkty 'whole milk'  maja takze bardzo wysoki wspoczynnik zaufania. W momencie, kiedy
#   wlasciciel sklepu chce ''isc na reke'' klientowi i zblizyc do siebie produkty nie ma problemu, bo wszystkie
#   te produkty beda obok siebie. Problem wystepuje kiedy wlasciciel wybiera druga opcje, czyli oddalenie od siebie
#   tych produktów. To by oznaczało, że 'whole milk' musiałoby być na srodku półki / alejki, a pozostałe artykuły
#   na ich krańcach. Przykładowo dla 'whole milk' wystepuja nastepujace reguły:
#   1081 {yogurt}                => {whole milk}         0.056024403 0.40160350 1.5717351
#   931  {whipped/sour cream}    => {whole milk}         0.032231825 0.44964539 1.7597542
#   815  {butter}                => {whole milk}         0.027554652 0.49724771 1.9460530
#   561  {curd}                  => {whole milk}         0.026131164 0.49045802 1.9194805
#   289  {dessert}               => {whole milk}         0.013726487 0.36986301 1.4475140
#   Jest ich wiecej niz 3, a to znaczy, że może nastapic konflikt - ktores z tych produktow także moga byc
#   w liscie ''czestych zakupow''. Sprawdzenia dokonam dla pierwszego produktu - 'yogurt':
# 1081 {yogurt}                => {whole milk}         0.056024403 0.40160350 1.5717351
# 925  {whipped/sour cream}    => {yogurt}             0.020742247 0.28936170 2.0742510
# 555  {curd}                  => {yogurt}             0.017285206 0.32442748 2.3256154
# 809  {butter}                => {yogurt}             0.014641586 0.26422018 1.8940273
# 285  {yogurt}                => {dessert}            0.009862735 0.07069971 1.9050182
# 85   {butter milk}           => {yogurt}             0.008540925 0.30545455 2.1896104
# 19   {beverages}             => {yogurt}             0.005490595 0.21093750 1.5120775
#   Nie inaczej. Wystepuje w tych samych listach zakupów z 'butter', 'curd', 'dessert' oraz
#   'whipped/sour cream', czyli własciwie wszystkimi. Trzeba jednak zauwazyć, że wsparcie
#   tych reguł jest znacznie mniejsze. Dlatego strategia z umiejscowieniem takiego produktu
#   w centrum półki / alejki jest nadal akutalna. Jedynie pozostałe artykuły należy 
#   podzielic na 2 grupy tak, aby wsparcie obu grup było mniej wiecej zbliżone,
#   ale jednoczesnie maksymalizujac sumaryczne wsparcie (biorac je z reguł)
#   miedzy regułami w pierwszej grupie, a regułami w drugiej grupie.
#
#   Jesli chodzi o zmienna 'productsLevel3_by_level2_other_category_index' zawierajaca reguly, ktore
#   mowia, ze czesto przy zakupie produktu z jednej półki osoba decyduje sie na zakup produktu z 
#   drugiej polki, to także można przyjac dwie strategie analizowania tych wyników: jakosciowa
#   oraz ilosciowa. Przy tej pierwszej (jakosciowej) nalezaloby badac poszczególne kategorie produktow.
#   To znaczy, że dla każdej kolumny trzebaby było zbadać reguły z każdego rzedu. Dla wlasciciela sklepu
#   takie badanie byłoby na pewno bardziej wartosciowe, ale można też założyć, że każda reguła jest 
#   równa tyle samo. Wtedy można badać jedynie ilosc reguł w danej kolumnie dla każdego rzedu.
#   W tym celu można przekształcić zmienna 'productsLevel3_by_level2_other_category_index' na postać liczbowa:
productsLevel3_by_level2_other_category_numbers  <- matrix(0, nrow=length(labelsLevel2_unique), ncol=length(labelsLevel2_unique))
for (i in 1:length(labelsLevel2_unique)) {
  for (j in 1:length(labelsLevel2_unique)) {
    productsLevel3_by_level2_other_category_numbers[[i, j]] <- length(productsLevel3_by_level2_other_category_index[[i, j]])
  }
}
print(labelsLevel2_unique)
productsLevel3_by_level2_other_category_numbers
#   W ten sposób, dzieki ilosciowemu podjesciu, mamy czysty obraz - które produkty sa kupowane z innymi.
#   Idac dalej, jesli wlasciciel bedzie chciał, żeby klient chodził po sklepie po swoje produkty,
#   powinien on odsunac od siebie półki / aleje, które sa na przecieciu najwyzszych liczb w 
#   powyzszej macierzy. Lub tez zbliżyć w przeciwnym wypadku.
#   Macierz należy czytac przy pomocy zmiennej 'labelsLevel2_unique'. Ponadto, macierz jest symetryczna
#   wzgledem prostej od [[1,1]] do [[55,55]], dla której wszystkie wartosci sa rowne 0.
#   Przykładowo dla powyższego eksperymentu: najwiecej reguł jest w rzedzie:
print(rowSums(productsLevel3_by_level2_other_category_numbers))
#   9 (czyli dairy produce), a nastepnie z podobnych wynikiem rzad 7 (czyli vegetables).
#   Badajac rzad 9 można zauważyć, że:
productsLevel3_by_level2_other_category_numbers[9,]
#   czesto (powyzej 2) produkty z tej kategorii sa kupowane z produktami z kategorii-6 (fruit - 5 reguł), 
#   7 (vegetables - 6 reguł) - oraz 32 (non-alc. drinks - 5 reguł). 
#   Badajac te kategorie mozna wysunac wnioski, że należy odsunac od dairy produce (przetwory mleczne)
#   regały / aleje zawierajace nastepujace rodzaje produktów: owoce, warzywa oraz napoje bezalkoholowe.
#   W ten sposób można badać każda aleje / półke z artykułami.
#   Przy wiekszej liczbie wygenerowanych reguł może wystapic podobna sytuacja do tej z rozmieszczaniem
#   poszczególnych artykułów na jedej półce / w jednej alei. Tutaj także można można próbować
#   maksymalizować funkcje(odleglosc, ilosc reguł).
#   Ponadto, podejscie ilosciowe można próbować ulepszyć wprowadzajac inny element zliczajacy.
#   Zamiast każda regułe traktowac tak samo można zliczać (sumować) wsparcie albo zaufanie reguł, a nawet 
#   stworzyć jakas funkcje od wspracia i zaufania i dodawać jej wartosc.


#   SERIA EKSPERYMENTÓW 2
#   Kolejna seria eksperymentów bedzie podobna do poprzedniej, z taka różnica, że kategorie, na jakie beda
#   podzielone produkty beda bardziej obszerne (bedzie ich mniej, ale beda bardziej liczne) - level1.
#   To już nie bedzie pojedyncza półka / aleja, tylko aleja / seria alei.
#   W tej serii eksperymentów takze otrzymam dwa wyniki. Pierwszym bedzie zaleznosc kupienia jednego 
#   produktu z tej kategorii przy okazji zakupu innego produktu w tej samej kategorii (na poziomie level1).
#   Ten wynik bedzie zawierał w sobie wynik z poprzedniej serii eksperymentu. Jednak na tej czesci wyniku
#   nie bedzie mi zalezec, gdyz jest on badany juz w poprzedniej serii eksperymentow. Bardziej zalezy
#   mi na zbiorze tych reguł, które nie wchodza w sklad poprzedniego eksperymentu, dlatego nalezy wykonac
#   operacje odejmowania zbiorow. Ta czesc wyniku pozwoli okreslic zależnosci panujace miedzy produktami
#   w konkretnych kategoriach na poziomie level1. Bedzie można w ten sposób przysunac lub odsunać (w 
#   zaleznosci od intencji wlasciciela) konkretne produkty w alei / serii alei. Ten wynik może zastapic
#   eksperymenty badajace zaleznosci położenia półek / alei (kategorii level2) w odniesieniu do
#   alei / serii alei (kategoria level1). Może, lecz nie musi, gdyż tutaj mamy eksperyment na poziomie
#   konkretnych produktów, ale przesuwajac konkretne produkty musimy przesuwac takze cala grupe 
#   artykułów (półke / aleje - level2) w przestrzeni wiekszej grupy - kategorii (level1).
#   Druga czesc wyniku takze bedzie podobna do drugiej czesci wyniku z poprzedniej serii eksperymentow.
#   Bedzie ona jednak bardziej uogólniona, gdyż kategorii jest mniej, ale sa bardziej liczne.
#   W sumie to reorganizujac sklep można byłoby najpierw ustawic wszystkie aleje / serie alei (level1)
#   dzieki wynikow z tej serii eksperymentów. Nastepnie przeorganizować położenie półek / alei wewnatrz
#   tych grup (za pomoca pierwszej czesci wynikow), a nastepnie ustawic produkty na półkach / alejach
#   (dzieki pierwszej czesci wynikow z pierwszej serii eksperymentow).
#   
productsLevel3_by_level1 <- apriori(Groceries, parameter = list(support=0.015, confidence = 0.10, minlen = 2, maxlen = 2, target = "rules"))
productsLevel3_by_level1_same_category_index = c()
productsLevel3_by_level1_other_category_index <- matrix(list(), nrow=length(labelsLevel1_unique), ncol=length(labelsLevel1_unique))
for (tmp in seq(1, length(productsLevel3_by_level1), by=2)) {
  
  lhs <- labelsAll[productsLevel3_by_level1[tmp]@lhs@data@i+1]
  lhs_research <- find_labels_level1_by_level3(lhs)
  lhs_level1 <- lhs_research$list_of_products
  lhs_index <- lhs_research$index_i
  rhs <- labelsAll[productsLevel3_by_level1[tmp]@rhs@data@i+1]
  rhs_research <- find_labels_level1_by_level3(rhs)
  rhs_level1 <- rhs_research$list_of_products
  rhs_index <- rhs_research$index_i
  
  if (rhs %in% lhs_level1 | lhs %in% rhs_level1) {
    productsLevel3_by_level1_same_category_index <- c(productsLevel3_by_level1_same_category_index, tmp)
  }
  else {
    if (is.null(productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]]) 
        | is.null(productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]])) {
      productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]] <- productsLevel3_by_level1[tmp]
      productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]] <- productsLevel3_by_level1[tmp]
      
    }
    productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]] <- union(productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]], productsLevel3_by_level1[tmp])
    productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]] <- union(productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]], productsLevel3_by_level1[tmp])
  }
}
remove(lhs)
remove(lhs_level1)
remove(lhs_research)
remove(lhs_index)
remove(rhs)
remove(rhs_level1)
remove(rhs_research)
remove(rhs_index)
remove(tmp)
productsLevel3_by_level1_same_category <- productsLevel3_by_level1[productsLevel3_by_level1_same_category_index]
remove(productsLevel3_by_level1_same_category_index)

#   Teraz musze od pierwszej czesci wynikow ('productsLevel3_by_level1_same_category') odjac wyniki z 
#   'productsLevel3_by_level2_same_category'. Ponadto musze odjac wyniki symetryczne do 
#   'productsLevel3_by_level1_same_category' , czyli zamienic kolejnoscia lhs oraz rhs.
productsLevel3_by_level2_same_category_tmp <- productsLevel3_by_level2_same_category
tmp <- productsLevel3_by_level2_same_category_tmp@lhs
productsLevel3_by_level2_same_category_tmp@lhs <- productsLevel3_by_level2_same_category_tmp@rhs
productsLevel3_by_level2_same_category_tmp@rhs <- tmp
remove(tmp)
productsLevel3_by_level2_same_category_tmp <- union(productsLevel3_by_level2_same_category_tmp, productsLevel3_by_level2_same_category)
productsLevel3_by_level1_same_category <- setdiff(productsLevel3_by_level1_same_category, productsLevel3_by_level2_same_category_tmp)
#   Koniec eksperymentu. Wyniki:
#   Powiazanie produktow w grupie na poziomie level1
inspect(sort(productsLevel3_by_level1_same_category, by='support'))
length(productsLevel3_by_level1_same_category)
#   Jest ich lacznie 15. Nie jest to  zadowalajaca liczba na skali calego sklepu, dlatego
#   wykonam jeszcze jeden eksperyment, ale tym razem z mniejszym minimalnym wsparciem.

#   Powiazanie miedzy produktami z innych kategorii (alei/seria alei).
print(labelsLevel1_unique)
productsLevel3_by_level1_other_category_index
#   Nie analizuje narazie tego wyniku, gdyż bede miał zestaw dla wiekszej liczby reguł

#   Kolejny eksperyment z tej serii. Zmniejszam wsparcie do 0.005 oraz zaufanie do 0.05, gdyż w poprzednim 
#   eksperymencie z tej serii eksperymentów otrzymalem jedynie 15 regul "miedzygrupowych".
#   UWAGA: skrypt może sie chwile wykonywac ze wzgledu na duża liczbe wygenerowanych reguł,
#   ponieważ badamy każda regułe z osobna.
productsLevel3_by_level1 <- apriori(Groceries, parameter = list(support=0.005, confidence = 0.05, minlen = 2, maxlen = 2, target = "rules"))
productsLevel3_by_level1_same_category_index = c()
productsLevel3_by_level1_other_category_index <- matrix(list(), nrow=length(labelsLevel1_unique), ncol=length(labelsLevel1_unique))
for (tmp in seq(1, length(productsLevel3_by_level1), by=2)) {
  
  lhs <- labelsAll[productsLevel3_by_level1[tmp]@lhs@data@i+1]
  lhs_research <- find_labels_level1_by_level3(lhs)
  lhs_level1 <- lhs_research$list_of_products
  lhs_index <- lhs_research$index_i
  rhs <- labelsAll[productsLevel3_by_level1[tmp]@rhs@data@i+1]
  rhs_research <- find_labels_level1_by_level3(rhs)
  rhs_level1 <- rhs_research$list_of_products
  rhs_index <- rhs_research$index_i
  
  if (rhs %in% lhs_level1 | lhs %in% rhs_level1) {
    productsLevel3_by_level1_same_category_index <- c(productsLevel3_by_level1_same_category_index, tmp)
  }
  else {
    if (is.null(productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]]) 
        | is.null(productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]])) {
      productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]] <- productsLevel3_by_level1[tmp]
      productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]] <- productsLevel3_by_level1[tmp]
      
    }
    productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]] <- union(productsLevel3_by_level1_other_category_index[[lhs_index, rhs_index]], productsLevel3_by_level1[tmp])
    productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]] <- union(productsLevel3_by_level1_other_category_index[[rhs_index, lhs_index]], productsLevel3_by_level1[tmp])
  }
}
remove(lhs)
remove(lhs_level1)
remove(lhs_research)
remove(lhs_index)
remove(rhs)
remove(rhs_level1)
remove(rhs_research)
remove(rhs_index)
remove(tmp)
productsLevel3_by_level1_same_category <- productsLevel3_by_level1[productsLevel3_by_level1_same_category_index]
remove(productsLevel3_by_level1_same_category_index)

#   Teraz musze od pierwszej czesci wynikow ('productsLevel3_by_level1_same_category') odjac wyniki z 
#   'productsLevel3_by_level2_same_category'. Ponadto musze odjac wyniki symetryczne do 
#   'productsLevel3_by_level1_same_category' , czyli zamienic kolejnoscia lhs oraz rhs.
productsLevel3_by_level2_same_category_tmp <- productsLevel3_by_level2_same_category
tmp <- productsLevel3_by_level2_same_category_tmp@lhs
productsLevel3_by_level2_same_category_tmp@lhs <- productsLevel3_by_level2_same_category_tmp@rhs
productsLevel3_by_level2_same_category_tmp@rhs <- tmp
remove(tmp)
productsLevel3_by_level2_same_category_tmp <- union(productsLevel3_by_level2_same_category_tmp, productsLevel3_by_level2_same_category)
productsLevel3_by_level1_same_category <- setdiff(productsLevel3_by_level1_same_category, productsLevel3_by_level2_same_category_tmp)
#   Koniec eksperymentu. Wyniki:
#   Powiazanie produktow w grupie na poziomie level1
inspect(sort(productsLevel3_by_level1_same_category, by='support'))
length(productsLevel3_by_level1_same_category)
#   Ta liczba - 80 - reguł jest już zadowalajaca. Dla 169 produktów w 10 grupach można już jakos 
#   sensownie je poukładać. Oczywiscie można byłoby powtorzyc eksperyment dla jeszcze mniejszego
#   wsparcia i jeszcze mniejszygo zaufania, ale wtedy skrypt wykonywalby sie bardzo dlugo, a 
#   majac na uwadze, że nie zależy nam aż na takiej szczegółowosci mozna pominac generowanie 
#   kolejnego eksperymentu w tej serii eksperymentów.
#   W tym eksperymencie (jak i w poprzednim, z poprzedniej serii eksperymentów) w regułach o
#   najwiekszym wsparciu wystepuja produkty 'whole milk'.
#   Tutaj także reakcja na wyniki może być podobna do tej z poprzedniej serii eksperymentów,
#   czyli badajac konkretna grupe (na poziomie level1) szukamy produktu, ktory albo ma 
#   najwieksze wsparcie sumaryczne (zliczajac ze wszystkich regul w jakich sie pojawia), albo
#   po prostu najczesciej sie pojawia (lepszy chyba bedzie wariant pierwszy). Nastepnie
#   umiescic go w centrum (mniej wiecej) i utworzyc dwie grupy o mniej wiecej podobnym 
#   sumarycznym wsparciu (zliczajac z regul). Tak jak w poprzedniej serii eksperymentów
#   moga wystepowac konflikty w "czestej liscie zakupow" na przykładzie:
#   'whole milk' jest czesto kupowany z 'rolls/buns'. Zakladamy, że 'whole milk' jest 
#   najczesciej kupowanym produktem w tej grupie (na poziomie level1). Wtedy umiejscowiony
#   jest w centrum, a 'rolls/buns' jest w jednej z grup, ale wtedy 'rolls/buns' także może
#   być przez to umiejscowiony obok ktoregos z produktow (półki produktów), z którym także 
#   czesto jest kupowany (zakladajac, że chcemy jak najwiekszej przestrzeni dla produtkow
#   ktore sa czesto kupowane razem). 
# 1085 {rolls/buns}            => {whole milk}               0.056634469 0.30790492 1.2050318
# 977  {pastry}                => {whole milk}               0.033248602 0.37371429 1.4625865
# 877  {domestic eggs}         => {whole milk}               0.029994916 0.47275641 1.8502027
# 747  {brown bread}           => {whole milk}               0.025216065 0.38871473 1.5212930
# 491  {frozen vegetables}     => {whole milk}               0.020437214 0.42494715 1.6630940
# 393  {whole milk}            => {white bread}              0.017081851 0.06685237 1.5881474
# 333  {cream cheese }         => {whole milk}               0.016471784 0.41538462 1.6256696
# 117  {sliced cheese}         => {whole milk}               0.010777834 0.43983402 1.7213560
# 79   {hard cheese}           => {whole milk}               0.010066090 0.41078838 1.6076815
# 71   {frozen meals}          => {whole milk}               0.009862735 0.34767025 1.3606593
# 37   {soft cheese}           => {whole milk}               0.007524148 0.44047619 1.7238692
# 25   {ice cream}             => {whole milk}               0.005897306 0.23577236 0.9227303

# 1085 {rolls/buns}            => {whole milk}               0.056634469 0.30790492 1.2050318
# 1077 {yogurt}                => {rolls/buns}               0.034367056 0.24635569 1.3393633
# 873  {domestic eggs}         => {rolls/buns}               0.015658363 0.24679487 1.3417510
# 927  {whipped/sour cream}    => {rolls/buns}               0.014641586 0.20425532 1.1104760
# 811  {butter}                => {rolls/buns}               0.013421454 0.24220183 1.3167800
# 487  {frozen vegetables}     => {rolls/buns}               0.010167768 0.21141649 1.1494092
# 557  {curd}                  => {rolls/buns}               0.010066090 0.18893130 1.0271638
# 329  {cream cheese }         => {rolls/buns}               0.009964413 0.25128205 1.3661465
# 87   {butter milk}           => {rolls/buns}               0.007625826 0.27272727 1.4827378
# 115  {sliced cheese}         => {rolls/buns}               0.007625826 0.31120332 1.6919208
# 123  {UHT-milk}              => {rolls/buns}               0.006405694 0.19148936 1.0410712
# 77   {hard cheese}           => {rolls/buns}               0.005897306 0.24066390 1.3084187
# 35   {soft cheese}           => {rolls/buns}               0.005388917 0.31547619 1.7151511
#   Taka sytuacja wystepuje miedzy 'whole milk', 'rolls/buns', 'domestic eggs'i np 'sliced cheese'.
#   Dlatego tutaj takze własciciel może zastosowac funkcje maksymalizujaca
#   sumaryczne wsparcie (/ zaufanie / funkcje od wsparcia i zaufania i odleglosci)
#   reguł produktów, od których sa oddalone.
#   Trzeba pamietać, że  przestawiamy nie konkretne produkty na półkach, ale artykuły na 
#   różnych półkach / alejach,  czyli w efekcie bedziemy przesuwac całe półki / aleje w  
#   ramach przestrzeni grupy alei / serii alei (level1). 


#   Powiazanie miedzy produktami z innych kategorii (alei/seria alei).
print(labelsLevel1_unique)
productsLevel3_by_level1_other_category_index
#   Sytuacja ma sie podobnie do tej z poprzedniej serii eksperymentu, z taka roznica, że
#   teraz przesuwamy cale aleje / serie alei. Mechanizm badania jest prawie taki sam, dlatego
#   nie bede sie powtarzal.
#   Tutaj także przedstawie prostszy wariant, czyli po prostu zliczanie ilosci reguł traktujac wszystkie rowno.
productsLevel3_by_level1_other_category_numbers  <- matrix(0, nrow=length(labelsLevel1_unique), ncol=length(labelsLevel1_unique))
for (i in 1:length(labelsLevel1_unique)) {
  for (j in 1:length(labelsLevel1_unique)) {
    productsLevel3_by_level1_other_category_numbers[[i, j]] <- length(productsLevel3_by_level1_other_category_index[[i, j]])
  }
}
print(labelsLevel1_unique)
productsLevel3_by_level1_other_category_numbers
print(rowSums(productsLevel3_by_level1_other_category_numbers))
#   Najczestciej dodatkowym produktem sa elementy z rzedu 3, czyli 'fresh products'.
#   Je trzeba ustawic w centrum sklepu.
productsLevel3_by_level1_other_category_numbers[3,]
#   Najbardziej oddalonymi grupami produktow powinny byc produkty z rzedow: 2 (fruit and vegetables),
#   1 (meet and sausage) oraz 6 (drinks).   

#   Serii eksperymentów, których wynikiem byłoby rozmieszczenie alejek (level2) w wiekszych 
#   kategoriach (level1) nie  zamisciłem ze wzgledu na jego objetosc obliczeń, ponieważ dla każdej 
#   reguły do tej pory musiałemprzypisywać albo kategorie z level2 albo kategorie z level1. 
#   Bardzo dobra alternatywa dla tej serii eksperymentów sa pierwsza czesc wynikow z drugiej serii
#   eksperymentow oraz druga czesc wynikow z pierwszej serii eksperymentow.


#   Jesli wlasciciel duzego sklepu na prawde chcialby dobrze rozmiescic towar na polkach (przy tej bazie danych)
#   to musialby jeszcze zmniejszyc wsparcie (zaufanie moze zostac, gdyz i tak jest już wystarczajaco niskie) dla
#   obu serii eksperymentów. Nastepnie poukładac serie alejek wg drugiego wyniku z drugiej serii eksperymentów.
#   Kolejno posiłkujac sie pierwsza czescia wynikow z drugiej serii eksperymentow oraz druga czescia wynikow
#   z pierwszej serii eksperymentow spróbowac porozstawiac półki / aleje w alejach / serii alejek. Na samym
#   koncu poustawiac towar w półkach korzystajac z pierwszej czesci wynikow z pierwszej serii eksperymentów.
#   Ponadto tam, gdzie można używać dokładniejszej metody (nie zliczajacej, a jakiejs funkcji).
