#   Laboratorium 7 - grupowanie

#   Autor: Jakub Guzek

#   Zbiorem analizowanym w ramach tego laboratorium będzie zbiór pokemonów.
#   https://www.kaggle.com/abcsds/pokemon
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/Pokemon.csv','Pokemon.csv')

#   Każdy pokemon jest scharakteryzowany następującymi atrybutami:
#   - X. -> numer katalogowy pokemona (id)
#   - Name -> nazwa pokemona
#   - Type.1 -> typ pokemona
#   - Type.2 -> typ pokemona; opcjonalny - w przypadku kiedy pokemon
#     jest więcej niż jednym typem pokemona (np. Charizard jest ognisty i latający)
#   - Total -> suma wszystkich atrybutów odnoszących się do "siły" pokemona
#   - HP -> ilość punktów życia (ile obrażeń może otrzymać przed zemdleniem)
#   - Attack -> bazowy modyfikator dla podstawowych (zwykłych) ataków
#   - Defense -> bazowy modyfikator odporności na podstawowe (zwykłe) ataki
#   - SP..Atk -> bazowy modyfikator dla specjalnych ataków
#   - Sp..Def -> bazowy modyfikator odporności na specjalne ataki
#   - Speed -> szybkość pokemona, określa, który pierwszy zada cios
#   - Generation -> generacja pokemona
#   - Legendary -> stwierdzenie, czy pokemon jest legendarny

#   Cel:
#   Z podanego zbioru pokemonów utworzyć najlepsze grupowania.
#   Grupwanie należy ocenić wg następujących kryteriów:
#   1. Łączej wewnątrzgrupowej sumy kwadratów odległości punktów od środka grupy 
#   (im mniejsza tym lepsza),
#   2. Czystości grup - średnia liczba typów pokemonów w grupie (typ jest reprezentowany
#   w grupie jeśli ma przynajmniej trzech przedstawicieli).
#   W obu przypadkach im mniej grup typ lepiej. 

pokemon <- read.csv("Pokemon.csv")
#   Będę chciał usunąć typy i inne atrybuty ze zmiennej pokemon, dlatego
#   w celu sprawdzania w przyszłości, czy grupowanie ma duży związek z 
#   typami pokemonów
pokemon_original_copy <- pokemon

#   Na samym początku wstępnego przetwarzania zbioru pokemonów
#   należy usunąć typy pokemonów (type.1 i type.2) po to, aby
#   nie wpływały na utworzone grupy.
pokemon$Type.1 = NULL
pokemon$Type.2 = NULL

#   Następnym krokiem będzie usunięcie imion pokemonów oraz 
#   ich numer katalogowy (id). Te dane nie pokazują nam nic
#   co pomogłoby rozróżnić pokemony, gdyż dla każdego pokemona
#   są one unikatowe (oczywiście niektóre pokemony są wymienione
#   więcej niż jeden raz - rodzaj żeński, rodzaj męski i 
#   chyba? nieokreślony)
pokemon$Name = NULL
pokemon$X. = NULL

#   W tym miejscu należy się się zastanowić co zrobić jeszcze z 
#   dwoma atrybutami, a mianowicie Generacji pokemona i 
#   atrybut mówiący o legendarności pokemona.
#   Na chwilę obecną zostawiam oba te atrybuty.

#   Jeśli chcę korzystać z atrybutu 'Legendary', to muszę go 
#   sprowadzić do postaci boolean - w tym momencie są to stringi
#   'True' albo 'False' i algorytm kmeans wyrzuca błędy.
#   W tym celu utworzę nowy atrybut 'Legendary_boolean'.
pokemon$Legendary_boolean = FALSE
for (tmp in 1:dim(pokemon)[1]) {
  if(pokemon[tmp,]$Legendary == "True") {
    pokemon[tmp,10] <- TRUE
  }
  else   if(pokemon[tmp,]$Legendary == "False") {
    pokemon[tmp, 10] <- FALSE
  }
}
remove(tmp)

#   Po tej operacji mogę się pozbyć starego atrybutu 'Legendary'
pokemon$Legendary = NULL

#   Ustawiam seed tak, aby eksperymenty były powtarzalne
set.seed(8)

#   Zanim zacznę serię eksperymentów warto także wydzielić 
#   wszystkie typy pokemonów:
unique_pokemons <-  list(pokemon_original_copy$Type.1)
unique_pokemons <- unique(unlist(unique_pokemons))
unique_pokemons_length <- length(unique_pokemons)


#####################################################################################
#
#   Seria eksperymentów 1
#   Ta seria eksperymentów jest wykonywana dla miary: Łączej wewnątrzgrupowej
#   sumy kwadratów odległości punktów od środka grupy (im mniejsza tym lepsza).
#   Do tych eksperymentów najbardziej odpowiednim algorytmem zdaje się być 
#   algorytm k-średnich (kmeans), ponieważ ze swojej natury dąży do tego, aby
#   zminimalizować łączną wewnątrzgrupową sumę kwadratów odległości (the
#   within-cluster sum of squares - WCSS).
#   Można przewidywać, iż wraz ze zwiększoną liczbą grup zmniejszać się będzie WCSS
#
#####################################
#   Eksperyment 1
#   Pierwszy eksperyment, bardzo podstawowy, aby tylko zestawić "szkielet" eksperymentu.
#   Na początku podzielę zbiór pokemonów jedynie na dwie grupy   
pokemon.kmeans = kmeans(pokemon, 2, iter.max = 20)

#   tabela podziału (typ_pokemona x grupa)
table(pokemon_original_copy$Type.1, pokemon.kmeans$cluster)
#   Widać, że podział zbioru pokemonów na dwie grupy zupełnie nie idzie razem
#   z podziałem pokemonów na typy. Liczby w tabeli dla pierwszej i drugiej grupy
#   są zbyt bardzo do siebie podobne dla zbyt dużej liczby typów. Przykład:
# Grass    35 35
# Ground   15 17
# Ice      10 14
# Normal   54 44
# Poison   14 14

#   wykres
plot(pokemon, col =(pokemon.kmeans$cluster +1) , main="Pokemony K-Means", pch=75, cex=0.5)

#   Łączna suma kwadratów odległości punktów od środka grupy (WCSS)
pokemon.kmeans$tot.withinss
#   WCSS = 6358363

#   Suma kwadratów odległości punktów od środka grupy dla każdej grupy
pokemon.kmeans$withinss
#   2173827 4184535

#####################################
#   Eksperyment 2
#   W tym eksperymencie znacznie zwiększymy liczbę grup (do 12).
#   Można przewidzieć, że WCSS będzie mniejsze niż w eksperymencie 1.
pokemon.kmeans = kmeans(pokemon, 12, iter.max = 20)

#   tabela podziału (typ_pokemona x grupa)
table(pokemon_original_copy$Type.1, pokemon.kmeans$cluster)
#   W porównaniu do poprzedniego eksperymentu, podział na większą liczbę
#   grup może doprowadzić, iż niektóre tpy pokemonów będą bardziej skupione
#   w niektórych grupach bardziej niż w innych.
#   Wynik:
#           1  2  3  4  5  6  7  8  9 10 11 12
# Bug       8  1  4 18  5  0  6  3  1  3 20  0
# Dark      7  2  2  1  2  0  5  0  4  2  5  1
# Dragon    5  1  0  0  2  0  1  5  3  0  4 11
# Electric  8  8  0  2  3  0  0  1  5  7 10  0
# Fairy     5  4  0  2  0  1  2  0  0  0  2  1
# Fighting  7  0  0  2  3  2  5  0  1  4  3  0
# Fire     11  2  1  1  3  0  3  3  6 10 11  1
# Flying    0  0  0  1  0  0  0  0  2  1  0  0
# Ghost    11  1  4  0  4  1  3  0  1  2  3  2
# Grass    22  7  1  3  4  2  8  3  3  6 11  0
# Ground   12  1  0  1  8  1  1  0  2  1  3  2
# Ice       7  3  1  1  2  0  2  2  1  3  2  0
# Normal   17  3  0 20  4  7 21  1  4  1 17  3
# Poison    9  1  1  3  4  1  6  0  0  1  2  0
# Psychic  14  7  0  2  1  1  1  8  7  6  5  5
# Rock     11  3  3  0  7  0  1  3  3  1 10  2
# Steel     5  0  8  0  2  0  1  6  0  0  3  2
# Water    34 15  3  6  8  5  9  6  3  6 14  3
#   W przypadku typu Water jedna grupa przetrzymuje aż 34 pokemony,
#   dwie około 15, a reszta już poniżej 10. Podobna sytuacja jest
#   w przypadku typu Normal, czy Bug.
#   Wydaję mi się jkednak, iż jest to bardziej przypadkowe, niż 
#   można się tego tutaj doszukiwać, gdyż sporo grup, jak Fairy, czy
#   Dark nie wykazują takiego rozkładu w ogóle.

#   wykres
plot(pokemon, col =(pokemon.kmeans$cluster +1) , main="Pokemony K-Means", pch=75, cex=0.5)

#   Łączna suma kwadratów odległości punktów od środka grupy (WCSS)
pokemon.kmeans$tot.withinss
#   WCSS = 2089094

#   Suma kwadratów odległości punktów od środka grupy dla każdej grupy
pokemon.kmeans$withinss
#   399286.23 136454.34 128856.86 134721.08 140548.24 119615.43 164564.16 179891.37 140707.26  97571.91 305296.99 141580.24

#   Zwiększona ilość grup dla algorytmu kmeans zmniejsza WCSS.

#####################################
#   Eksperyment 3
#   W tym eksperymencie zbadam metodą 'łokcia' optymalną liczbę grup
#   biorąc pod uwagę łączną wewnątrzgrupową sumę kwadratów odległości
#   punktów od środka grupy (WCSS). Następnie obliczę WCSS dla 
#   najlepszego grupowania

wcss <- vector(mode = "integer" ,length = unique_pokemons_length)

for (i in 1:unique_pokemons_length) {
  kmeans.group <- kmeans(pokemon, centers = i, nstart=20)
  # Całkowita suma odległości wewnątrz grup
  wcss[i] <- kmeans.group$tot.withinss
}
remove(kmeans.group)

print(wcss)
# [1] 15789506  6358363  4251396  3476049  3124246  2901934  2593540  2431045  2317386  2212841  2120166  2031396  1976849  1902412  1845000  1800797  1735770
# [18]  1713983
plot(1:unique_pokemons_length, wcss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrz grup")

#   Zgodnie z przewidywaniami na początku tej serii eksperymentów 
#   algorytm kmeans będzie dawał coraz lepsze WCSS dla większej
#   ilości tworzonych grup. Z drugiej strony jesteśmy ograniczeni
#   założeniami zadanie - maksymalna ilość grup == ilość
#   typów pokemonów, czyli 18.

#   Ze zmiennej wcss wiemy, że WCSS dla 18 grup == 1713983,
#   lecz przeprowadźmy jeszcze raz grupowanie, aby przejrzeć tabelę i wykresy
pokemon.kmeans = kmeans(pokemon, unique_pokemons_length, iter.max = 20, nstart = 50)

#   tabela podziału (typ_pokemona x grupa)
table(pokemon_original_copy$Type.1, pokemon.kmeans$cluster)
#   Wynik:
#           1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
# Bug       3  5  0  0  4  1  3  5  7  4  0  2 11  2  0 15  6  1
# Dark      3  4  0  0  3  1  1  2  2  4  1  0  2  5  0  1  1  1
# Dragon    1  1  0  1  0  0  0  3  2  8 10  0  2  0  0  0  2  2
# Electric  4  0  0  0  0  0  7  7  1  6  0  8  5  5  0  1  0  0
# Fairy     0  0  0  0  1  0  0  1  1  0  1  3  0  4  0  2  1  3
# Fighting  6  2  1  0  5  0  0  3  2  1  0  0  1  2  0  2  2  0
# Fire      7  1  0  0  1  0  4  9  1  8  1  6  8  3  0  1  1  1
# Flying    0  0  0  0  0  0  0  0  0  2  0  1  0  0  0  1  0  0
# Ghost     1  4  1  0  2  1  2  3  0  1  2  1  2  5  0  0  4  3
# Grass     5  5  0  0  1  0  4  9  6  6  0  6  5  6  0  3  7  7
# Ground    3  6  0  1  1  0  0  3  2  2  1  0  1  3  0  0  7  2
# Ice       1  1  0  0  1  1  4  5  1  2  0  3  1  1  1  1  1  0
# Normal    3  3  4  0 14  1  4 10  4  5  3  2 11 13  0 17  1  3
# Poison    4  2  0  0  5  0  0  5  0  0  0  0  0  4  0  1  4  3
# Psychic   1  0  1  2  0  2  7  9  1 13  3  2  3  6  0  2  0  5
# Rock      2  6  0  0  1  4  0  1  6  4  2  1  2  7  1  0  5  2
# Steel     1  4  0  0  0  2  0  0  4  3  2  0  0  0  4  0  5  2
# Water     8  5  3  1  5  0  4 13  8  6  4  9  6 13  2  5  9 11
#   Sytuacja ma się podobnie do poprzedniego eksperymentu - w przypadku
#   niektórych grup, jak Dragon wygląda, że podział skupia się jednym
#   głównym punkcie, paru mniejszych, a reszta to wartości marginalne.
#   Niestety sporo trypów, jak chociażby Normal, Fighting dają zupełnie
#   inne wrażenie - przypadkowości.

#   wykres
plot(pokemon, col =(pokemon.kmeans$cluster +1) , main="Pokemony K-Means", pch=75, cex=0.5)

#   Łączna suma kwadratów odległości punktów od środka grupy (WCSS)
pokemon.kmeans$tot.withinss
#   WCSS = 1705261
#   Widać, że różnica między WCSS z tego eksperymentu, a eksperymentu 2 jest, ale nie 
#   jest już ona tak kolosalna jak różnica między eksperymentami pierwszym i drugim.

#   podobny do tego z eksperymentu drugiego - zwiększona liczba grup
#   zmniejszy łączną wewnątrzgrupową sumę kwadratów odległości punktów od środka grupy.

#####################################
#   Eksperyment 4
#   W tym eksperymentcie zobaczę, jaki wpływ ma skalowanie na grupowanie zbioru pokemonów.

pokemon_scaled <- scale(pokemon, center = FALSE)

pokemon_scaled.kmeans = kmeans(pokemon_scaled, unique_pokemons_length, iter.max = 20, nstart = 50)
pokemon_scaled.kmeans$tot.withinss
#   Wynik WCSS = 299.1396 jest zaskakująco niski. Z tego powodu sprawdzę, jak skalowanie
#   wpłynęło na grupowanie na inną liczbę grup. Posłużę się wykorzystywaną wczęsniej metodą.

wcss <- vector(mode = "integer" ,length = unique_pokemons_length)

for (i in 1:unique_pokemons_length) {
  kmeans.group <- kmeans(pokemon_scaled, centers = i, nstart=20)
  # Całkowita suma odległości wewnątrz grup
  wcss[i] <- kmeans.group$tot.withinss
}
remove(kmeans.group)

print(wcss)
# [1] 1642.0690  810.6812  614.7789  547.3468  498.9701  457.0422  433.6306  411.9519  394.8368  381.6075  365.0948  355.4231  344.4428  332.7466  324.7291
# [16]  315.1440  305.8109  299.2311
plot(1:unique_pokemons_length, wcss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrz grup")

#   Jak widać wyniki dla wszystkich grupowań drastycznie spadły. 

table(pokemon_original_copy$Type.1, pokemon_scaled.kmeans$cluster)

#   Po zweryfikowaniu tego, że tabela wcale nie wygląda lepiej można zorientować
#   się, że wyniki są lepsze dlatego, że wartości każdej kolumny zostały znormalizowane 
#   do przedziału <0,1>. To spowodowało, że odległości między punktami były mniejsze.
#   Przeglądanie zmiennej 'pokemon_scaled' uświadomiło mi, że była to kolumna generacji. 
#   Być może pozostawienie generacji było sporym błędem, dlatego w następnym eksperymentach
#   w tej serii będę pozbywał się zbędnych kolumn.


#####################################
#   Eksperyment 5
#   Tak jak zapowiedziałem w poprzednim eksperymencie, usunę w tym eksperymencie kolumnę
#   generacji i powtórzę część badań wcześniej wykonanych.

pokemon_2 <- pokemon
pokemon_2$Generation = NULL

#   Mógłbym z góry założyć, iż najlepszym grupowaniem będzie grupowanie dla maksymalnej
#   liczby z góry założonego przedziału, czyli 18. Wykonam jednak na wszelki wypadek
#   poszukiwanie metodą 'łokcia'
wcss <- vector(mode = "integer" ,length = unique_pokemons_length)

for (i in 1:unique_pokemons_length) {
  kmeans.group <- kmeans(pokemon_2, centers = i, nstart=20)
  # Całkowita suma odległości wewnątrz grup
  wcss[i] <- kmeans.group$tot.withinss
}
remove(kmeans.group)

print(wcss)
# [1] 15787301  6356161  4249191  3473857  3122090  2807303  2591028  2428968  2315063  2210797  2113544  2046179  1971540  1901977  1858144  1796106  1738799
# [18]  1700279
plot(1:unique_pokemons_length, wcss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrz grup")
#   Nadal najlepszy wynik WCSS algorytm kmeans daje dla grupowania na 18 grup.
#   Jest on równy dla powyższych nastawów kmeans = 1700279
#   Wszystkie wyniki, w tym ten najlepszy, są trochę lepsze od uzyskanych w eksperymencie 3.
#   Może to być spowodowane tym, że generacja była przeszkodą, a być może jedynie pozbyliśmy
#   się jednego z kryteriów (wymiarów), czego naturalnym następstwem była lekka poprawa w grupowaniu.

#   Sprawdźmy, czy skalowanie wynikóW da jakąkolkwiek poprawę:
pokemon_scaled <- scale(pokemon_2, center = FALSE)
pokemon_scaled.kmeans = kmeans(pokemon_scaled, centers = unique_pokemons_length, iter.max = 20, nstart = 5,  algorithm = 
                                 c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
pokemon_scaled.kmeans$tot.withinss
#   WCSS = 219.8889
table(pokemon_original_copy$Type.1, pokemon_scaled.kmeans$cluster)

#   Jak widać WCSS dla przeskalowanych wartości bardzo zmalała w porównaniu do poprzedniej.
#   To może być potwierdzeniem, że trzymanie generacji było sporym błędem.
#   Niestety tabela nie różni się bardzo od otrzymanych wcześniej wyników, chociaż można
#   dostrzec, że nastąpiła lekka poprawa. Na przykład grupa 1. zawiera przede wszystkim 
#   typ Fire, Normal i Water oraz ewentualnie (już nie dwucyfrowe liczby) Electric i Grass.

#####################################
#   Eksperyment 6
#   W tym eksperymencie usunę kolejny domniemanie zbędny atrybut - legendarność.
#   Jest przeprowadzony dla przewidywanie najlepszego podziału - na 18 grup.
pokemon_3 <- pokemon
pokemon_3$Generation = NULL
pokemon_3$Legendary_boolean = NULL

#   Jednocześnie zauważyłem, że nastawy algorytmu kmeans w poprzednich eksperymentach mogły
#   być niewystarczająco dobre i nie dawać najlepszych możliwych wyników.
pokemon_3.kmeans = kmeans(pokemon_3, unique_pokemons_length, iter.max = 500, nstart = 1000)
pokemon_3.kmeans$tot.withinss
#   WCSS = 1682289
#   Nastąpiła kolejna poprawa wyniku. Mogło się tak stać za sprawą usunięcia zbędnej
#   kolumny legendarnośći pokemonów, ale najprawdopodobniej stało się to za sprawą 
#   lepszych nastawów algorytmu kmeans.

table(pokemon_original_copy$Type.1, pokemon_3.kmeans$cluster)
# Wynik:
#           1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
# Bug       7 15 12  0  1  2  5  0  0  2  6  4  7  1  0  0  4  3
# Dark      4  1  2  1  1  0  3  0  4  5  2  2  1  1  0  0  0  4
# Dragon    1  0  2 10  0  0  0  1  1  0  2  2  3  2  0  0  7  1
# Electric  0  1  5  0  0  8  6  0  5  5  1  7  0  1  0  0  1  4
# Fairy     1  2  0  1  0  3  0  0  0  4  1  1  1  3  0  0  0  0
# Fighting  7  2  1  0  0  0  2  0  1  2  2  3  2  0  1  0  0  4
# Fire      0  1  8  1  0  7  1  0  3  3  1  9  1  2  0  0  5 10
# Flying    0  1  0  0  0  1  0  0  2  0  0  0  0  0  0  0  0  0
# Ghost     5  0  2  2  1  3  1  0  1  5  0  3  4  3  1  0  0  1
# Grass     6  3  5  0  0  6  4  0  2  6  6  9  7  7  0  0  4  5
# Ground    7  0  1  1  0  0  0  1  0  3  2  3  7  2  0  0  2  3
# Ice       2  1  1  0  1  4  3  0  1  1  1  5  1  0  0  1  1  1
# Normal    5 17 10  3  1  2 15  0  1 13  5 10  1  2  4  0  4  5
# Poison    4  1  0  0  0  0  4  0  0  4  0  4  5  2  0  0  0  4
# Psychic   0  2  4  3  1  4  4  2  7  6  0  9  0  6  1  0  7  1
# Rock      6  0  2  2  4  1  0  0  1  7  7  0  6  2  0  1  3  2
# Steel     4  0  0  2  2  0  0  0  0  0  4  0  5  2  0  4  3  1
# Water     6  5  6  2  0  9  7  1  3 13  8 13  9 12  3  2  5  8
#   Patrząc na tę tabelę można dostrzec, że mimo, iż jest nadal 
#   nieidealna, to każda grupa posiada jeden do dwóch typów,
#   które przeważają liczebnością nad innymi. Niestety
#   nie widać tutaj idealnego odwzorowania (podziału), w którym
#   każda grupa przetrzymuje inne typy.

plot(pokemon_3, col =(pokemon_3.kmeans$cluster +1) , main="Pokemony K-Means", pch=75, cex=0.5)


#   Spróbujmy jeszcze przeskalować wartości w kolumnach:
pokemon_scaled <- scale(pokemon_3, center = FALSE)
pokemon_scaled.kmeans = kmeans(pokemon_scaled, centers = unique_pokemons_length, iter.max = 500, nstart = 1000,  algorithm = 
                                 c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
pokemon_scaled.kmeans$tot.withinss
#   WCSS = 200.2606 dla podziału na 18 grup.
#   

#   W poprzednich eksperymentach liczba iteracji, albo nstart mogłą być 
#   niewystarczająco duża i dostarczać nienajlepsze możliwie wyniki.
#   Ponadto WCSS dla znormalizowanych odległości i nieznormalizowanych
#   daje jeszcze nieco lepsze wyniki, niż w poprzednim eksperymencie,
#   jednak jak widać nielegendarność nie wpływała aż tak bardzo.
#   Może to byc spowodowane tym, że jednak przytłaczająco większość
#   pokemonów nie była legendarna i była grupowana w jednej grupie.


#####################################################################################
#
#   Seria eksperymentów 2
#   Ta seria eksperymentów jest wykonywana dla miary: Czystości grup - średnia
#   liczba typów pokemonów w grupie (typ jest reprezentowany w grupie jeśli
#   ma przynajmniej trzech przedstawicieli).

#   W tym celu, aby zautomatyzować liczenie średniej liczby typów pokemonów w 
#   grupie można utworzyć funkcję. 

#   Pierwsza funkcja będzie liczyła średnią liczbę pokemonów, uwzględniając jedynie
#   podstawowy typ pokemona oraz jest przeznaczona dla algorytmu kmeans.
mean_pokemon_types_in_cluster = function(kmeans) {
  #   Ustalam, na ile grup został zrobiony podział
  len <- length(unique(kmeans$cluster))
  
  #   Tworzę macierz grupa x typ_pokemona
  x <- matrix(vector(mode = "integer", length = len*unique_pokemons_length), nrow=len, ncol=unique_pokemons_length)

  #   Dla wygody nazywam kolumny (typy pokemonów)
  colnames(x, do.NULL = TRUE, prefix = "col")
  colnames(x) <- unique_pokemons
  
  #   Zakładam, że pokemony w zmiennej kmeans$cluster są w
  #   takiej kolejności, w jakiej były podane w zmiennej pokemon.
  for (i in 1:length(kmeans$cluster)) {
    cluster_number <- kmeans$cluster[i]
    type_1 <- pokemon_original_copy$Type.1[i]
    x[cluster_number, type_1] = x[cluster_number, type_1] + 1
  }
  
  remove(i, type_1, cluster_number)
  
  vector_of_number_of_groups_detected <- vector(mode = "integer", length = len)
  for(i in 1:len) {
    number_of_groups_detected = 0
    for (j in 1:length(unique_pokemons)) {
      if (x[i, j] >= 3) {
        number_of_groups_detected = number_of_groups_detected + 1
      }
    }
    vector_of_number_of_groups_detected[i] <- number_of_groups_detected
  }
  remove(i, j, len)
  return(mean(vector_of_number_of_groups_detected))
}

#   Druga funkcja będzie liczyła średnią liczbę pokemonów, uwzględniając już 
#   oba - typ podstawowy i drugi pokemona. Ta funkcja też jest przeznaczona 
#   dla algorytmu kmeans.
mean_pokemon_types_in_cluster_2types = function(kmeans) {
  
  #   Ustalam, na ile grup został zrobiony podział
  len <- length(unique(kmeans$cluster))
  
  #   Tworzę macierz grupa x typ_pokemona
  x <- matrix(vector(mode = "integer", length = len*unique_pokemons_length), nrow=len, ncol=unique_pokemons_length)
  
  #   Dla wygody nazywam kolumny (typy pokemonów)
  colnames(x, do.NULL = TRUE, prefix = "col")
  colnames(x) <- unique_pokemons
  
  #   Zakładam, że pokemony w zmiennej kmeans$cluster są w
  #   takiej kolejności, w jakiej były podane w zmiennej pokemon.
  for (i in 1:length(kmeans$cluster)) {
    cluster_number <- kmeans$cluster[i]
    type_1 <- pokemon_original_copy$Type.1[i]
    type_2 <- pokemon_original_copy$Type.2[i]
    x[cluster_number, type_1] = x[cluster_number, type_1] + 1
    if (type_2 != "") {
    x[cluster_number, toString(type_2)] = x[cluster_number, toString(type_2)] + 1
    }
  }
  
  remove(i, type_1, type_2, cluster_number)
  
  vector_of_number_of_groups_detected <- vector(mode = "integer", length = len)
  for(i in 1:len) {
    number_of_groups_detected = 0
    for (j in 1:length(unique_pokemons)) {
      if (x[i, j] >= 3) {
        number_of_groups_detected = number_of_groups_detected + 1
      }
    }
    vector_of_number_of_groups_detected[i] <- number_of_groups_detected
  }
  remove(i, j, len)
  return(mean(vector_of_number_of_groups_detected))
}


#####################################
#   Eksperyment 1
#   W tym eksperymencie sprawdzę dla każdego grupowania (grupowania
#   na 2, 3, 4, ... 18 grup), używając algorytmu kmeans, czystość grup,
#   czyli średnią liczbę typów pokemonów w grupie. 
#   Oczywiście będę sprawdzał jednocześnie sprawdzał dla podstawowego
#   oraz dwóch typów (funkcje mean_pokemon_types_in_cluster oraz
#   mean_pokemon_types_in_cluster_2types). Można więc powiedzieć, że
#   ten eksperyment zawiera w sobie 2 eksperymenty.
#   Oszczędzę także eksperymentów dla zbiorów pokemon i pokemon_2 i
#   od razu będę analizował pokemon_3.

purity <- vector(mode = "integer", length = unique_pokemons_length)
purity_2types <- vector(mode = "integer", length = unique_pokemons_length)

for (i in 1:unique_pokemons_length) {
  kmeans.group <- kmeans(pokemon_3, centers = i, iter.max = 500, nstart=750)
  purity[i] <- mean_pokemon_types_in_cluster(kmeans.group)
  purity_2types[i] <- mean_pokemon_types_in_cluster_2types(kmeans.group)
}
remove(kmeans.group)

print(purity)
# [1] 18.000000 17.500000 16.000000 15.750000 14.400000 12.333333 11.714286 11.125000  9.888889  8.900000  9.000000  8.666667  7.846154  7.428571  7.200000
# [16]  7.187500  6.470588  6.166667
print(purity_2types)
# [1] 18.00000 18.00000 17.66667 17.75000 16.20000 14.83333 14.71429 14.62500 13.44444 12.40000 12.54545 12.08333 11.84615 11.00000 10.66667 10.37500 10.11765
# [18]  9.00000

#   Najlepszymi wynikami są odpowiednio dla:
#   - purify -> średnio 6.166667 typów w grupie dla podziału na 18 grup
#   - purity_2types -> średnio 9.00000 typów w grupie dla podziału na 18 grup

plot(1:unique_pokemons_length, purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość grup dla podstawowego typu")

plot(1:unique_pokemons_length, purity_2types, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość grup dla podstawowego typu")

#   Jak widać czystość grup w przypadku algorytmu kmeans również
#   ma tendencję spadkową wraz ze zwiększającą się liczbą grup, 
#   na ile kmeans ma podzielić zbiór pokemonów.
#   Na wykresach można zobaczyć, że między niektórymi wartośćiami
#   w czystości grup dla kolejnych liczb grup jest bardzo duża,
#   a dla innych wręcz przeciwnie - może wzrosnąć (minimalizujemy
#   czystość, więc jest to zjawisko niepożądane). Ogólnie jednak
#   jest tendencja spadkowa dla obu czystości grup dla podstawowego
#   i podstawowego wraz z drugim typem.

#####################################
#   Eksperyment 2
#   W tym eksperymencie sprawdzę, czy skalowanie ma wpływ na czystość
#   grup. Eksperyment ten, podobnie, jak poprzedni, będzie analizował
#   czystość grupy dla podstawowego typu, ale także dla dwóch typów.
#   Można więc powiedzieć, że podobnie, jak poprzedni eksperyment,
#   składa się z dwóch podeksperymentóW.
pokemon_scaled <- scale(pokemon_3, center = FALSE)

purity <- vector(mode = "integer", length = unique_pokemons_length)
purity_2types <- vector(mode = "integer", length = unique_pokemons_length)

for (i in 1:unique_pokemons_length) {
  kmeans.group <- kmeans(pokemon_scaled, centers = i, iter.max = 750, nstart=1000)
  purity[i] <- mean_pokemon_types_in_cluster(kmeans.group)
  purity_2types[i] <- mean_pokemon_types_in_cluster_2types(kmeans.group)
}
remove(kmeans.group)

print(purity)
# [1] 18.000000 17.500000 17.000000 15.000000 13.800000 12.833333  9.857143 10.750000 10.111111  9.500000  9.909091  8.666667  8.307692  7.928571  7.333333
# [16]  6.750000  6.705882  6.388889
print(purity_2types)
# [1] 18.000000 18.000000 18.000000 17.500000 16.600000 15.500000 13.285714 13.875000 13.333333 12.700000 12.727273 11.916667 11.538462 11.285714 10.733333
# [16] 10.125000  9.470588  9.277778

#   Najlepszymi wynikami są odpowiednio dla:
#   - purify -> średnio 6.388889 typów w grupie dla podziału na 18 grup
#   - purity_2types -> średnio 9.277778 typów w grupie dla podziału na 18 grup

plot(1:unique_pokemons_length, purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość grup dla podstawowego typu")

plot(1:unique_pokemons_length, purity_2types, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość grup dla podstawowego typu")

#   Wykres ma trochę inną charakterystykę niż ten w poprzednim eksperymencie,
#   jednak także ma charakterytykę spadkową - dla więszkej ilości grup spada
#   średnia liczba typów pokemonów w grupie. Największy spadek można zauważyć 
#   między podziałem na 3 i 7 grup. Następnie przy paru następnych podziałach 
#   nie widać prawie żadnej poprawy, ale już w końcowych kilku podziałach
#   znowu widać wyraźnie spadkową tendencję wykresu.
#   Zaskakująco także wyniki nieznacznie się pogorszyły w stosunku do poprzedniego
#   eksperymentu.



#####################################################################################
#   Wnioski: 
#   Wnioskami są przedstawiane w trakcie oraz przede wszystkim pod koniec każdego
#   eksperymentu. Są w nich analizowane wyniki i przedstawiane porównania do innych
#   eksperymentów.
#   Wszystkie przedstawione wnioski dotyczą algorytmu k-środków, czyli kmeans.
#   Spostrzeżeniem, które można zaobserwować dla obu (WCSS oraz czystość grup)
#   miar jest to, że są coraz lepsze (mniejsze, gdyż minimalizujemy te warotści)
#   dla coraz większej liczby grup. Dla obu jest to całkiem zrozumiałe, gdyż
#   dążąc do 800 grup każdy pokemon miałby swoją grupę, co spowodowałoby, że
#   ogległość wewnątrzgrupowa od środka (WCSS) byłaby zerowa, przy czym czystość
#   także byłaby zerowa (1 zaczynałaby się od 3 pokemonów danego typu w grupie).
#   Niestety nie udało się odwzorować grup na typy pokemonów.
#   Być może, gdyby brać pod uwagę nie tylko typ pierwszy i drugi pokemona, tylko
#   traktować je jako osobne byty, czyli były pokemony z typem ogień+latanie,
#   czyli łącznie mogłoby być 18+18*18 = 342 typów (niektóre nie mają drugiego typu),
#   to grupowanie byłoby lepsze. Wtedy też podniosłaby się maksymalna liczba
#   grup by się znacznie zwiększyła. Niestety założenia początkowe, zawarte w 
#   treści zadania raczej wskazywały na maksymalną liczbę grup = 18.
#   Ponadto przy podziale na grupy warto pozbyć się niepotrzebnych atrybutów. W ich
#   skład wchodzi nie tylko imię i id pokemona, ale także jego generacja oraz 
#   ewentualnie jego legendarność.
#   W drugiej serii eksperymentów liczba eksperymentów jest dużo mniejsza, poniważ z 
#   góry założyłem, że zbiór pokemon_3 pozbawiony domniemanie zbędnych atrybutów
#   będzie najlepszy. Oprócz tego, eksperymenty te mierzą jednocześnie dwie wartośći -
#   czystość grupy dla podstawowego typu danych i dla dwóch typów danych.
