# Praca domowa 7.

### Treść zadania:
Waszym zadaniem będzie kompresja dowolnego obrazka z użyciem PCA.

### Uwagi:
* Tak, powyżej to cała treść.
* PCA znakomicie sprawdza się do kompresji obrazków. Niestety przyjmuje ono dane w dwóch wymiarach.
Dlatego dla typowych obrazków mających trzy kanały (kolory) konieczne będzie przekształcenie go do macierzy 2D.
Najprościej da się to osiągnać łącząc szerokość obrazka z kanałami w jeden wymiar (mam tu na myśli na przykład funkcję `reshape` z `numpy`).
* Nie oczekujemy zapisania skompresowanego obrazka do pliku. Chodzi raczej o pokazanie:
"o tutaj, w tym miejscu, obrazek zmniejszył swoją objętość <dużo> razy".
Można to pokazać na przykład wypisując rozmiary **wszystkich** macierzy potrzebnych do oddtworzenia obrazka i porównując
z wymiarami przed PCA.
* Jeśli ktoś ma zamiar korzystać z `matplotlib.image` do wczytania obrazka, polecam używanie obrazków w formacie `png`.
Z mojego doświadczenia sprawiają najmniej problemów.
* Powyższe punkty to tylko sugestie. To, czego oczekujemy to pokazanie, że użyliście PCA i opisanie jakie macie zyski (na objętości) i straty (zapewne na jakości).
* Inicjatywa własna zawsze mile widziana. Będzie nam również miło, jeśli poświęcicie chwilę na dobór ładnego/ciekawego/mającego inne walory obrazka. Jest to jednak oczywiście fakultatywne.

## Zadanie dodatkowe
Nieostrożny prowadzący zbierał pomiary z czterech źródeł. Niestety pomylił się i pomieszał je ze sobą, kombinując liniowo te cztery sygnały. Czy pomożecie biednemu prowadzącemu odzyskać oryginalne pomiary? Plik z pomieszanymi pomiarami to `signals.tsv`.  
Rada: pomocne może być zastosowanie algorytmu Independent Component Analysis.

Uprzedzając pytania jak prowadzący mógł kombinować liniowo sygnały i tego nie zauważyć:
![](https://cdn.donald.pl/filer_public_thumbnails/filer_public/d6/b6/d6b6f748-c225-43e8-85bb-8f15d89ddfa1/martyniuk.jpg__655x0_q85_crop_subsampling-2_width-655.jpg)
