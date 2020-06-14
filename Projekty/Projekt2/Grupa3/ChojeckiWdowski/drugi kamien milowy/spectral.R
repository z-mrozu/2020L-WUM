library(plot3D)
library(dendextend)
library(mclust)
library(dplyr)
library(igraph)
library(genie)
library(RSpectra)
library(stringi)
library(genie)
library(cluster)

Mydist <- function(x, y){
  # zwraca odleglosc miedzy dwoma wektorami w R^n
  (x-y)^2 %>% sum %>% sqrt
}

vMdist <- function(v, X){
  # zwraca wektor odległośći wektora v od wektorów macierzy X
  apply(X, 1, Mydist, y=v)
}

Mdist <- function(X){
  # zwraca macierz odległości, czyli W=[w_{i,j}], gdzie w_{i,j} to odległość między punktem i, a punktem j
  apply(X, 1, vMdist, X)
}

which.min.M <- function(v, W, M){
  # zwraca wektor indexów M najbliższych sąsiadów v'a(v to numer wektora) w macierzy X
  
  wybor <- rbind(W[v, ], 1:length(W[v, ])) # biorę tylko odległości do interesującego mnie wektora i numeruję
  wybor <- wybor[, which(wybor[2,] != v)]  # nie biore pod uwege jego samego, chce jego sasiadow
  wybor <- wybor[, order(wybor[1, ])]      # ustawiam ich w odpowiedniej polejnosci-od najblizszego
  wybor[2, 1:M]                            # biore pierwsze M wynikow
}

Mnn <- function(X, M){
  # zwraca macierz z indeksami najblizszych sasiadow
  
  n <- nrow(X)
  # W <- Mdist(X) własna funkcja, bardzo wolna, bo to 2 pentle
  W <- dist(X, upper=TRUE) %>% as.matrix
  S <- apply(as.matrix(1:n), 1, which.min.M, W, M)
  t(S)
}








vG1 <- function(v, n){
  # funkcja przyjmuje wektor z sasiadami
  # zwraca wektor sasiedztwa, czyli na piejscu sasiadow jest 1, a na pozostalych 0
  sasiedzi <- integer(length = n)
  sasiedzi[v] <- 1
  sasiedzi
}

Mnn_graph <- function(S){
  n <- nrow(S)
  G1 <- apply(S, 1, vG1, n) # na miejscach sasiadow jest 1, a na pozostalych 0
  G2 <- t(G1)               # G1 to macierz grafu skierowanego, aby był nieskierowany
  G <- G1 | G2              # trzeba go polaczyc z jego transpozycja
  G <- uspojnij(G)          # lacze skladowe w spojny graf
  G
}



# funkcja niewywolywana, jest zbyt wolna
spojny_old <- function(G){
  # funkcja rekurencyjna, przyjmuje niespojny graf, jesli po dodaniu jednej krawedzi dalej bedzie niespujny, wywola sie raz jeszcze
  nieskonczone <- G %>% distances() %>% is.infinite() # nieskonczone odleglosci w grafie
  
  index <- which(nieskonczone, arr.ind = TRUE)[1, ] # przykladowy index nieskonczonej odleglosci
  G <- add_edges(G, index) # wystarczy dodac w jednym miejscu, bo G jest undirected
  
  nieskonczone <- G %>% distances() %>% is.infinite() # niekonczone raz jeszcze
  
  if(!any(nieskonczone)){
    return(G[]) # jesli juz jest spojne
  }else{
    G2 <- spojny(G) # lacz dalej
    return(G[] | G2[])   # lacze ich matrixy
  }
}
# funkcja niewywolywana, jest zbyt wolna
uspojnij_old <- function(G){
  # przyjmije macierz grafu nieskierowanego
  # zwraca ja uspujniona
  Gprym <- G %>% graph_from_adjacency_matrix(mode = "undirected") # zmieniam macierz na graf
  
  if(!Gprym %>% distances() %>% is.infinite() %>% any)
    return(as.matrix(Gprym[])) # jesli jest spojny
  else
    return(spojny(Gprym))
}

uspojnij <- function(G){
  # przyjmije macierz grafu nieskierowanego
    # zwraca ja uspujniona
  Gprym <- G %>% graph_from_adjacency_matrix(mode = "undirected") # zmieniam macierz na graf
  
  bylo <- 1
  c <- components(Gprym)$membership
  for(i in 1:length(c)){
    if(!c[i] %>% is.element(bylo)){
      Gprym <- add_edges(Gprym, c(i, i-1))
      Gprym <- add_edges(Gprym, c(i-1, i))
      
      bylo <- union(bylo, c[i])
    }
  }
  
  Gprym[]
}




diagonala <- function(d, n){
  v <- integer(n)
  v[d[2]] <- d[1]
  v
}

Laplacien_eigen <- function(G, k){
  n <- dim(G)[1]   # wielkosc kwadratowej
  D <- apply(G, 1, sum) # ciag stopni wierzcholkow
  D <- cbind(D, 1:length(D)) # stopnie wierzcholkow wraz z indeksami
  D <- apply(D, 1, diagonala, n)
  
  L <- D-G
  
  
  # funkcja eigis jest duzo szybsza od eigen, ale dla rzadkich 
  E <- tryCatch({
    e <- eigs(L, k+1, sigma=0)$vectors
    e[, 1:k]       # od drugiego
  }, warning=function(cond){   # wywoluja zwykla procedure jesli wystapi przypadekniedzialania
    print("dupa")
    WW <- eigen(L)
    
    najmniejsze <- which(WW[["values"]] %in% (WW[["values"]] %>% sort %>% head(k+1) %>% tail(k))) # czesc do przetestowania
    E <- cbind(WW[["vectors"]][, najmniejsze])
    E
  })
  
  
  E
}



Laplacien_eigen_k_lowest_eigen_value <- function(G, k){
  n <- dim(G)[1]   # wielkosc kwadratowej
  D <- apply(G, 1, sum) # ciag stopni wierzcholkow
  D <- cbind(D, 1:length(D)) # stopnie wierzcholkow wraz z indeksami
  D <- apply(D, 1, diagonala, n)
  
  L <- D-G
  
  # funkcja eigis jest duzo szybsza od eigen, ale w rzadkich rzypadkach nie zwraca wyniku, wtedy posloze sie funkcja wbudowana
  E <- tryCatch({
    e <- eigs(L, k, sigma = 0)$vectors
    e
  }, warning=function(cond){   # wywoluje zwykla procedure jesli wystapi przypadek niedzialania
    print("dupa")
    WW <- eigen(L)
    e_eigen <- WW$values[1100:1200]
    
    najmniejsze <- which(WW[["values"]] %in% (WW[["values"]] %>% sort %>% head(k))) # czesc do przetestowania
    E <- cbind(WW[["vectors"]][, najmniejsze])
    E
  })
  
  
  E
}






spectral_clustering <- function(X, k, M){
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacien_eigen(G, k)
  kmeans(E, k)[["cluster"]]
}


spectral_clustering_some_kmeans <- function(X, k, M, l){
  # wywoluje angorytm k-srednich kilka razy i zwraca wszystkie wyniki
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacien_eigen(G, k)
  
  out <- matrix(nrow=nrow(X), ncol=l)
  for(i in 1:l){
    out[, i] <- kmeans(E, k)[["cluster"]]
  }
  
  if(dim(t(unique(t(out))))[2]>200){   # jesli jest na tyle duze, ze bardzo dlugo trwa liczenie
    print("Nie podjemuje sie szukania takich samych")
    attr(out, "ilosc") <- integer(dim(out)[2])+1
    return(out)
  }
  rozne(out)
}

czy_to_samo <- function(x, y){
  # zwraca prawde, jesli dane clastry sa takie same
  dendextend::FM_index(x, y) == 1
}

rozne <- function(out){
  # dostaje macierz clastrow i zwraca tylko rozniace sie miedzy soba
  
  unikalne <- t(unique(t(out))) # tylko unikalne wartosci ( nadal zostja te same clastry ze zmienionymi indexami, ale obliczenia w pentli beda na mniejszej ilosci danych )
  
  # aby z macierzy out wyciagnac naprawde wyjadkowe clastry, skorzystam z funkcji dendextend::FM_index
  
  
  
  nie_zwroce <- numeric(0)
  
  # do usuniecia powtarzajacych sie urzywam pentli, bo i tak jest ich malo
  for(i in 1:dim(unikalne)[2]){                  # dla kazdego clastra
    if(i %>% is.element(nie_zwroce)) next   # jesli juz taki mialem, to nastepny
    for(j in i:dim(unikalne)[2]){                # porownam go z kolejnymi, ale rowniez z pierwszym
      if(i==j) next                         # a z pierwszym nie chce porownywac
      if(j %>% is.element(nie_zwroce)) next # juz wiem, ze go nie wezme
      
      if(czy_to_samo(unikalne[,i], unikalne[,j])) nie_zwroce <- nie_zwroce %>% append(j) # dadaje do niechcianych
    }
  }
  
  zwroce <- which(!(1:dim(unikalne)[2] %>% is.element(nie_zwroce))) # zwracam te, ktorych nie usunolem
  
  dodaj_attr_ilosc(out, unikalne[, zwroce])
}


dodaj_attr_ilosc <- function(out, unikalne){
  # funkcja dostaje macierz i porownuje ze soba kolumny za pomoca funkcji czy_to_samo
    # przyjmuje macierz, ktorej kolumny sa unikalnymi kolumnami z out
    # zwraca macierz unikalne z dodatkowym atrubutem-ilosc, ktory wskazuje na to ile razy
    # dany wektor pojawia sie jako kolumna macierzy out
  if(is.null(dim(unikalne))){
    attr(unikalne, "ilosc") <- dim(out)[2]
    return(unikalne)
  }
  
  ilosc <- integer(dim(unikalne)[2]) # wektor z zerami,bede dodawal jeden, gdy to samo
  
  niesprawdzone <- 1:dim(out)[2]     # wektor z liczbami, ktore bede sprawdzal w kazdej iteracji
  
  for(i in 1:length(ilosc)){
    for(j in niesprawdzone){                      # porownujetylko te, ktorych jeszcze nie porownywalem
      if(czy_to_samo(unikalne[,i], out[, j])){      # jesli trafie na to samo
        ilosc[i] <- ilosc[i]+1
        niesprawdzone <- niesprawdzone[-which(niesprawdzone==j)]  # usun z listy nieporownywanych
      }
    }
  }
  
  attr(unikalne, "ilosc") <- ilosc
  
  if(!ilosc %>% sum == dim(out)[2]) print("cos poszlo nie tak z liczeniem ilosci clastrow") # ten if ulatwil mi znalesienie bledu :)
  
  unikalne
}






spectral_clustering_k_lowest_eigen_value <- function(X, k, M){
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacien_eigen_k_lowest_eigen_value(G, k)
  kmeans(E, k)[["cluster"]]
}





spectral_clustering_some_kmeans_k_lowest_eigen_value <- function(X, k, M, l){
  # wywoluje angorytm k-srednich kilka razy i zwraca wszystkie wyniki
    # biorac naprawde k najmniejszych wektorow wlasnych
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacien_eigen_k_lowest_eigen_value(G, k)
  
  out <- matrix(nrow=nrow(X), ncol=l)
  for(i in 1:l){
    out[, i] <- kmeans(E, k)[["cluster"]]
  }
  
  if(dim(t(unique(t(out))))[2]>200){   # jesli jest na tyle duze, ze bardzo dlugo trwa liczenie
    print("Nie podjemuje sie szukania takich samych")
    attr(out, "ilosc") <- integer(dim(out)[2])+1
    return(out)
  }
  rozne(out)
}






suma_poza_klastrem <- function(X, y){
  uy <- unique(y)
  
  A <- dist(X, upper=TRUE) %>% as.matrix
  
  for(i in uy){
    A[y==i, y==i] <- 0
  }
  
  sum(A)
}


suma_wewnatrz_klastra <- function(X, y){
  uy <- unique(y)
  
  A <- dist(X, upper=TRUE) %>% as.matrix
  
  out <- 0
  for(i in uy){
    out <- out + sum(A[y==i, y==i])
  }
  
  out
}













