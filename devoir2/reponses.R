## Exercice 1
## (a)


point<- function(k){
  k<-sort(unique(as.integer(k)))
  stopifnot(all(k>0))
  return(k)
}

set <- function(E, k) {
  if (length(E)<k) return(0)
  combinaison<-combn(E,k)  # on calcule toutes les  combinaisons de k éléments 
  pdt<-apply(combinaison,2,prod)  # on fait le produit de chaque élément
  inclus<-E[E%in% pdt] # on garde ceux qui sont dans E
  sum(inclus)
}

##Test##
E<-point(c(2, 5, 6, 12, 15, 60))
cat("la somme de tous les k-uplets de (E,2)=",set(E,2),"\n")

## (b)
setn <- function(E) {
  n<-length(E)
  finale<-0
  for(k in 2:n){
    finale<-finale+set(E,k)
  }
  return(finale)
}
##Test##
# Ici j'utilise le même E que pour la partie A
cat("la somme des set_n(E)=",set_n(E),"\n")

## (c)
set_crible <- function(E) {
  N<-length(E)
  # 4 est le premier nombre composé car 2et 3 sont premiers 
  #et 1 n'est pas premiers aussi il n'est pas composé
  if(N< 4){
    # on retourne un vecteur vide si N<4
    return(numeric(0))
    
  }
  compose<-rep(FALSE,N)
  compose[1]<-TRUE
  # on vas parcourir les nombres de 2 à N
  for(i in 2:N){
    # on sait que si i est premier ses multiples sont composés
    j=2*i
    while (j<=N) {
      compose[j]<-T
      j<-j+i
      
    }
  }
  # on vas retourner toutes les composés ou compose[i] est vrai
  nombres_composes<-which(compose)
  # on trie tous les composés , et on garde que ceux qui sont supérieur à 4
  nombre_compose<-nombres_composes[nombres_composes>=4]
  return(nombre_compose)
}

