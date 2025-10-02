## (a)
echantillon <- function(..., centrer = FALSE, reduire = FALSE) {
  donnee<-list(...) # en bas on vas convertir la liste en data frame
  if(length(donnee)==0){
    stop("l'échantillon doit avoir au moins une variable")
  }
  # L'argument doit permettre à l'utilisateur de saisir les données en utilisant 
  #la syntaxe nom_1 = var_1, ..., nom_p = var_p
  noms_vars<-names(donnee)
  if(is.null(noms_vars)|| any(noms_vars=="")){
    stop("on doit avoir (nom_1 =var1,...).")
  }
  
  #création des vecteurs moyennes et écart-types
  moyennes<-rep(0,length(donnee))
  ecartype<-rep(1,length(donnee))
  names(moyennes)<-noms_vars
  names(ecartype)<-noms_vars
  
  
  
  # standardisé les données
  data<-as.data.frame(donnee)
  if(centrer|| reduire){
    for(i in seq_along(data))
      variable<-data[[i]]
    
    #centrons
    if(centrer){
      m<-mean(variable)
      variable<-variable - m
      moyennes[i]<-m
      
      
      #réduisons
      
      if(reduire){
        s<-sd(variable)
        ecartype[i]<-s
        
      }
      
    }
    data[[i]]<-variable
  }
  obj<-data
  # attribu
  
  attr(obj,"centrer")<-centrer
  attr(obj,"reduire")<-reduire
  attr(obj,"moyennes")<-moyennes
  attr(obj,"ecartype")<-ecartype 
  class(obj)<-c("echantilon",class(obj))
  return(obj)
  
}


set.seed(1234)
e1<-echantillon(poids=c(56,23,90), age=c(12,15,17))
e1




## (b)
?print
print.echantillon <- function(x) {
    n_obs<-nrow(x)
    n_vars<-ncol(x)
    # on récupére les attribus deux attribus du constructeur
    centrer<-attr(x,"centre")
    reduire<- attr(x,"reduit")
    class(x)<-class(x)[-1]
    #print(head(x))
    class(x)<-c("echantillon",class(x)) # on mets classe pour pas modifié l'objet
    invisible(x)
    
  }

## Implémentez la méthode `summary` ici

## (c)
?summary
#format(x, digits = max(3L, getOption("digits") - 3L), ...)
## S3 method for class 'summaryDefault'
#print(x, digits = max(3L, getOption("digits") - 3L), ...)
summary.echantillon<-function(x,...){
  # récupération des attribus
  moyennes<-attr(x,"moyenne")
  ecartype<-attr(x,"ecartype")
  parametre<-data.frame( Moyenne=moyennes,Ecartype=ecartype)
  print(parametre)
  
  format(x, digits = max(3L, getOption("digits") - 3L), ...)
  
  print(x, digits = max(3L, getOption("digits") - 3L), ...)
}
summary.echantillon(e1)

# Implémentez la méthode `lm` ici

## (d)
?lm
?formula
### Create a formula for a model with a large number of variables:
#xnam <- paste0("x", 1:25)
#(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
# ici j'ai utilisé la documentation de formula pour implémenter mon code

lm.echantillon<-function(echantillon,reponse,covariable){
  y<-reponse
  x<-covariable
  data_lm<-as.data.frame(echantillon$data)
  formula_lm<-as.formula(paste("y~ ", paste(x, collapse = "+")))
  
  model<-lm(formula_lm,data=data_lm)
  return(model)
}

# Implémentez vos fonctions ici

sous_echantillon<-function(in_echa,taille,remplacement=FALSE,...){
  n<- nrow(in_echa)
  ratio<-0.8
  indicesample<-sample(1:n,size = floor(ratio*n)) # tire 80% des indices  de léchantillon initiale partie enrtière
  sous_echa<- data[indicesample, ]
  
  #Un booléen indiquant si l'échantillonnage doit être fait avec ou sans remplacement
  echantillon_remise<-sample(x=in_echa,taille,replace = TRUE)
  
  nouveau_echa<-echantillon(sous_echa)
  return(nouveau_echa)
}
