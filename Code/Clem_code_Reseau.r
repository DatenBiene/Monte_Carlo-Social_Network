rm(list=ls())
# install.packages("igraph")
library(igraph)

#?graph_from_data_frame()

# d_adjacente<-matrix(c(0,1,1,0,0,1,1,1,0),ncol=3,byrow = TRUE)
# plot(graph_from_adjacency_matrix(d_adjacente))

##Rq si sym�trie dans la cnnection, alors les matrices d'adjacence sont sym�triques.

#### Fonctions ####

#Une utilisation r�alise l'algo de gibbs pour un seul tour#
#chaque xij est recalcul� compte tenu des autres et ceci selon une bernulli dont lexpression esdt donn�e par num (j'ai le calcul sur un brouillon d�gueu)
iteration<-function(mat,param_arrete,param_triangle){
  nbnode<-dim(mat)[1]
  for(i in 1:(nbnode-1)){
  for(j in (i+1):nbnode){
    #i=2;j=3    
    #on va g�n�rer xij sachant les autres points et changer directement la matrice en cons�quence
    #on travaille sur la diagonale sup�rieure
    #calcul de la somme des xikxjk
    
    num<-exp(param_arrete+param_triangle*sum(mat[i,]*mat[,j]))
    proba<-num/(1+num)
    mat[i,j]<-rbinom(1,size = 1,prob = proba)
    mat[j,i]<-mat[i,j]
    }
  }
  return(mat)
}


#en sortie toutes les it�rations de r�seau

generation_reseau_gibbs<-function(nbnode,param_arrete,param_triangle,nbiteration){
# nbnode<-10
# param_arrete<-9
# param_triangle<-1
matrice_init<-matrix(rep(1,nbnode^2),nrow=nbnode)-diag(nbnode)

liste_iteration<-list()
liste_iteration[[1]]<-matrice_init

for(i  in 2:nbiteration){
  liste_iteration[[i]]<-iteration(mat = liste_iteration[[i-1]],param_arrete = param_arrete,param_triangle = param_triangle)
}

return(liste_iteration)
}


#Trac� des n derni�res it�rations de la liste d'iter r�seaux
trace_iter<-function(liste_iteration,niter){
  par(mfrow=c(1,2))
  for(i in (length(liste_iteration)-niter):length(liste_iteration)){
    net<-graph_from_adjacency_matrix(liste_iteration[[i]])
    
    plot(net, edge.arrow.size=.2, edge.color="black",
         vertex.color="orange", vertex.frame.color="#ffffff",
         vertex.label.color="black")
  }
}




#### Application ####

#ici les tiriangles et arr�tes ont le m�me poids
liste_iteration1<-generation_reseau_gibbs(nbnode = 10,param_arrete = 1 ,param_triangle = 1,nbiteration = 3000)
trace_iter(liste_iteration1,9)  #r�seau "normal" (rq on ne regarde que les derni�res it�rations car la chaine converge vers la loi jointe du r�seau)

#Ici je p�nalise les triangles beaucoup plus => on a + de segments et le r�seau est moins connect� (moins de triangles)
liste_iteration2<-generation_reseau_gibbs(nbnode = 10,param_arrete = 1,param_triangle = -10,nbiteration = 3000)
trace_iter(liste_iteration2,9)#on a ici beaucoup  moins de triangles sur les derni�res it�rations

#ici je p�nalise les arr�tes
liste_iteration3<-generation_reseau_gibbs(nbnode = 10,param_arrete = 1 ,param_triangle = 10,nbiteration = 3000)
trace_iter(liste_iteration3,9)# tout le minde est connect� grosso modo










