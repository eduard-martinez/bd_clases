
# functions
cat("Estas funciones fueron tomadas de: 
     https://stackoverflow.com/questions/45177590/map-of-bivariate-spatial-correlation-in-r-bivariate-lisa")

# Bivariate Moran's I
moran_I = function(x, y = NULL, W){
          if(is.null(y)) y = x
          
          xp = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
          yp = (y - mean(y, na.rm=T))/sd(y, na.rm=T)
          W[which(is.na(W))] = 0
          n = nrow(W)
          
          global = (xp%*%W%*%yp)/(n - 1)
          local  = (xp*W%*%yp)
          
          list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran = function(x, y = NULL, W, nsims = 1000){
  
                if(is.null(y)) y = x # Si es univariada
                
                n   = nrow(W)
                IDs = 1:n
                
                xp = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
                W[which(is.na(W))] = 0
                
                global_sims = NULL
                local_sims  = matrix(NA, nrow = n, ncol=nsims)
                
                ID_sample = sample(IDs, size = n*nsims, replace = T)
                
                y_s = y[ID_sample]
                y_s = matrix(y_s, nrow = n, ncol = nsims)
                y_s = (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
                
                global_sims  = as.numeric( (xp%*%W%*%y_s)/(n - 1) )
                local_sims  = (xp*W%*%y_s)
                
                list(global_sims = global_sims,
                     local_sims  = local_sims)
}
