`estex` <-
function(model, group, select=0, gf="single", count=FALSE, ...)
	   {
		   # Defining Internal Variables
		   data.adapted <- model.frame(model)
		   original.parameters <- which(substr(names(fixef(model)), 1,6) != "estex.")
		   
   		   grouping.names <- grouping.levels(model, group)
   
		   n.groups <- length(grouping.names)
		   n.pred <- length(fixef(model)[original.parameters])	
   
		   if(select[1] == 0)
		  	 {
			      
				# Defining and naming the output elements
		   		# Fixed Estimates of the original model
		   	   	or.fixed <- matrix(ncol = n.pred , nrow = 1, data = fixef(model)[original.parameters])
		   	   	dimnames(or.fixed) <- list(NULL, names(fixef(model))[original.parameters])
		   	   	
		   	   	# Standard Error of the original model
		   		or.se <- matrix(ncol = n.pred , nrow = 1, data = se.fixef(model)[original.parameters])
	   		 	dimnames(or.se) <- list(NULL, names(fixef(model))[original.parameters])
	   		 	
	   		 	# Variance / Covariance Matrix of the original model
			   	or.vcov <- as.matrix(vcov(model)[original.parameters, original.parameters])
			   	dimnames(or.vcov) <- list(
			   		names(fixef(model)[original.parameters]), 
			   		names(fixef(model)[original.parameters]))
			   	
			   	# Fixed Estimates of the modified model(s)
		   		alt.fixed <- matrix(ncol = n.pred, nrow = n.groups, data = NA)
		   		dimnames(alt.fixed) <- list(grouping.names, names(fixef(model))[original.parameters])
		   		
		   		# Standard Error of the modified model(s)
				alt.se <- matrix(ncol = n.pred , nrow = n.groups, data = NA)
				dimnames(alt.se) <- list(grouping.names, names(fixef(model))[original.parameters])	
				
				# Variance / Covariance Matrix of the modified model(s)
		   		alt.vcov <- list()
		   
				   
		   for (i in 1:n.groups)
			   {
		   
			   if(count == TRUE) {print(n.groups + 1 - i)}
				   
				   model.updated <- exclude.influence(model, group, grouping.names[i], gf=gf)

				   alt.fixed[i,] <- as.matrix(fixef(model.updated)[original.parameters])
				   alt.se[i,] <- as.matrix(se.fixef(model.updated)[original.parameters])
				   alt.vcov[[i]] <- as.matrix(vcov(model.updated)[original.parameters, original.parameters])
			   }
			   	
			
		   }
		   
		   if(select[1] != 0)
			   {
				   
				   model.updated <- exclude.influence(model, group, select, gf=gf)
				   
				   
				# Defining and naming the output elements
		   		# Fixed Estimates of the original model
		   	   	or.fixed <- matrix(ncol = n.pred , nrow = 1, data = fixef(model)[original.parameters])
		   	   	dimnames(or.fixed) <- list(NULL, names(fixef(model))[original.parameters])
		   	   	
		   	   	# Standard Error of the original model
		   		or.se <- matrix(ncol = n.pred , nrow = 1, data = se.fixef(model)[original.parameters])
	   		 	dimnames(or.se) <- list(NULL, names(fixef(model))[original.parameters])
	   		 	
	   		 	# Variance / Covariance Matrix of the original model
			   	or.vcov <- as.matrix(vcov(model)[original.parameters, original.parameters])
			   	dimnames(or.vcov) <- list(
			   		names(fixef(model)[original.parameters]), 
			   		names(fixef(model)[original.parameters]))
			   	
			   	# Fixed Estimates of the modified model(s)
		   		alt.fixed <- matrix(ncol = n.pred, nrow = 1, data = fixef(model.updated)[original.parameters])
		   		dimnames(alt.fixed) <- list(
		   			"Altered model", 
		   			names(fixef(model))[original.parameters])
		   		
		   		# Standard Error of the modified model(s)
				alt.se <- matrix(ncol = n.pred , nrow = 1, data = se.fixef(model.updated)[original.parameters])
				dimnames(alt.se) <- list("Altered model", names(fixef(model))[original.parameters])
				
				# Variance / Covariance Matrix of the modified model(s)
				alt.vcov <- list()
		   		alt.vcov[[1]] <- as.matrix(vcov(model.updated)[original.parameters, original.parameters])
		   		dimnames(alt.vcov[[1]]) <- list(
			   		names(fixef(model)[original.parameters]), 
			   		names(fixef(model)[original.parameters]))
				 
			   }
		   
		   estex <- list(
			   or.fixed = or.fixed,
			   or.se = or.se,
			   or.vcov = or.vcov,
			   alt.fixed = alt.fixed, 
			   alt.se = alt.se,
			   alt.vcov = alt.vcov)
   
		   return(estex)	
	   }

