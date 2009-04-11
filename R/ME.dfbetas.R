`ME.dfbetas` <-
function(estex, parameters=0, plot=FALSE, ...) 
	{
		
		if(plot==TRUE)
			{
			dp.ME.dfbetas(estex, parameters=parameters, ...)
			}
	
		n.groups <- dim(estex$alt.fixed)[1]
		n.parameters <- dim(estex$alt.fixed)[2]
		ifelse(parameters==0, sel <- 1:n.parameters, sel <- parameters)	
					
		a <- as.matrix(estex$or.fixed[,sel])
		b <- as.matrix(estex$alt.fixed[,sel])
		c <- as.matrix(estex$alt.se[,sel])
		
		e <- matrix(data=NA, nrow=n.groups, ncol=length(sel))
		
		if(n.groups == 1)
			{
			e <- (a-b) / c
			return(e)
			}
		
		if(n.groups > 1)
		{
			for (i in 1:n.groups) 
				{
					e[i,] <- (a-b[i,]) / c[i,]
				}
			dimnames(e) <- dimnames(estex$alt.fixed[, sel])
		}	
		
		

		return(e)
	}