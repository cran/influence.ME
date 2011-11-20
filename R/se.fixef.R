`se.fixef` <-
function(model)
	{
		se <- sqrt(diag(as.matrix(vcov(model))))
		return(se)	
	}