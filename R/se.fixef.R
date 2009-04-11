`se.fixef` <-
function(model)
	{
		se <- sqrt(diag(vcov(model)))
		return(se)
	}