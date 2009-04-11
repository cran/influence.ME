`dp.ME.cook` <-
function(estex, parameters=0, groups=0, tol=0, ...)
	{	
	ifelse(parameters==0, p.sel <- 1:dim(estex$alt.fixed)[2], p.sel <- parameters) 
	ifelse(groups==0, g.sel <- 1:dim(estex$alt.fixed)[1], g.sel <- groups)
	
	plot.matrix <- ME.cook(estex, parameters=p.sel)[g.sel,]
	
	if(tol == 0)
		{ 
		
		print(dotplot(plot.matrix, ...))
		
		}
	
	if(tol != 0)
		{
		col <- rep("blue", length(plot.matrix))
		col[which(abs(plot.matrix) >= tol)] <- "red"
		
		pch <- rep(19, length(plot.matrix))
		pch[which(abs(plot.matrix) >= tol)] <- 17
		
		print(dotplot(plot.matrix, col=col, pch=pch, ...))
		
		}
		
	}
