`dp.ME.cook` <-
function(estex, parameters=0, groups=0, cutoff=0, sort=FALSE, ...)
	{	
	ifelse(parameters==0, p.sel <- 1:dim(estex$alt.fixed)[2], p.sel <- parameters) 
	ifelse(groups==0, g.sel <- 1:dim(estex$alt.fixed)[1], g.sel <- groups)
	
	plot.matrix <- ME.cook(estex, parameters=p.sel, sort=sort)[g.sel,]
		
	if(cutoff == 0)
		{ 
		
		print(dotplot(plot.matrix, ...))
		
		}
	
	if(cutoff != 0)
		{
		col <- rep("blue", length(plot.matrix))
		col[which(abs(plot.matrix) >= cutoff)] <- "red"
		
		pch <- rep(19, length(plot.matrix))
		pch[which(abs(plot.matrix) >= cutoff)] <- 17
		
		print(dotplot(plot.matrix, col=col, pch=pch, ...))
		
		}
		
	}
