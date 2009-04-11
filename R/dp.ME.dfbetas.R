`dp.ME.dfbetas` <-
function(estex, parameters=0, groups=0,  ...)
		{	
		ifelse(parameters==0, p.sel <- 1:dim(estex$alt.fixed)[2], p.sel <- parameters) 
		ifelse(groups==0, g.sel <- 1:dim(estex$alt.fixed)[1], g.sel <- groups)
	
			plot.matrix <- ME.dfbetas(estex)[g.sel, p.sel]
			print(dotplot(plot.matrix, groups=FALSE, ...))

		}

