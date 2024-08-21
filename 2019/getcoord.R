getcoord <- function(plot, corner=NULL, plotcoordx=NULL, plotcoordy=NULL)
{
	coords <- which(matrix(1:50,nrow=10,ncol=5,byrow=TRUE)==plot,arr.ind=TRUE)
	
	xpos <- (coords[2]-1)*400
	ypos <- (coords[1]-1)*(-400)	
	
	if (!is.null(corner) & is.null(plotcoordx) & is.null(plotcoordy))
	{
		offset_x <- NA
		offset_y <- NA
		if (corner=="upper left")
		{
			offset_x <- 0
			offset_y <- 0
		}
		else if (corner=="upper right")
		{
			offset_x <- 200
			offset_y <- 0
		}
		else if (corner=="lower left")
		{
			offset_x <- 0
			offset_y <- -200
		}
		else if (corner=="lower right")
		{
			offset_x <- 200
			offset_y <- -200
		}
		
		xpos <- xpos + offset_x
		ypos <- ypos + offset_y
	}
	else if (!is.null(plotcoordx) & !is.null(plotcoordy) & is.null(corner))
	{
		xpos <- xpos + plotcoordx
		ypos <- ypos - (200 - plotcoordy)
	}
	else
	{
		stop("incorrect input")
	}
	
	return(c(x=xpos, y=ypos))
}