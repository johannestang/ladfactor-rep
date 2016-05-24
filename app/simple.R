simpleforecast <- function(preddata, xdata, startdate, date, h, verbose, hWindow=FALSE, forcFunc="mean")
{
    forecasts <- c()
	for (j in 1:ncol(preddata))
	{
		X <- window(preddata[,j], start=startdate)
		noobs <- NROW(na.omit(X))
		if (hWindow)
		{
			tmp <- do.call(forcFunc, list(X[(noobs-h+1):noobs]));
		}
		else
		{
			tmp <- do.call(forcFunc, list(X[1:noobs]));
		}
		forecasts <- c(forecasts, tmp)
	}
	names(forecasts) <- colnames(preddata)
	return(list(forecasts=forecasts))
}
