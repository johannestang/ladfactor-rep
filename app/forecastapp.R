library(forecastexp)
library(macrods)
library(pryr)
library(xtable)

# Load additional code
source("simple.R")

# Load the dataset and define data function
load("FRED.rda")
datafunc <- partial(getmacrodata, ds=FRED)

# Dates:
dates <- cbind(rep(1981:2012, each=12), rep(1:12,(2012-1981+1)));
dates <- dates[1:(nrow(dates)-2),];

startdate <- fsd <- c(1971,2)

# Define the variables we wish to forecast and the experiment function
vars <- c("INDPRO", "W875RX1", "PAYEMS", "CPILFESL", "PCEPI", "PPIFGS");
expfunc <- partial(forecastexp, datafunc=datafunc, startdate=startdate, 
				   dates=dates, vars=vars, verbose=TRUE)

# Define the experiment
runexperiments <- function(h) {
    filename <- paste("LADResultsH", h, ".RData", sep="");
    if (file.exists(filename)) load(filename) else allres <- list()

    if (!exists("AR", allres)) {
        allres$AR <- expfunc(partial(arforecast, p=0:6), h=h)
        save(allres, file=filename)
    }

    # Case 1
    ck <- "fixed"
	nm <- paste("PCp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0, k=4, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("PCSp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0, k=4, chooseK=ck, factorstartdate=fsd), h=h, screen=TRUE)
        save(allres, file=filename)
    }
	nm <- paste("LADp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0, k=4, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("LADMADp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0, k=4, chooseK=ck, factorstartdate=fsd, scale="mad"), h=h)
        save(allres, file=filename)
    }

    # Case 2
    ck <- "IC1"
	nm <- paste("PCp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0, k=12, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("PCSp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0, k=12, chooseK=ck, factorstartdate=fsd), h=h, screen=TRUE)
        save(allres, file=filename)
    }
	nm <- paste("LADp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0, k=12, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("LADMADp0", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0, k=12, chooseK=ck, factorstartdate=fsd, scale="mad"), h=h)
        save(allres, file=filename)
    }

    # Case 3
    ck <- "IC1"
	nm <- paste("PCp6", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=6, k=12, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("PCSp6", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(pcfactorforecast, p=6, k=12, chooseK=ck, factorstartdate=fsd), h=h, screen=TRUE)
        save(allres, file=filename)
    }
	nm <- paste("LADp6", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=6, k=12, chooseK=ck, factorstartdate=fsd), h=h)
        save(allres, file=filename)
    }
	nm <- paste("LADMADp6", ck, sep="")
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(ladfactorforecast, p=6, k=12, chooseK=ck, factorstartdate=fsd, scale="mad"), h=h)
        save(allres, file=filename)
    }

    # Cases 4-7
	for (ck in c("IC1", "IC2", "IC3", "BIC")) {
		nm <- paste("PC", ck, sep="")
		if (!exists(nm, allres)) {
            allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0:6, k=12, chooseK=ck, factorstartdate=fsd), h=h)
            save(allres, file=filename)
        }
    }
	for (ck in c("IC1", "IC2", "IC3", "BIC")) {
		nm <- paste("PCS", ck, sep="")
		if (!exists(nm, allres)) {
            allres[[nm]] <- expfunc(partial(pcfactorforecast, p=0:6, k=12, chooseK=ck, factorstartdate=fsd), h=h, screen=TRUE)
            save(allres, file=filename)
        }
    }

	for (ck in c("IC1", "IC2", "IC3", "BIC")) {
		nm <- paste("LAD", ck, sep="")
		if (!exists(nm, allres)) {
            allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0:6, k=12, chooseK=ck, factorstartdate=fsd), h=h)
            save(allres, file=filename)
        }
    }

	for (ck in c("IC1", "IC2", "IC3", "BIC")) {
		nm <- paste("LADMAD", ck, sep="")
		if (!exists(nm, allres)) {
            allres[[nm]] <- expfunc(partial(ladfactorforecast, p=0:6, k=12, chooseK=ck, factorstartdate=fsd, scale="mad"), h=h)
            save(allres, file=filename)
        }
    }

    # Case 9
    # Simple
	nm <- "Umean"
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(simpleforecast, hWindow=FALSE, forcFunc="mean"), h=h)
        save(allres, file=filename)
    }
	nm <- "UmeanW"
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(simpleforecast, hWindow=TRUE, forcFunc="mean"), h=h)
        save(allres, file=filename)
    }
	nm <- "Umedian"
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(simpleforecast, hWindow=FALSE, forcFunc="median"), h=h)
        save(allres, file=filename)
    }
	nm <- "UmedianW"
	if (!exists(nm, allres)) {
        allres[[nm]] <- expfunc(partial(simpleforecast, hWindow=TRUE, forcFunc="median"), h=h)
        save(allres, file=filename)
    }

    save(allres, file=filename)
}

# Helper function for making the tables
mktable <- function(h, outfilename="")
{
    filename <- paste("LADResultsH", h, ".RData", sep="");
	load(filename);
	res <- rbind( 
        relmse(allres$PCp0fixed, allres$AR),
        relmse(allres$PCSp0fixed, allres$AR),
        relmse(allres$LADp0fixed, allres$AR),
        relmse(allres$LADMADp0fixed, allres$AR),

        relmse(allres$PCp0IC1, allres$AR),
        relmse(allres$PCSp0IC1, allres$AR),
        relmse(allres$LADp0IC1, allres$AR),
        relmse(allres$LADMADp0IC1, allres$AR),

        relmse(allres$PCp6IC1, allres$AR),
        relmse(allres$PCSp6IC1, allres$AR),
        relmse(allres$LADp6IC1, allres$AR),
        relmse(allres$LADMADp6IC1, allres$AR),

        relmse(allres$PCIC1, allres$AR),
        relmse(allres$PCSIC1, allres$AR),
        relmse(allres$LADIC1, allres$AR),
        relmse(allres$LADMADIC1, allres$AR),

        relmse(allres$PCIC2, allres$AR),
        relmse(allres$PCSIC2, allres$AR),
        relmse(allres$LADIC2, allres$AR),
        relmse(allres$LADMADIC2, allres$AR),

        relmse(allres$PCIC3, allres$AR),
        relmse(allres$PCSIC3, allres$AR),
        relmse(allres$LADIC3, allres$AR),
        relmse(allres$LADMADIC3, allres$AR),

        relmse(allres$PCBIC, allres$AR),
        relmse(allres$PCSBIC, allres$AR),
        relmse(allres$LADBIC, allres$AR),
        relmse(allres$LADMADBIC, allres$AR),

        relmse(allres$Umean, allres$AR),
        relmse(allres$UmeanW, allres$AR),
        relmse(allres$Umedian, allres$AR),
        relmse(allres$UmedianW, allres$AR),
        
        sqrt(predmse(allres$AR))*1200*c(1/h,1/h,1/h,1,1,1)
    )
    kbars <- c(4, 4, 4, 4, 
               getK(allres$PCp0IC1)[1],
               getK(allres$PCSp0IC1)[1],
               getK(allres$LADp0IC1)[1],
               getK(allres$LADMADp0IC1)[1],
               getK(allres$PCp6IC1)[1],
               getK(allres$PCSp6IC1)[1],
               getK(allres$LADp6IC1)[1],
               getK(allres$LADMADp6IC1)[1],
               getK(allres$PCIC1)[1],
               getK(allres$PCSIC1)[1],
               getK(allres$LADIC1)[1],
               getK(allres$LADMADIC1)[1],
               getK(allres$PCIC2)[1],
               getK(allres$PCSIC2)[1],
               getK(allres$LADIC2)[1],
               getK(allres$LADMADIC2)[1],
               getK(allres$PCIC3)[1],
               getK(allres$PCSIC3)[1],
               getK(allres$LADIC3)[1],
               getK(allres$LADMADIC3)[1])
    names(kbars) <- NULL
	kbars <- formatC(kbars, digits=2, format="f")
    kbars <- c(kbars, rep("", 9))

	rownames(res) <- c(
            "PC&0&4", "PC-S&0&4", "LAD&0&4", "LAD-MAD&0&4",
            "PC&0&IC$_1$", "PC-S&0&IC$_1$", "LAD&0&IC$_1$", "LAD-MAD&0&IC$_1$",
            "PC&6&IC$_1$", "PC-S&6&IC$_1$", "LAD&6&IC$_1$", "LAD-MAD&6&IC$_1$",
            "PC&BIC&IC$_1$", "PC-S&BIC&IC$_1$", "LAD&BIC&IC$_1$", "LAD-MAD&BIC&IC$_1$",
            "PC&BIC&IC$_2$", "PC-S&BIC&IC$_2$", "LAD&BIC&IC$_2$", "LAD-MAD&BIC&IC$_2$",
            "PC&BIC&IC$_3$", "PC-S&BIC&IC$_3$", "LAD&BIC&IC$_3$", "LAD-MAD&BIC&IC$_3$",
            "PC&BIC&BIC", "PC-S&BIC&BIC", "LAD&BIC&BIC", "LAD-MAD&BIC&BIC",
            "U.Mean&&", "U.Mean W&&", "U.Median&&", "U.Median W&&",
            "\\multicolumn{3}{l}{RMSFE(AR)}")
	res2 <- formatC(res, digits=4, format="f");
	for (i in 1:ncol(res)) for (j in 1:(nrow(res)-1))
	{
		if (res[j,i] == min(res[-nrow(res),i])) res2[j,i] <- paste("\\textbf{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 1:4)
	{
		if (res[j,i] == min(res[1:4,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 5:8)
	{
		if (res[j,i] == min(res[5:8,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 9:12)
	{
		if (res[j,i] == min(res[9:12,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 13:16)
	{
		if (res[j,i] == min(res[13:16,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 17:20)
	{
		if (res[j,i] == min(res[17:20,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 21:24)
	{
		if (res[j,i] == min(res[21:24,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	for (i in 1:ncol(res)) for (j in 25:28)
	{
		if (res[j,i] == min(res[25:28,i])) res2[j,i] <- paste("\\uline{", res2[j,i], "}", sep=""); 
	}
	print(xtable(cbind(kbars, res2)), hline.after=c(4,8,12,16,20,24,28,32), 
          include.rownames=TRUE, include.colnames=FALSE, 
		  only.contents=TRUE, sanitize.text.function=function(x) {return(x);}, file=outfilename);
}

getK <- function(res)
{
    n1 <- length(res)
    n2 <- length(res[[1]]$forecasts)
    ks <- numeric(n2)

    for (i in 1:n1)
    {
        ks <- ks + res[[i]]$k
    }
    ks <- ks/n1
    return(ks)
}

runexperiments(12)
runexperiments(6)
runexperiments(24)

if (!file.exists("tex")) dir.create("tex")
mktable(12, outfilename="tex/table12.tex")
mktable(6, outfilename="tex/table6.tex")
mktable(24, outfilename="tex/table24.tex")
