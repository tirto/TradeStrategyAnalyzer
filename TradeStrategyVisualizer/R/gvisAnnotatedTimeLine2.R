# code taken from googleVis package
# added optional params for chart title and description

gvisAnnotatedTimeLine2 <- function(data, datevar="",
                                  numvar="", idvar="", titlevar="",
                                  annotationvar="",
                                  date.format="%Y/%m/%d",
                                  options=list(), 
                                  chartid, 
                                  charttitle="", 
                                  chartdesc=""){


  my.type <- "AnnotatedTimeLine"
  dataName <- deparse(substitute(data))

  my.options <- list(gvis=modifyList(list(width = 600, height=300), options),
		     dataName=dataName,
                     data=list(datevar=datevar, numvar=numvar,
                       idvar=idvar, titlevar=titlevar, annotationvar=annotationvar,
		       date.format=date.format,
                       allowed=c("number", "string", "date", "datetime"))
                     )

  checked.data <- gvisCheckAnnotatedTimeLineData(data, my.options, datevar=datevar,
                                                 idvar=idvar, titlevar=titlevar, annotationvar=annotationvar)

  output <- gvisChart2(type=my.type, checked.data=checked.data, options=my.options, chartid=chartid, charttitle, chartdesc)

  return(output)
}


gvisCheckAnnotatedTimeLineData <- function(data, options, datevar,idvar,
                                           titlevar, annotationvar){

  if( any(class(data[[datevar]]) %in% c("POSIXct", "POSIXlt")) ){   
    data.structure <- list(
                           datevar  = list(mode="required",FUN=check.datetime),
                           numvar   =  list(mode="required",FUN=check.num),
                           idvar  = list(mode="optional",FUN=check.char),                   
                           titlevar = list(mode="optional",FUN=check.char),
                           annotationvar  = list(mode="optional",FUN=check.char))
  }else{
    data.structure <- list(
                           datevar  = list(mode="required",FUN=check.date),
                           numvar   =  list(mode="required",FUN=check.num),
                           idvar  = list(mode="optional",FUN=check.char),                   
                           titlevar = list(mode="optional",FUN=check.char),
                           annotationvar  = list(mode="optional",FUN=check.char))
  }
  
  x <- gvisCheckData(data=data, options=options, data.structure=data.structure)
   

  x.df <- as.data.frame(x)


    ##check if idvar is missing that datevar define unique rows
  if(idvar==""){
    if(length(x.df[,1]) != length(unique(x.df[,1])))
      message("Warning: The data appears to more than one entry for the same date.\n",
              "Have you considered using the idvar variable?")
    ## if idvar is missing no reshape is required.
    return(x.df)
  }
  

  groups <- factor(x.df[[idvar]])
  ngroups <- nlevels(groups)


  checkTitleAnno <- c(titlevar != "", annotationvar != "")
  if(all(checkTitleAnno)){
    varying.vars <- c(2,4,5)
  }else{
    if(any(checkTitleAnno))
      varying.vars <-  c(2,4)
    else
      varying.vars <- 2
  }
  var.names <- names(x.df)[varying.vars]
  
  x.df <- reshape(x.df,
                  v.names=var.names, ##numvar , titlevar, annotationvar
                  idvar=names(x.df)[1], ## datevar 
                  timevar=names(x.df)[3], ## idvar
                  direction="wide") 

  names(x.df) <- gsub(paste(var.names[1], ".", sep=""), "", names(x.df))
  
  return(x.df)
}

