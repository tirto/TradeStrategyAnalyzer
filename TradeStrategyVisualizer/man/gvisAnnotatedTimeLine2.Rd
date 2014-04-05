\name{gvisAnnotatedTimeLine2}
\alias{gvisAnnotatedTimeLine2}
\title{
modified version of gvisAnnotatedTimeLine2
}
\description{
has all of the functionalities of gvisAnnotatedTimeLine, plus optional chart title and description args and fixes to javascript api location
}
\usage{
gvisAnnotatedTimeLine2(data, 
                       datevar="", 
                       numvar="", 
                       idvar="", 
                       titlevar="", 
                       annotationvar="", 
                       date.format="\%Y/\%m/\%d", 
                       options = list(), 
                       chartid, 
                       charttitle="", 
                       chartdesc="")
}
\arguments{
  \item{data}{
  a \code{data.frame}. The data has to have at least two
    columns, one with date information (\code{datevar})
    and one numerical variable.

  }
  \item{datevar}{
    column name of \code{data} which shows the date dimension. 
  }
  \item{numvar}{
    column name of \code{data} which shows the values to be displayed
  }
  \item{idvar}{
    column name of \code{data} which identifies different groups of the data. 
  }
  \item{titlevar}{
    column name of \code{data} which shows the title of the annotations. 
  }
  \item{annotationvar}{
    column name of \code{data} which shows the annotation text.
  }
  \item{date.format}{
    specifies how the dates are reformatted to be used by JavaScript.
  }
  \item{options}{
    list of configuration options for Google Annotated Time Line.
  }
  \item{chartid}{
    character. If missing (default) a random chart id will be generated
  }
  \item{charttitle}{
    character. can be HTML snippets
  }
  \item{chartdesc}{
    character. can be HTML snippets
  }
}
\references{
  \url{http://code.google.com/apis/visualization/documentation/gallery/motionchart.html}
}
\seealso{
  \code{gvisAnnotatedTimeLine} 
}
\examples{
# see demo.R
}
