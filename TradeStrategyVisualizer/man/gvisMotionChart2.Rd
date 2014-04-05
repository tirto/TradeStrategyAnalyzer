\name{gvisMotionChart2}
\alias{gvisMotionChart2}
\title{
modified version of gvisMotionChart
}
\description{
has all of the functionalities of gvisMotionChart, plus optional chart title and description args
}
\usage{
gvisMotionChart2(data, 
                 idvar="id", 
                 timevar="time", 
                 date.format="\%Y/\%m/\%d", 
                 options = list(), 
                 chartid, 
                 charttitle="",
                 chartdesc="")
}
\arguments{
  \item{data}{
    a \code{data.frame}. The data has to have at least four
    columns with subject name (\code{idvar}), time (\code{timevar}) and
    two columns of numeric values. Further columns, numeric and
    character/factor are optional. The combination of \code{idvar} and
    \code{timevar} has to describe a unique row.
  }
  \item{idvar}{
    column name of \code{data} with the subject to be analysed.
  }
  \item{timevar}{
    column name of \code{data} which shows the time dimension. 
  }
  \item{date.format}{
    specifies how the dates are reformatted to be used by JavaScript.
  }
  \item{options}{
    list of configuration options for Google Motion Chart.
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
  \code{gvisMotionChart} 
}
\examples{
## see demo.R
}
