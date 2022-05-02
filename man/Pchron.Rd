\name{Pchron}
\docType{data}
\alias{Pchron}
\title{Tree-ring width (TRW) chronology
}

\description{TRW chronology for Pinus pinaster forests from two sample
 plots in northern Spain (Soria) and east-central Spain (Cuenca). The
 data set contains eight TRW series. To account for source variability,
 two trees were selected per site, and two samples were extracted from
 each tree.}

\usage{data(Pchron)} 
\format{ \code{data.frame} object with eight columns.
  \describe{
    \item{Rows}{Row names of the data set are time-units labels (years of formation).}

   \item{Columns}{Column names represent levels in three within-stand
    ecological factors that are common to most TRW data and represent
    variability in stand qualities (stand), tree-radial morphology
    (sample), and tree genetics/phenotypes (tree).  The names are
    dot-separated labels representing the hierarchy of the three
    ecological factors, where higher ecological-factor levels are
    defined first and lower levels after. For example, code
    'P16106.17.a' is the column name of sample replicate 'a' in tree '17'
    in plot 'P16106'.}
  
}
    }
    
    \details{ This data set belongs to a more extensive tree-ring
chronology which was processed in previous studies (Bogino and Bravo,
2008) by measuring TRW in polished-core samples (5 mm diameter) using
the Windendro program and cross-dating the tree-ring chronologies using
COFECHA software. Time units for the TRWs are formation years spanning
from 1861 to 2005. The cross-dating records suggested that maritime pine
chronologies had high SNR: (23-28 in northern Spain, and 38-61 in
east-central Spain) and high EPS (0.96 in northern Spain, and 0.98 in
east-central Spain). }

\references{Bogino, S., and Bravo, F. (2008). Growth response of Pinus
pinaster Ait. to climatic variables in central Spanish
forests. Ann. For. Sci. 65, 1-13.  }
\examples{ str(Pchron) }
\keyword{data sets}
