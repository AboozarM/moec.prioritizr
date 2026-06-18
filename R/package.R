#' moec.prioritizr: Multi-Objective Prioritization with the Epsilon-Constraint Approach
#'
#' Systematic conservation planning is a framework to identify
#' priority areas for conservation management (Giakoumi et al. 2025).
#' Briefly, priority areas are identified by
#' formulating a mathematical optimization problem based on conservation
#' objectives and requirements and then using optimization algorithms
#' to generate solutions. To help account for multiple objectives,
#' the \pkg{moec.prioritizr} package provides an implementation
#' of the epsilon-constraint approach for multi-objective optimization
#' (Eichfelder 2008). The package is designed as a plugin for
#' the \pkg{prioritizr} package -- a general purpose
#' package for systematic conservation planning (Hanson et al. 2025) --
#' and provide enhanced functionality.
#' By using multi-objective optimization approaches, conservation scientists and
#' practitioners can explore trade-offs and identify
#' solutions that represent a desirable compromise among multiple objectives
#' (Neubert et al. 2025).
#'
#' @section Citation:
#' Please cite the _moec.prioritizr R_ package when using it in publications. To
#' cite the package, please use:
#'
#'
#' @seealso
#' Useful links:
#' * Package website (<https://AboozarM.github.io/moec.prioritizr/>)
#' * Source code repository (<https://github.com/AboozarM/moec.prioritizr>)
#' * Report bugs (<https://github.com/AboozarM/moec.prioritizr/issues>)
#'
#' @author
#'  Authors:
#' * Aboozar Mohammadi \email{mohammadi.aboozar@gmail.com} ([ORCID](https://orcid.org/0000-0003-3411-9424))
#' * Jeffrey O Hanson \email{jeffrey.hanson@uqconnect.edu.au} ([ORCID](https://orcid.org/0000-0002-4716-6134))
#'
#' @references
#' Eichfelder G (2008) _Adaptive Scalarization Methods in Multiobjective
#' Optimization_. Springer Berlin Heidelberg.
#'
#' Giakoumi S, Richardson AJ, Doxa A, Moro S, Andrello M, Hanson JO, Hermoso V,
#' Mazor T, McGowan J, Kujala H, Law E, Álvarez Romero JG, Magris RA, Gissi E,
#' Arafeh-Dalmau N, Metaxas A, Virtanen EA, Ban NC, Runya RM, Dunn DC,
#' Fraschetti S, Galparsoro I, Smith RJ, Bastardie F, Stelzenmüller V,
#' Possingham HP, and Katsanevakis S (2025) Advances in systematic conservation
#' planning to meet global biodiversity goals.
#' *Trends in Ecology and Evolution*, 40: 395--410.
#'
#' Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese P,
#' Bennett JR, and Possingham HP (2025) Systematic conservation prioritization
#' with the prioritizr R package. *Conservation Biology*, 39: e14376.
#'
#' Neubert S, McGowan J, Metcalfe K, Hanson JO, Buenafe KCV, Dabalà A,
#' Dunn DC, Everett JD, Possingham HP, Stelzenmüller V, Estep A, Ervin J, and
#' Richardson AJ (2025) Multiple-use spatial planning for sustainable
#' development and conservation. *Trends in Ecology and Evolution*,
#' 40: 1126--1142.
#'
#' @name moec.prioritizr
#' @docType package
#' @aliases moec.prioritizr-package
"_PACKAGE"

# avoid false positive NOTES
#' @importFrom terra rast
NULL

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1")  utils::globalVariables(c("self"))
