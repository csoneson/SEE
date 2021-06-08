#' Update lasso information
#'
#' Update the lasso information based on the incoming click object from the application.
#'
#' @param click A Shiny click object.
#' @param previous \code{NULL} if no waypoints exist, otherwise a list produced by this function.
#' @param tol A numeric scalar specifying the tolerance to detect if the next click closes the lasso.
#'
#' @return A list containing:
#' \describe{
#' \item{\code{coord}:}{A numeric matrix of lasso waypoint coordinates, where each row is a waypoint.
#' The first column is the x-axis coordinate and the second column is the y-axis coordinate.}
#' \item{\code{closed}:}{A logical scalar specifying whether the lasso is closed.}
#' \item{\code{panelvar1}, \code{panelvar2}:}{Strings specifying the facet on which the lasso occurs.}
#' \item{\code{mapping}:}{A list containing further information about the variables used on the x- and y-axes and for faceting.}
#' \item{\code{css}:}{A numeric matrix of lasso waypoint coordinates in terms of the pixels.
#' Each row corresponds to a waypoint in \code{coord}.}
#' }
#'
#' @details
#' A click close to the first waypoint will close the lasso.
#' The tolerance is defined as a product of \code{tol} and the x-/y-axis range.
#'
#' @author Aaron Lun
#' @seealso \code{\link{.process_selectby_choice}},
#' \code{\link{lassoPoints}}
#' @rdname INTERNAL_update_lasso
.update_lasso <- function(click, previous=NULL, tol=0.01) {
    new_lasso <- list(
        coord=cbind(click$x, click$y), closed=FALSE, 
        panelvar1=click$panelvar1, panelvar2=click$panelvar2, mapping=click$mapping, 
        css=cbind(click$coords_css$x, click$coords_css$y)
    )

    if (length(previous)) {
        # Checking we're in the same facet as any previous waypoints (both can be NULL).
        same.panel <- identical(previous$panelvar1, click$panelvar1) && 
            identical(previous$panelvar2, click$panelvar2)

        if (same.panel) {
            # Closing the lasso if you click close to the starting point, within the same facet.
            xrange <- click$domain$right - click$domain$left
            yrange <- click$domain$top - click$domain$bottom

            last <- previous$coord[1,]
            if (abs(click$x - last[1]) < xrange * tol && abs(click$y - last[2]) < yrange * tol) {
                new_lasso$coord <- rbind(previous$coord, last)
                new_lasso$css <- rbind(previous$css, previous$css[1,])
                new_lasso$closed <- TRUE
            } else {
                new_lasso$coord <- rbind(previous$coord, new_lasso$coord)
                new_lasso$css <- rbind(previous$css, new_lasso$css)
            }
        }
    } 

    new_lasso
}

#' Is the object a closed lasso or Shiny brush?
#' 
#' Checks if an object is a lasso or Shiny brush data store.
#' Note that it may not be either if there is no brush or if the lasso is not yet closed.
#'
#' @param x A lasso or Shiny brush object.
#'
#' @return A logical scalar specifying if \code{x} is a open/closed lasso or Shiny brush.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_is_lasso
.is_open_lasso <- function(x) {
    isFALSE(x$closed)
}

#' @rdname INTERNAL_is_lasso
.is_closed_lasso <- function(x) {
    isTRUE(x$closed)
}

#' @rdname INTERNAL_is_lasso
.is_brush <- function(x) {
    length(x) && is.null(x$closed)
}

#' Test if Shiny brushes are identical
#'
#' Check whether brush coordinates have actually changed between two Shiny brush objects.
#'
#' @param old_brush A Shiny brush object with the elements \code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}.
#' @param new_brush Another  Shiny brush object for the same plot.
#'
#' @details
#' This function checks whether there are any significant differences in the rectangular regions defined by two Shiny brushes.
#' If there is no change, there is no need to waste time updating the plot.
#'
#' The tolerance is defined as one millionth of the x- and y-axis range for \code{xmin}/\code{xmax} and \code{ymin}/\code{ymax}, respectively.
#' We do not use \code{all.equal(old_brush, new_brush)}, as the plot domain can sometimes change without affecting the actual brush coordinates.
#'
#' @return A logical scalar indicating whether the brushes are identical.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_identical_brushes
#' @seealso
#' \code{\link{iSEE}}
.identical_brushes <- function(old_brush, new_brush) {
    old_null <- length(old_brush)==0L
    new_null <- length(new_brush)==0L
    if (old_null || new_null) {
        return(old_null==new_null)
    }

    if (!.is_brush(old_brush) || !.is_brush(new_brush)) {
        return(FALSE)
    }

    xspan <- old_brush$xmax - old_brush$xmin
    tol <- xspan * 1e-6
    if (abs(old_brush$xmin - new_brush$xmin) > tol
        || abs(old_brush$xmax - new_brush$xmax) > tol) {
      return(FALSE)
    }

    yspan <- old_brush$ymax - old_brush$ymin
    tol <- yspan * 1e-6
    if (abs(old_brush$ymin - new_brush$ymin) > tol
        || abs(old_brush$ymax - new_brush$ymax) > tol) {
      return(FALSE)
    }

    TRUE
}

#' Get the brushed points
#'
#' Get the identities of brushed points selected by a Shiny brush or a closed lasso.
#'
#' @param contents A data.frame containing all points displayed in the affected plot with their identities in the row names.
#' @param cur_brush A Shiny brush or lasso structure in the affected plot.
#'
#' @return
#' A character vector of names of selected points in the brush/lasso.
#' If \code{cur_brush} is empty or not a closed lasso, \code{NULL} is returned instead.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_get_brushed_points
#' @importFrom shiny brushedPoints
.get_brushed_points <- function(contents, cur_brush) {
    if (.is_brush(cur_brush)) {
        rownames(brushedPoints(contents, cur_brush))
    } else if (.is_closed_lasso(cur_brush)) {
        rownames(lassoPoints(contents, cur_brush))
    } else {
        NULL
    }
}
