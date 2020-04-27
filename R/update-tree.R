#' Update the tree with new data
#' 
#' Extract the nodes from the tree that are selected in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object
#' @param data JSON data or nested list representing the new tree structure.
#' @export
updateTree <- function(session, treeId, data=NULL) {
  if(is.list(data)){
    data<-Rlist2json(data)
  }
  message <- list(type="updateTree",data=data)
  if(!is.null(message)) {
    session$sendInputMessage(treeId, message)
  }
}


#' @importFrom jsonify toJSON
Rlist2json <- function(nestedList) {
  as.character(jsonify::toJSON(get_flatList2(nestedList), auto_unbox = T))
}

#' @importFrom stringr str_match
# fix icon retains backward compatibility for icon entries that are not fully specified
fixIconName <- function(icon){
  ## - 'yes' branch of 'if' covers everything which should not be changed
  ##   e.g. "/images/ball.jpg" or "fa fa-file
  ## - 'no' branch of 'if' covers all cases which need to be changed:
  ##   use regex (str_match) to capture groups: 
  ##     * group 1 is either 'glyphicon', 'fa' or 'NA' (if not present)
  ##     * group 2 is the rest wihtout a potential dash '-'
  ##     * if group 1 is empty set it to 'fa'
  ##     * paste the pieces together
  res <- ifelse(grepl("[/\\]|(glyphicon|fa) \\1-", icon), 
                icon, 
                {
                  parts <- str_match(icon, "(glyphicon|fa)*-*(\\S+)")
                  parts[, 2] <- ifelse(is.na(parts[, 2]), "fa", parts[, 2])
                  paste(parts[, 2], paste(parts[, 2], parts[, 3], sep = "-")) 
                })
  ## if NULL was given as parameter res will be length zero
  if (!length(res)) {
    NULL
  } else {
    res
  }
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = fixIconName(attr(nestedList[[name]],"sticon")),
      "type" = attr(nestedList[[name]],"sttype")
    )
    additionalAttributes <- additionalAttributes[which(sapply(additionalAttributes,Negate(is.null)))]
    
    data <- lapply(names(attributes(nestedList[[name]])),function(key){
      if(key %in% c("icon","type","names","stopened","stselected","sttype", "stdisabled")){
        NULL
      }else{
        attr(nestedList[[name]],key)
      }
    })
    if(!is.null(data) && length(data) > 0){
      names(data) <- names(attributes(nestedList[[name]]))
      data <- data[which(sapply(data,Negate(is.null)))]
    }
    
    nodeData <- append(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nestedList[[name]], "stopened")),
          selected = isTRUE(attr(nestedList[[name]], "stselected")),
          disabled = isTRUE(attr(nestedList[[name]], "stdisabled"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nestedList[[name]]))
      flatList =
        Recall(nestedList[[name]], flatList, parent = as.character(length(flatList)))
  }
  flatList
}

get_flatList2 <- function(nstl, fl = NULL, pr = "#") {
	for (name in names(nstl)) {
		nstnm <- nstl[[name]]
		
		typ = attr(nstnm,"sttype")
		ico = attr(nstnm,"sticon")
		if (is.null(typ)) {
			adatr <- list("icon" = ico)
		} else {
			adatr <- list("icon" = ico,"type" = typ)
		}
		
		len = as.character(length(fl) + 1)
		nd <- c(list(
			id = len,
			text = name,
			parent = pr,
			state = list(
				opened   = isTRUE(attr(nstnm, "stopened")),
				selected = isTRUE(attr(nstnm, "stselected"))
			)
		),
		adatr
		)
		
		fl = c(fl,list(nd))
		if (is.list(nstnm)) {
			fl = Recall(nstnm, fl, pr = len)
		}
	}
	fl
}
