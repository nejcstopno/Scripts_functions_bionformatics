blank2na = function(x,na.strings=c('','.','NA','na','N/A','n/a','NaN','nan')) {
  if (is.factor(x)) {
    lab = attr(x, 'label', exact = T)
    labs1 <- attr(x, 'labels', exact = T)
    labs2 <- attr(x, 'value.labels', exact = T)
    
    # trimws will convert factor to character
    x = trimws(x,'both')
    if (! is.null(lab)) lab = trimws(lab,'both')
    if (! is.null(labs1)) labs1 = trimws(labs1,'both')
    if (! is.null(labs2)) labs2 = trimws(labs2,'both')
    
    if (!is.null(na.strings)) {
      # convert to NA
      x[x %in% na.strings] = NA
      # also remember to remove na.strings from value labels 
      labs1 = labs1[! labs1 %in% na.strings]
      labs2 = labs2[! labs2 %in% na.strings]
    }
    
    # the levels will be reset here
    x = factor(x)
    
    if (! is.null(lab)) attr(x, 'label') <- lab
    if (! is.null(labs1)) attr(x, 'labels') <- labs1
    if (! is.null(labs2)) attr(x, 'value.labels') <- labs2
  } else if (is.character(x)) {
    lab = attr(x, 'label', exact = T)
    labs1 <- attr(x, 'labels', exact = T)
    labs2 <- attr(x, 'value.labels', exact = T)
    
    # trimws will convert factor to character
    x = trimws(x,'both')
    if (! is.null(lab)) lab = trimws(lab,'both')
    if (! is.null(labs1)) labs1 = trimws(labs1,'both')
    if (! is.null(labs2)) labs2 = trimws(labs2,'both')
    
    if (!is.null(na.strings)) {
      # convert to NA
      x[x %in% na.strings] = NA
      # also remember to remove na.strings from value labels 
      labs1 = labs1[! labs1 %in% na.strings]
      labs2 = labs2[! labs2 %in% na.strings]
    }
    
    if (! is.null(lab)) attr(x, 'label') <- lab
    if (! is.null(labs1)) attr(x, 'labels') <- labs1
    if (! is.null(labs2)) attr(x, 'value.labels') <- labs2
  } else {
    x = x
  }
  return(x)
}
