# Usage:
# plot2(y ~ x | panels | styles,  data, graphical parameters, ...)

# ==============================================================================

require("colorspace")
source("formlist.R")
source("addresponse.R")
source("dateAxis.R")
#source("bwplot.R")

#{{ PRE-PANEL FUNCTIONS

prepanel.default <-
function(x, y, xlab, ylab, main, ...) {

}

#}}

#{{ POST-PANEL FUNCTIONS

postpanel.default <-
function(x, y, xlab, ylab, main, ...) {
    Axis(x = x, side = 1)
    Axis(x = y, side = 2)
    box()
    title(main)
}

postpanel.timeline <-
function(x, y, xlab, ylab, main, ...) {
    axis(2)
    dateAxis()
    box()
    title(main)
}

#}}

#{{ PANEL FUNCTIONS

panel.points <-
function(x, y, pch, cex, col.pt, bg, lwd, ...) {
    points(x, y, pch = pch, cex = cex,
        col = col.pt, bg = bg,
        lwd = lwd)
}

panel.matrix <-
function(x, y, pch, cex, col.pt, bg, lwd, col.line, lty,
    type = "l", ...) {
    matplot(x, y, pch = pch, cex = cex,
        col = col.pt, bg = bg,
        type = type,
        lwd = lwd, add = TRUE)
}


panel.confint <-
function(x, y, pch, cex, col.pt, bg, lwd,
    col.line, lty, col.border, lty.border, lwd.border = lwd,
    bg.pt, lend,
    layer = 1,
    ...) {
    
    stopifnot(is.matrix(y) && ncol(y) >= 3L)
    
    switch(layer,
           # layer 1
           {
           .cipoly(x, y[, 1L], y[, 2L], y[, 3L],
                bg = bg, col = col.line, lty = lty, lwd = lwd, border = col.border,
                lty.border = lty.border, lwd.border = lwd.border,
                pch = NULL, bg.pt = bg.pt, cex.pt = cex, col.pt = col.pt,
                lend = lend)
           }, {
           # layer 2
            points(x, y[, 1L], pch = pch, cex = cex,
                   col = col.pt, bg = bg.pt, lwd = lwd)
           }, warning("maximum 2 layers"))
}

panel.bw <-
function(x, y, pch, cex, col.pt, bg, lwd,
    col.line, lty, col.border, lty.border, lwd.border = lwd,
    bg.pt, lend,
    ...) {
    .NotYetImplemented()
    bwplot(x, y[, 1L], y[, 2L], y[, 3L],
        bg = bg, col = col.line, lty = lty, lwd = lwd, border = col.border,
        lty.border = lty.border, lwd.border = lwd.border,
        pch = pch, bg.pt = bg.pt, cex.pt = cex, col.pt = col.pt,
        lend = lend)
}
#}}

plot2 <-
function(formula, ...) {
    UseMethod("plot2")
}

coloradjust <-
function(x, darken = 0, alpha = NULL) {
    if(!is.null(alpha)) {
        alpha <- rep(alpha, length.out = length(x))
        ok <- !is.na(alpha)
        x[ok] <- colorspace::adjust_transparency(x[ok], alpha[ok])
    }
    colorspace::lighten(x, amount = -darken, method = "relative", space = "combined")
}

.is.named <- \(x) !is.null(names(x)) && any(nzchar((names(x))))



# .fillarg - allows for incomplete multi-element arguments
# argument can be a named subset of the required value or unnamed, then it is
# matched positionally, with NA for default values.


#test <- \(a = c(item1 = -1, item2 = -2, item3 = -3), ok = 1:3) {
#    .fillarg(argname = "a")
#}
#print((test(a = c(item3 = 3, item1= 2)))) # --> (new, default, new)
#print((test(a = c(NA, 2, 3)))) # --> (default, new2, new3)


.fillarg <-
function(argname,
  formalargs = formals(definition),
  call = match.call(definition = definition, call = sys.call(which)),
  definition = sys.function(which), which = sys.parent(1L)
  ) {
    x0 <- eval.parent(formalargs[[argname]])
    if(!hasName(call, argname))
        return(x0)
    x <- get(argname, parent.frame(), inherits = FALSE)
    mode(x) <- mode(x0)
    if(.is.named(x)) {
        x <- x[!is.na(x)]
        i <- match(names(x), names(x0), nomatch = 0L)
        if(any(i == 0L))
            warning(sprintf("%s[%s] are invalid and were omitted", argname, paste0(which(i == 0L), collapse = ",")))
        x0[i] <- x[i != 0L]
    } else {
        warning(sprintf("unnamed '%s'", argname))
        x <- x[seq_along(x0)]
        i <- !is.na(x)
        x0[i] <- x[i]
    }
    x0
}


plot2.default <-
function(formula, xvar, yvar, gvar, hvar = NULL,
    xlim = range(formula[[xvar]], finite = TRUE),
    ylim = range(formula[[yvar]], finite = TRUE),
    layout = TRUE,
    type = c("point", "line", "cipoly", "bw"),
    nlayers = NULL,
    panel = if(is.matrix(formula[[yvar]])) panel.matrix else panel.points,
    pre.panel = prepanel.default,
    post.panel = postpanel.default,

    col = palette(nh, alpha = alpha),
    
    pch = seq.int(1L, length.out = min(nh, 25L)),
    cex = 1,
    col.pt = coloradjust(col, darken = darken["pt"], alpha = alpha["pt"]),
    bg.pt = coloradjust(col, darken = darken["bg.pt"], alpha = alpha["bg.pt"]),

    col.line = coloradjust(col, darken = darken["line"], alpha = alpha["line"]),
    lty = "solid",
    lwd = par("lwd"),
    bg = coloradjust(col, darken = darken["bg"], alpha = alpha["bg"]),

    col.border = col.line,
    lty.border = "dotted",
    lwd.border = lwd,
    lend = 3,
    palette = hcl.colors,
    
    darken = c(pt = 0.5, bg.pt = 0, line = 0.5, bg = -0.1),
    alpha = c(pt = 1, bg.pt = 0.5, line = 1, bg = 0.5),
   
    ...) {
    
    if(!missing(type)) .NotYetUsed("type")
    
    fa <- formals()
    cl <- match.call()
    alpha <- .fillarg("alpha", fa, cl)
    darken <- .fillarg("darken", fa, cl)
    
    if(is.null(hvar)) {
        nh <- 1L
    } else {
        formula[hvar] <- lapply(formula[hvar], as.factor)
        nh <- prod(vapply(formula[hvar], nlevels, NA_integer_))
    }

    .plot2(data = formula, xvar = xvar, yvar = yvar, gvar = gvar, hvar = hvar,
        nh = nh,
        xlim = xlim, ylim = ylim,
        layout = layout, nlayers = nlayers,
        panel = panel, pre.panel = pre.panel, post.panel = post.panel,
        col.pt = col.pt, bg.pt = bg.pt, pch = pch, cex = cex, 
        bg = bg,
        col.line = col.line, lty = lty, lwd = lwd,
        col.border = col.border, lty.border = lty.border, lwd.border = lwd.border,
        lend = lend,
        ...)
}


plot2.formula <-
function(formula, data, subset, drop.unused.levels = FALSE, ...) {
    fl <- formlist(formula)
    stopifnot(attr(fl,"mform.style") == "|")
    
    cl <- match.call()
    cl[[1L]] <- as.name("model.frame")
    cl <- cl[c(TRUE, names(cl)[-1L] %in% names(formals(model.frame.default)))]
    cl$formula <- formula.formlist(fl, style = "combined", response = getresponse(fl[[1L]]))    
    
    mf <- eval.parent(cl)
    
    varnames <- lapply(fl, \(f) rownames(attr(terms(f),"factors")))
    varnames[-1L] <- lapply(varnames[-1L], "[", -1L)
    plot2.default(mf,
        xvar = varnames[[c(1L, 2L)]],
        yvar = varnames[[c(1L, 1L)]],
        gvar = varnames[[2L]],
        hvar = if(length(varnames) < 3L) NULL else varnames[[3L]], ...)
}

.plot2 <-
function(data, xvar, yvar, gvar, hvar = NULL,
    xlim = range(data[[xvar]], finite = TRUE),
    ylim = range(data[[yvar]], finite = TRUE),
    layout = TRUE,
    nlayers = NULL,
    log = "", asp = NA,
    nh = NULL,
    panel = panel.points,
    pre.panel = prepanel.default,
    post.panel = postpanel.default,
    ...) {
    
    if(is.null(nh)) {
        if(is.null(hvar)) {
            nh <- 1L
        } else {
            data[hvar] <- lapply(data[hvar], as.factor)
            nh <- prod(vapply(data[hvar], nlevels, NA_integer_))
        }
    }

    
    dots <- list(...)
    panelargnames <- names(formals(panel))
    
    nlayers <- if(is.null(nlayers) || ! "layer" %in% panelargnames) 1L else
        if(is.numeric(nlayers) && nlayers > 0L) as.integer(nlayers) else
            stop("invalid value of 'nlayers'")
    
    dots.panel <- lapply(
        dots[names(dots) %in% panelargnames],
        rep, length.out = nh)
    dots.pre <- dots[names(dots) %in% names(formals(pre.panel))]
    dots.post <- dots[names(dots) %in% names(formals(post.panel))]
    
    nz <- nrow(data)
    
    zz <- split(seq_len(nz), data[gvar], drop = TRUE)
    if(!is.null(layout)) {
        mfrow <- if(isTRUE(layout))
            n2mfrow(length(zz)) else
                if(is.numeric(layout)) 
                    layout
        par(mfrow = mfrow)
    }
    
    invisible(lapply(zz, \(i) {

        plot.new()
        plot.window(xlim, ylim, asp = asp, log = log)
        x <- data[i, xvar]
        y <- data[i, yvar]
        
        maintitle <- paste0(gvar, ": ", sapply(data[i[1L], gvar], as.character), collapse = " ")
       
        do.call("pre.panel", c(list(x = x, y = y,
            xlab = xvar, ylab = yvar, main = maintitle,
            data = data, index = i), dots.pre))
        
        if(is.null(hvar)) {
            for(layer in seq_len(nlayers))
                do.call(panel, c(list(x, y, layer = layer), dots.panel))
            #panel(x, y, ...)
        } else {
            js <- split(i, data[i, hvar], drop = FALSE)
            ok <- lengths(js, use.names = FALSE) != 0L

            for(layer in seq_len(nlayers))
                .mapply(\(j, layer, ...) {
                        panel(data[j, xvar], data[j, yvar, drop = TRUE],
                            layer = layer, ...)
                    }, c(list(j = js[ok]), lapply(dots.panel, "[", ok)),
                    MoreArgs = list(layer = layer))
                
        }
        
        do.call("post.panel", c(list(x = x, y = y,
            xlab = xvar, ylab = yvar, main = maintitle,
            data = data, index = i), dots.post))
        
        list(x = x, y = y, xlim = xlim, ylim = ylim, mfg = par("mfg"),
             main = maintitle)
    }))
}
