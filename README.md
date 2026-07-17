Example usage of `plot2`:
---

```{r}

# create some data:
dat <- expand.grid(lapply(c(g1 = 3, g2 = 4, g3 = 2, g4 = 1), \(k) factor(LETTERS[1:k])))
dat <- dat[rep(seq_len(nrow(dat)), each = 20), ]
dat$x <- runif(nrow(dat))
dat <- dat[order(dat$x), ]
dat$y <- model.matrix(~ g2*g3*g1 + g1 * x + g2 * x + g3*x, dat) %*% runif(ncol(X), -5, 5)

plot2(
    cbind(y, y + 2, y - 4) ~ I(x + 1) | g2 | g1 + g3,
    data = dat,
    subset = !(g2 == "A" & g3 == "A"),
    lwd = 2, lend = 1,
    nlayers = 2,
    panel = panel.confint,
    pre.panel = \(col.grid, col.plotbg, ...) {
        usr <- par("usr")
        rect(usr[1L], usr[3L], usr[2L], usr[4L], col = col.plotbg,
             border = NA)
        grid(col = col.grid)
    },
    col.grid = "#ffffff33",
    col.plotbg = "#5b6f6c",
    asp  = NA,
    cex = 1.5,
    pch = 21, 
    #col.pt = "red",
    #bg.pt = "blue",
    darken = c(bg.pt = .4, line = .9, bg = -.2),
    alpha = c(bg = .7),
    palette = \(n, alpha = 1) hcl.colors(n, palette = "hawaii", alpha = alpha),
    )

```
