## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE,warning = FALSE)
library(glober)
library(genlasso)
library(fda)
library(ggplot2)
library(plot3D)

## ----observations1D-----------------------------------------------------------
## --- Loading the values of the input variable --- ##
data('x_1D')

## ----y1D----------------------------------------------------------------------
## --- Loading the corresponding noisy values of the response variable --- ##
data('y_1D')

## ----xpred_1D-----------------------------------------------------------------
## --- Loading the values of the input variable for which an estimation 
## of f_1 is required --- ##
data('xpred_1D')
## --- Loading the corresponding evaluations to plot the function --- ##
data('f_1D')

## -----------------------------------------------------------------------------
## -- Building dataframes to plot -- ##
data_1D = data.frame(x = xpred_1D, f = f_1D)
obs_1D = data.frame(x = x_1D, y = y_1D)
real.knots = c(0.1, 0.27,0.745)

## ---- echo=FALSE, out.width="50%", fig.align = 'center'-----------------------
ggplot(data_1D, aes(x, f)) +
  geom_line(color = 'red') +
  geom_point(aes(x, y), data =obs_1D, shape= 4, color = "blue", size = 4)+
  geom_vline(xintercept = real.knots, linetype = 'dashed', color = 'grey27') +
  xlab('x') +
  ylab('y') +
  theme_bw()+
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size =20),
        axis.text.x = element_text(size=19),
        axis.text.y = element_text(size=19))

## -----------------------------------------------------------------------------
res = glober.1d(x = x_1D, y = y_1D, xpred = xpred_1D, ord = 3, parallel = FALSE)

## -----------------------------------------------------------------------------
fhat = res$festimated
head(fhat)

## -----------------------------------------------------------------------------
res$rss

## -----------------------------------------------------------------------------
res$rsq

## -----------------------------------------------------------------------------
knots.set = res$Selected.knots
print(knots.set)

## ---- out.width="50%", fig.align = 'center'-----------------------------------
## Dataframe of selected knots ##
idknots = which(xpred_1D %in% knots.set)
yknots = f_1D[idknots]
data_knots = data.frame(x.knots = knots.set, y.knots = yknots)
## Dataframe of the estimation ##
data_res = data.frame(xpred = xpred_1D, fhat = fhat)

plot_1D = ggplot(data_1D, aes(xpred_1D, f_1D)) +
    geom_line(color = 'red') +
    geom_line(data = data_1D, aes(x = xpred_1D, y = fhat), color = "black") +
    geom_vline(xintercept = real.knots, linetype = 'dashed', color = 'grey27') +
    geom_point(aes(x, y), data = obs_1D, shape = 4, color = "blue", size = 4)+
    geom_point(aes(x.knots, y.knots), data = data_knots, shape = 19, color = "blue", 
               size = 4)+
    xlab('x') +
    ylab('y') +
    theme_bw()+
    theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 19),
          axis.text.y = element_text(size = 19))
plot_1D

## -----------------------------------------------------------------------------
## --- Loading the values of the input variables --- ##
data('x_2D')
head(x_2D)

## -----------------------------------------------------------------------------
## --- Loading the corresponding noisy values of the response variable --- ##
data('y_2D')

## ----xpred_2D-----------------------------------------------------------------
## --- Loading the values of the input variables for which an estimation 
## of f_2 is required --- ##
data('xpred_2D')
head(xpred_2D)
## --- Loading the corresponding evaluations to plot the function --- ##
data('f_2D')

## ---- message=FALSE, fig.align = 'center',echo=FALSE, out.width="60%", out.height="60%"----
scatter3D(xpred_2D[,1], xpred_2D[,2], f_2D, bty = "g", pch = 18, col = gg.col(100),
          theta = 180, phi = 10)

## -----------------------------------------------------------------------------
res = glober.2d(x = x_2D, y = y_2D, xpred = xpred_2D, ord = 3, parallel = FALSE)

## -----------------------------------------------------------------------------
fhat_2D = res$festimated
head(fhat_2D)

## -----------------------------------------------------------------------------
res$rss

## -----------------------------------------------------------------------------
res$rsq

## -----------------------------------------------------------------------------
knots.set = res$Selected.knots
print('For the first dimension:')
print(knots.set[[1]])
print('For the second dimension:')
print(knots.set[[2]])

## ---- fig.align = 'center', out.width="40%", out.height="40%"-----------------
scatter3D(xpred_2D[,1], xpred_2D[,2], f_2D, bty = "g", pch = 18, col = 'red',
          theta = 180, phi = 10)

## ---- fig.align = 'center', out.width="40%", out.height="40%"-----------------

scatter3D(xpred_2D[,1], xpred_2D[,2], fhat_2D, bty = "g", pch = 18, col = 'forestgreen',
          theta = 180, phi = 10)

