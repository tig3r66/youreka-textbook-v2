library(animation)

# 95% CI
ani.options(interval=0.1, nmax=100,
            ani.width= 1500, ani.height=1000, ani.res = 200)
saveGIF(conf.int(0.95, main="Demonstration of 95% Confidence Intervals"),
        movie.name="ci95.gif")

# CLT
ani.options(interval=0.1, nmax=100,
            ani.width= 1500, ani.height=1000, ani.res = 200)
saveGIF(clt.ani(obs=300, mean=5),
        movie.name="clt.gif")

# least squares
ani.options(interval = 0.05, nmax = 100,
            ani.width= 1500, ani.height=1000, ani.res = 200)

saveGIF(least.squares(),
        movie.name="leastsq.gif")
