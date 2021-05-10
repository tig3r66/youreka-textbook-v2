library(animation)

# 95% CI
ani.options(interval=0.1, nmax=100)
saveGIF(conf.int(0.95, main="Demonstration of 95% Confidence Intervals"),
        movie.name="ci95.gif")

# CLT
ani.options(interval=0.1, nmax=100)
saveGIF(clt.ani(obs=300, mean=5),
        movie.name="clt.gif")
