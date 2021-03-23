# template_R
easy handler for R manipulation <br>

## Examples <br>
*"pp.()"* function will be useful copy & paste tech when you don't feel like using bothersome read.csv() <br>
Others are; <br>

    plt.(iris[4:5])
    plt.(iris[-5], legePos = c(0.01, 0.99), lty = 1)
    dens.(iris[4:5], cum = F)
    crp.(iris[2:3])
    hist.(iris[2:3], col = c('slateblue', 'coral2'), bin = 0.1, name = c('A', 'B'), overlay = T)
    corp.(iris[3:4])
    box2.(iris[-5], col = 1:4, rot = 22, cut = T)
    box2.(case2.(iris[3], div = 3))
    box2.(timeSpliter.(economics[1:2], div = 'month', origin = F))
    barp.(iris, xyChange = T, rot = 25)
    barp.(iris, cum = T, xyChange = T)
    sp.(iris, col = 3)
    base_stats.(iris)
    pie.(iris[41:120,5], per = T)
    html.(starwars)
    ...
