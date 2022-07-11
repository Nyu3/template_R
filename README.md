# template_R
easy handler for R manipulation <br>

## Examples <br>
*"pp.()"* function will be useful copy & paste tech when you don't feel like using bothersome read.csv() <br>
Others are; <br>

    plt.(iris[4:5])
    plt.(iris[-5], legePos = c(0.01, 0.99), lty = 1)
    iplot.(us_rent_income[c(2,5)], rot = 35)
    dens.(iris[4:5], cum = F)
    crp.(iris[2:3])
    hist.(iris[2:3], col = c('slateblue', 'coral2'), bin = 0.1, name = c('A', 'B'), overlay = T)
    corp.(iris[3:4])
    ellip.(iris)
    box2.(iris, rot = 20, pareto = T, cut = T)
    box2.(diamonds[1:1000, 1:3], mark = 'color')
    box2.(id2y.(diamonds[1:1000, 1:3]))
    box2.(case2.(us_rent_income[5], div = 100), col = 0)
    box2.(time2.(economics[1:50, ], div = 'year'))
    barp.(iris, xyChange = T, rot = 25)
    barp.(iris, cum = T, xyChange = T)
    pie.(iris[41:120,5], percent = T)
    sp.(iris, col = 3)
    stats.(iris)
    smry.(iris, f = 'sd(x) / mean(x)')
    html.(starwars)
    ...
