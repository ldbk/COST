loadRdata <-
function(rdata, liste) {
        myf <- function(chemin, acharger) {
            .nv <- new.env(parent = baseenv())
            load(chemin, .nv)
            for (i in acharger) {
                assign(i, get(i, env = .nv), envir = .GlobalEnv)
            }
        }
        myf(rdata, liste)
    }
