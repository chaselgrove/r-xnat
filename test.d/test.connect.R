test.anonymous <- function() {
    conn <- xnat.connection('https://central.xnat.org')
    conn$close()
}

test.login <- function() {
    conn <- xnat.connection('https://central.xnat.org', 'nosetests', 'nosetests')
    conn$close()
}

test.bad.password <- function() {
    checkException(xnat.connection('https://central.xnat.org', 'nosetests', 'bogus'))
}

# eof
