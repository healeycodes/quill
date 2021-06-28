log := msg => out(string(msg) + char(10))

` recursive via tail call optimization `
factorial := n => n :: {
    0 -> 1
    _ -> n * factorial(n-1)
}
factorial(5)

` event loop examples `
w := (t, c) => wait(t, () => log(c))
w(0.1, 'a')
w(0.2, 'b')
w(0.3, 'c')
w(0.35, 'd')
w(0.4, 'e')
w(0.45, 'f')

` this prints before the wait() calls `
log(string(time()))

` a map `
observation := {
    weather: 'Sunny',
    'observedAt': {
        time: time()
    }
}

log(observation.weather)
observation.('weather') := 'Raining'
log(observation.weather)
log(observation.observedAt) ` composite value stringing `
