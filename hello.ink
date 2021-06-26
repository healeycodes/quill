factorial := n => n :: {
    0 -> 1
    _ -> n * factorial(n-1)
}
factorial(5)

w := (t, c) => wait(t, () => out(c))
w(0.1, 'a')
w(0.2, 'b')
w(0.3, 'c')
w(0.35, 'd')
w(0.4, 'e')
w(0.45, 'f')
out(string(time()))
