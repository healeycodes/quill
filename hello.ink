factorial := n => n :: {
    0 -> 1
    _ -> n * factorial(n-1)
}
factorial(5)

w := (c) => wait(0.2, () => c)
w('a')
w('b')
w('c')
