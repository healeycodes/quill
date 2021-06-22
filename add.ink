factorial := n => n :: {
    0 -> 1
    _ -> n * factorial(n-1)
}
factorial(5)