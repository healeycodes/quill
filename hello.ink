` Ink prime sieve `

` is a single number prime? `
prime? := n => (
	` is n coprime with nums < p? `
	max := floor(pow(n, 0.5)) + 1
	(ip := p => p :: {
		max -> true
		_ -> n % p :: {
			0 -> false
			_ -> ip(p + 1)
		}
	})(2)
)

` primes under N are numbers 2 .. N, filtered by prime? `
getPrimesUnder := n => filter(range(2, n, 1), prime?)

` log result `
primes := getPrimesUnder(100)
log(f('Primes under 100: {{ 0 }}', [stringList(primes)]))