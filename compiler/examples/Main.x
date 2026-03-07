/* int fib(int n)
{
    if (n < 2):
        return n
    
    return fib(n - 1) + fib(n - 2)
}

fun add(a: int, b: int) -> int = a + b

int sum(int from, int to)
{
    sum = 0

    while (from < to)
    {
        sum = add(sum, from)
        from = add(from, 1)
    }

    return sum
}

bool isPrime(int i)
{
    test = 2

    while (test < i)
    {
        if (i % test == 0):
            return false
    }

    return true
} */

void findPrime(int from, int to)
{
    while (from < to)
    {
        if (from == 2):
            putln(from)
        else
        {
            i = 2
            isPrime = true

            while (i < from)
            {
                if (from % i == 0):
                    isPrime = false
                i = i + 1
            }

            if (isPrime):
                putln(from)
        }

        from = from + 1
    }
}

void main()
{
    findPrime(2, 1000)
}
