void findPrime(int from, int to)
{
    while (from < to)
    {
        if (from == 2):
            putln(from)

        i = 2
        isPrime = true

        while (i < from)
        {
            if (from % i == 0):
                isPrime = false
        }

        if (isPrime):
             putln(from)

        from = from + 1
    }
}

void main()
{
}
