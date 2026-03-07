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

void main()
{
    putln(sum(0, 101))
}
