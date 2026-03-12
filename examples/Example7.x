/*
loop examples

1) while with block
2) while also supports ':' single-line style: while cond: statement
3) continue / break
4) while ... else (else also supports single-line ':')
*/

int main()
{
    var i: int = 0
    var sum: int = 0

    // 1) while with block + continue
    while i < 5 {
        i = i + 1
        if i == 3: continue
        sum = sum + i
    }

    // 2) while with ':' single-line style
    var x: int = 0
    while x < 3: x = x + 1

    // 3) break
    var n: int = 0
    while n < 10 {
        n = n + 1
        if n > 4: break
        sum = sum + n
    }

    // 4) while ... else
    var cond: bool = false
    while cond {
        sum = sum + 100
    } else: sum = sum + 1

    return sum + x + n
}
