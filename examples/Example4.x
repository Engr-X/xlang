/*
access keywords: public / private

1) public:
   symbol can be accessed from outside (default is public if omitted).

2) private:
   symbol is intended for internal use in current scope/module.
*/

public val apiVersion: int = 1
private var internalCounter: int = 0
val defaultPublicValue: int = 42 // no `public` keyword, still public by default

public fun inc() -> void
{
    internalCounter = internalCounter + 1
}

private fun reset() -> void
{
    internalCounter = 0
}

void main()
{
    inc()
    inc()
    reset()
    inc()
}
