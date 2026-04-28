fun main(args: Array<String>)
{
    val names = listOf("Tom", "Tim", "Alice")

    val r = names.groupBy { it.length }
    println(r)
}
