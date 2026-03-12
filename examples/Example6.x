/*
if statement examples

1) single statement style:
   if condition: statement

2) if + else (single statement with ':'):
   if condition: statement
   else: statement

3) multi-statement style (use block):
   if condition { ... } else { ... }
*/

void main()
{
    var score: int = 86
    var passed: bool = false
    var level: String = "none"

    // 1) single statement after ':'
    if score > 59: passed = true

    // 2) if + else with single statement
    if score > 89: level = "A"
    else: level = "B"

    // 3) block style for multiple statements
    if score > 100 {
        level = "S"
        passed = true
    } else {
        level = "normal"
    }

    // 4) nested if in else block
    if score > 89: level = "A"
    else {
        if score > 79: level = "B"
        else: level = "C"
    }
}
