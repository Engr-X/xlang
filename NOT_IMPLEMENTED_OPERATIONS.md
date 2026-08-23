# Not Implemented Operations

This file tracks operations that are planned, but not needed for the first compiler version.

## Deferred Bitwise Assignment Sugar

| Token / keyword | Planned desugar |
| --- | --- |
| `~=` | `assign(x, inv(x))` |
| `&=` | `assign(x, bitwiseAnd(x, y))` |
| `!&=` | `assign(x, inv(bitwiseAnd(x, y)))` |
| `^=` | `assign(x, bitwiseOr(bitwiseAnd(x, inv(y)), bitwiseAnd(inv(x), y)))` |
| `!^=` | `assign(x, bitwiseOr(bitwiseAnd(x, y), bitwiseAnd(inv(x), inv(y))))` |
| `|=` | `assign(x, bitwiseOr(x, y))` |
| `!|=` | `assign(x, inv(bitwiseOr(x, y)))` |

## Deferred Logical Operators

| Token / keyword | Planned desugar |
| --- | --- |
| `->` | `logicalOr(not(x), y)` |
| `!->` | `logicalAnd(x, not(y))` |
| `<->` | `logicalOr(logicalAnd(x, y), logicalAnd(not(x), not(y)))` |
| `!<->` | `logicalOr(logicalAnd(x, not(y)), logicalAnd(not(x), y))` |
| `!&&` | `not(logicalAnd(x, y))` |
| `!||` | `not(logicalOr(x, y))` |

## Deferred Named Bitwise Operators

| Token / keyword | Planned desugar |
| --- | --- |
| `nor` | `inv(bitwiseOr(x, y))` |
| `xnor` | `bitwiseOr(bitwiseAnd(x, y), bitwiseAnd(inv(x), inv(y)))` |

## First Version Notes

Keep first-version work focused on the common operators: plain assignment, arithmetic compound assignment, arithmetic, comparison, equality, `&&`, `||`, `and`, `nand`, `or`, and `xor`.