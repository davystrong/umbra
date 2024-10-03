// Gets the length of an arbitrary vector
#let _norm(p) = calc.sqrt(p.map(x => calc.pow(x.pt(), 2)).sum()) * 1pt
// Adds any number of arbitrary vectors
#let _add(..ps) = ps.pos().fold(none, (acc, x) => if acc == none { x } else { acc.zip(x).map(((y, z)) => y + z) })
// Takes the first vector and subtracts all subsequent vectors from it
#let _sub(..ps) = ps.pos().fold(none, (acc, x) => if acc == none { x } else { acc.zip(x).map(((y, z)) => y - z) })
// Rotates a 2D vector by the given angle
#let _rot(p, angle) = (
  p.first() * calc.cos(angle) - p.last() * calc.sin(angle),
  p.first() * calc.sin(angle) + p.last() * calc.cos(angle),
)
// Multiply (scale) a vector by some number
#let _mult(p, x) = p.map(y => x * y)
// Roll a vector by count positions, moving the overflow at the end back to the start
#let _roll(arr, count) = (arr.slice(count) + arr).slice(0, arr.len())