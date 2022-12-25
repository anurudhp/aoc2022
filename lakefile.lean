import Lake
open Lake DSL

package aoc {
  -- add package configuration options here
}

lean_lib Aoc {
  -- add library configuration options here
}

@[defaultTarget]
lean_exe aoc {
  root := `Main
}

lean_exe day01 { root := `Aoc.Day01 }
lean_exe day02 { root := `Aoc.Day02 }
lean_exe day03 { root := `Aoc.Day03 }
lean_exe day04 { root := `Aoc.Day04 }
lean_exe day05 { root := `Aoc.Day05 }
lean_exe day06 { root := `Aoc.Day06 }
lean_exe day07 { root := `Aoc.Day07 }
lean_exe day08 { root := `Aoc.Day08 }
lean_exe day09 { root := `Aoc.Day09 }
lean_exe day10 { root := `Aoc.Day10 }
lean_exe day11 { root := `Aoc.Day11 }
lean_exe day12 { root := `Aoc.Day12 }
lean_exe day13 { root := `Aoc.Day13 }
lean_exe day14 { root := `Aoc.Day14 }
lean_exe day15 { root := `Aoc.Day15 }
lean_exe day16 { root := `Aoc.Day16 }
lean_exe day17 { root := `Aoc.Day17 }
lean_exe day18 { root := `Aoc.Day18 }
lean_exe day19 { root := `Aoc.Day19 }
lean_exe day20 { root := `Aoc.Day20 }
lean_exe day21 { root := `Aoc.Day21 }
lean_exe day22 { root := `Aoc.Day22 }
lean_exe day23 { root := `Aoc.Day23 }
lean_exe day24 { root := `Aoc.Day24 }
lean_exe day25 { root := `Aoc.Day25 }