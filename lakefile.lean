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