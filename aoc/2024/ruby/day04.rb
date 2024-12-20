require 'awesome_print'
require 'pry'

# cSpell:disable
example1 = <<EOS
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
EOS
# cSpell:enable

Point = Struct.new(:y, :x)

north   = [ Point.new(0,0), Point.new(-1, 0), Point.new(-2, 0), Point.new(-3, 0) ]
south   = [ Point.new(0,0), Point.new( 1, 0), Point.new( 2, 0), Point.new( 3, 0) ]
west    = [ Point.new(0,0), Point.new( 0,-1), Point.new( 0,-2), Point.new( 0,-3) ]
east    = [ Point.new(0,0), Point.new( 0, 1), Point.new( 0, 2), Point.new( 0, 3) ]

north_east = [ Point.new(0,0), Point.new(-1, 1), Point.new(-2, 2), Point.new(-3, 3) ]
north_west = [ Point.new(0,0), Point.new(-1,-1), Point.new(-2,-2), Point.new(-3,-3) ]
south_east = [ Point.new(0,0), Point.new( 1, 1), Point.new( 2, 2), Point.new( 3, 3) ]
south_west = [ Point.new(0,0), Point.new( 1,-1), Point.new( 2,-2), Point.new( 3,-3) ]

# I'm using lambda for it's closure capability (won't work with def)
answer1 = lambda do |str|
  matrix = str.lines.map{ |line| line.strip.chars }

  at = lambda do |y,x|
    offset = lambda do |p|
      new_y = y + p.y
      new_x = x + p.x
      if 0 <= new_y && new_y < matrix.size && 0 <= new_x && new_x < matrix.size then
        matrix[new_y][new_x]
      else
        :NO
      end
    end
    [ north,
      north_east,
      east,
      south_east,
      south,
      south_west,
      west,
      north_west
    ].map { |points| points.map(&offset) }
  end

  res = matrix.map.with_index do |row, y|
    row.map.with_index do |col, x|
      at.(y,x).count { |xs| xs.join == "XMAS" }
    end
  end

  res.flatten.sum
end

example2 = <<EOS
.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
EOS

=begin

(-1, -1) (-1, 0) (-1, 1)
( 0, -1) ( 0, 0) ( 0, 1)
( 1, -1) ( 1, 0) ( 1, 1)

=end

def answer2(str)
  matrix = str.lines.map{ |line| line.strip.chars }

  at = lambda do |y,x|
    offset = lambda do |dy, dx|
      new_y = y + dy
      new_x = x + dx
      if 0 <= new_y && new_y < matrix.size && 0 <= new_x && new_x < matrix.size then
        matrix[new_y][new_x]
      else
        :NO
      end
    end
    [
      [[-1, -1], [0, 0], [ 1, 1]],
      [[ 1, -1], [0, 0], [-1, 1]],
    ].map { |dir| dir.map { |y,x| offset.(y,x) }
  }
  end

  res = matrix.map.with_index do |row, y|
    row.map.with_index do |col, x|
      curr = at.(y,x)

      next 0 if col != "A" # not strictly necessary

      case [curr[0].join, curr[1].join]
      when ["MAS", "MAS"]
        1
      when ["SAM", "SAM"]
        1
      when ["MAS", "SAM"]
        1
      when ["SAM", "MAS"]
        1
      else
        0
      end
    end
  end

  res.flatten.sum
end

content = File.read("../_inputs/04.txt")
puts "Example1: #{answer1.(example1)}"
puts "Part1:    #{answer1.(content)}" # 2344
puts "Example2: #{answer2(example2)}"
puts "Part2:    #{answer2(content)}" # 1815
