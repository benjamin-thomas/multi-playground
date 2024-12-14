require 'awesome_print'

example = <<EOF
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
EOF

prepare = lambda do |str|
  str.lines.map(&:strip).map do |line|
    line.split.map(&:to_i)
  end
end

def is_safe(levels)
  deltas = levels.each_cons(2).map do |a, b|
    b - a
  end
  all_neg = deltas.all? { |n| n < 0 }
  all_pos = deltas.all? { |n| n > 0 }
  in_range = deltas.all? { |n| n.abs.between?(0, 3) }
  (all_neg || all_pos) && in_range
end

def is_safe2(levels)
  levels.size.times.any? do |i|
    lst = levels.filter.with_index { |_, i2 | i != i2 }
    is_safe(lst)
  end
end

ex = prepare.(example)
ap(ex.map(&method(:is_safe2)))
# ap(is_safe4 ex[5])

content = File.read("../_inputs/02.txt")
print("Part 1: ")
ap(prepare.(content).count(&method(:is_safe))) # 421
print("Part 2: ")
ap(prepare.(content).count(&method(:is_safe2))) # 476