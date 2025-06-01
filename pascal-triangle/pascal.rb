#!/usr/bin/env ruby

def gen_row(row)
  [1] + row.each_cons(2).map { |a, b| a + b } + [1]
end

def pascal(n)
  rows = [[1]]
  (n - 1).times do
    rows << gen_row(rows.last)
  end
  rows
end

def print_triangle(rows)
  width = rows.last.map(&:to_s).join(' ').length

  rows.each do |row|
    puts row.join(' ').center(width)
  end
end

print_triangle(pascal 6)
