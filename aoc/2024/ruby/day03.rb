require 'awesome_print'

example1 = <<EOS
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
EOS

example2 = <<EOS
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
EOS
example2.chomp!

=begin
MatchData API:

match.begin(0) returns the cursor index at the beginning of the whole match
match.begin(1) returns the cursor index at the beginning of the first capture group 
match.begin("a") returns the cursor index at the beginning of the named capture group
 
And so...
match.end(0) returns the character position at the beginning of the whole match
=end

def part1(str)
  matches = []
  cursor = 0

  re = %r{mul\((?<a>\d+),(?<b>\d+)\)}
  while (match = re.match(str, cursor))
    matches << match
    cursor = match.end(0) # See doc comment above
  end

  matches.map { |m|
    m.values_at("a", "b").map(&method(:Integer)).each_cons(2).map { |a, b| a * b }
  }.flatten.sum
end

def part2(str)
  found = []
  cursor = 0
  activate = true

  re = %r{mul\((\d+),(\d+)\)|don't\(\)|do\(\)}
  while (match = re.match(str, cursor))
    cursor = match.end(0)

    case match[0]
    when "don't()"
      activate = false
      next
    when "do()"
      activate = true
      next
    else
      next if !activate
      found << match.captures.map(&method(:Integer))
    end
  end

  found.reduce(0) do |tot, m|
    tot + m.each_cons(2).sum { |a, b| a * b }
  end
end

puts("Example 1: #{part1(example1)}") # 161
puts("Example 2: #{part2(example2)}") # 48
puts("---")
content = File.read("../_inputs/03.txt")
puts("Part 1: #{part1(content)}") # 178794710
puts("Part 2: #{part2(content)}") # 76729637
