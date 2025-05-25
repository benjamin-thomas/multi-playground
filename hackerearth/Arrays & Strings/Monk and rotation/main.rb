#!/usr/bin/env ruby

=begin

cat ./input.txt | DEBUG=1 ./main.rb

DEBUG=1 ./main.rb < ./input.txt
=end

def handle(n, k, lst)
  k.times do
    lst.unshift(lst.pop)
  end
  lst
end

times  = ARGF.gets.to_i
(1..times).each do |i|
  n, k = ARGF.gets.chomp.split.map(&:to_i)
  lst = ARGF.gets.chomp.split.map(&:to_i)
  puts(handle(n, k, lst).join(" "))
end