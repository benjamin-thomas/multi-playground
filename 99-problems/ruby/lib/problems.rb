# frozen_string_literal: true

=begin

irb -r ./lib/problems.rb

irb(main):001:0> extend Problems
=> main

irb(main):002:0> last([1,2,3])
=> 3

=end

module Problems
  extend self
  # Write a function last : 'a list -> 'a option that returns the last element of a list
  def last(lst)
    return if lst.empty?

    head, *tail = lst
    if tail.empty?
      head
    else
      last(tail)
    end
  end

  def last2(lst)
    inner = lambda do |rest, acc|

    end
  end
end
