def count_inversions(matrix, y, x, ref)
  cnt = 0
  matrix.each_with_index do |row, i|
    row.each_with_index do |cell, j|
      next if y > i || x > j
      curr = matrix[i][j]
      cnt += 1 if curr < ref
    end
  end
  cnt
end

def compute_inversions(matrix)
  cnt = 0
  matrix.size.times do |y|
    matrix[y].size.times do |x|
      cnt += count_inversions(matrix, y, x, matrix[y][x])
    end
  end
  cnt
end

def handle_case
  dim = ARGF.gets.to_i
  matrix = []
  dim.times do
    matrix << ARGF.gets.chomp.split.map(&:to_i)
  end
  compute_inversions(matrix)
end


#### TESTS ####################################################################

# echo ./main.rb | entr -c bash -c 'TEST=1 ruby ./main.rb'
require 'minitest'
class MainTest < Minitest::Test
  def test_hello
    assert_equal 0, compute_inversions([])
    assert_equal 0, compute_inversions([[]])

    assert_equal 0, compute_inversions(
      [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
    )

    assert_equal 2, compute_inversions(
      [
        [4, 3],
        [1, 4],
      ]
    )

    assert_equal 17, compute_inversions(
      [
        [4, 7, 9],
        [8, 2, 0],
        [9, 1, 4],
      ]
    )
  end
end

#### MAIN ####################################################################

if ENV['TEST'] == '1'
  require 'minitest/autorun'
else
  cnt = ARGF.gets.to_i
  cnt.times.each do |i|
    puts handle_case
  end
end