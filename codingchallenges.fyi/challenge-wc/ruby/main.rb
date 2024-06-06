#!/usr/bin/env ruby

# rg --files | entr -c ./main.rb ../test.txt

module A
  def self.run
    ARGV.each do |file|
      byte_count = 0
      line_count = 0
      word_count = 0
      rune_count = 0
      File.readlines(file).each do |line|
        byte_count += line.bytesize
        line_count += 1
        word_count += line.split.count
        rune_count += line.chars.count
      end
      puts "module(#{name}) | #{file} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
    end
  end
end

module B
  def self.run
    ARGV.each do |file|
      lines = File.readlines(file)
      byte_count, line_count, word_count, rune_count =
        lines.reduce([0, 0, 0, 0]) do |(bytes, lines, words, runes), line|
          [ bytes + line.bytesize,
            lines + 1,
            words + line.split.count,
            runes + line.chars.count,
          ]
        end
      puts "module(#{name}) | #{file} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
    end
  end
end


module C
  def self.run
    ARGV.each do |file|
      lines = File.readlines(file)
      byte_count, line_count, word_count, rune_count =
        lines.reduce([0, 0, 0, 0]) do |(bytes, lines, words, runes), line|
          [ bytes + line.bytesize,
            lines + 1,
            words + line.split.count,
            runes + line.chars.count,
          ]
        end
      puts "module(#{name}) | #{file} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
    end
  end
end

module D
  extend self
  def run
    count_stats = lambda do |(bytes, lines, words, runes), line|
      [ bytes + line.bytesize,
        lines + 1,
        words + line.split.count,
        runes + line.chars.count,
      ]
    end

    ARGV.each do |file|
      lines = File.readlines(file)
      byte_count, line_count, word_count, rune_count =
        lines.reduce([0, 0, 0, 0], &count_stats)
      puts "module(#{name}) | #{file} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
    end
  end
end

module E
  extend self
  def run
    count_stats = -> ((bytes, lines, words, runes), line) {
      [ bytes + line.bytesize,
        lines + 1,
        words + line.split.count,
        runes + line.chars.count,
      ]
    }

    ARGV.each do |file|
      lines = File.readlines(file)
      byte_count, line_count, word_count, rune_count =
        lines.reduce([0, 0, 0, 0], &count_stats)
      puts "module(#{name}) | #{file} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
    end
  end
end

module F
  extend self
  private def count_stats((bytes, lines, words, runes), line)
    [ bytes + line.bytesize,
      lines + 1,
      words + line.split.count,
      runes + line.chars.count,
    ]
  end

  private def handle_path(path)
    lines = File.readlines(path)
      byte_count, line_count, word_count, rune_count =
        lines.reduce([0, 0, 0, 0], &method(:count_stats))
      puts "module(#{name}) | #{path} => (#{byte_count}, #{line_count}, #{word_count}, #{rune_count})"
  end

  def run
    ARGV.each(&method(:handle_path))
  end
end

A.run
B.run
C.run
D.run
E.run
F.run
