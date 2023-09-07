if ENV['COLOR'] == '1'
  # RubyMine doesn't like this gem, so I'll specify en env var if I run my tests from the command line:
  #   Current implementation of IntelliJ Minitest support conflicts with Minitest::Reporters
  require 'minitest/reporters'
  Minitest::Reporters.use! [Minitest::Reporters::DefaultReporter.new(:color => true)]
end

# RubyVM::InstructionSequence.compile_option = {
#   tailcall_optimization: true,
#   trace_instruction: false
# }
