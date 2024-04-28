#!/usr/bin/env ruby

require 'ffi'

module Argon2
  extend FFI::Library
  ffi_lib 'libargon2.so'

  attach_function :argon2id_verify, [:string, :pointer, :size_t], :int
end

def verify_password(encoded_hash, password)
  password_ptr = FFI::MemoryPointer.from_string(password)

  warn("Password size: #{password.bytesize}")
  result = Argon2.argon2id_verify(encoded_hash, password_ptr, password.bytesize)
  warn("Verify result: #{result}")

  password_ptr.free
  result == 0
end

encoded_hash = "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw"
password = ARGV.first
if password.nil?
  puts "Usage: #{$PROGRAM_NAME} <password>"
  exit 1
end

if verify_password(encoded_hash, password)
  puts "Password is correct!"
else
  puts "Password is incorrect!"
end
