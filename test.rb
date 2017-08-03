#!/bin/env ruby

require 'ffi'
require 'msgpack'

# For finding where the built .so file is, independent on whether it was built with stack or cabal
require 'find'
def find_file_ending_with(ending_with_str, path='.')
  Find.find('.') do |path|
    if path.end_with? ending_with_str
      return path
    end
  end
  abort("Could not find " + ending_with_str + " in " + path)
end


module Test1
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  attach_function :free, [:pointer], :void

  ffi_lib find_file_ending_with('build/call-haskell-from-anything.so/call-haskell-from-anything.so')

  attach_function :f1_t_export, [:pointer], :pointer
  attach_function :fib_export, [:pointer], :pointer

  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :hs_exit, [], :void

  def self.make_msgpack_fun(fun_sym)
    attach_function fun_sym, [:pointer], :pointer

    proc { |*args|
      packed = MessagePack.pack(args)
      length_64bits = [packed.length].pack("q>") # big-endian
      resptr = method(fun_sym).call(length_64bits + packed)
      msg_length = resptr.read_string_length(8).unpack("q>")[0]
      resmsg = (resptr + 8).read_string_length(msg_length)
      Test1.free(resptr)
      MessagePack.unpack(resmsg)
    }
  end
end

Test1.hs_init(nil, nil)

msg = [1,2.23].to_msgpack
length_64bits = [msg.length].pack("q>") # big-endian
resptr = Test1.f1_t_export(length_64bits + msg)
msg_length = resptr.read_string_length(8).unpack("q>")[0]
resmsg = (resptr + 8).read_string_length(msg_length)
Test1.free(resptr)

puts "Haskell said: #{MessagePack.unpack(resmsg)}"

fib = Test1.make_msgpack_fun(:fib_export)

puts "Haskell fib: #{fib.call(13)}"

sum = 0
(0..99999).each do |i|
  sum += fib.call(15)
end

puts sum

Test1.hs_exit
