#!/bin/env ruby

require 'ffi'
require 'msgpack'

module Test1
  extend FFI::Library
  ffi_lib './dist/build/call-haskell-from-anything.so/call-haskell-from-anything.so'

  attach_function :f1_t_export, [:string], :string
  attach_function :fib_export, [:int], :int

  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :hs_exit, [], :void

  def self.make_msgpack_fun(fun_sym)
    attach_function fun_sym, [:string], :string

    proc { |*args|
      MessagePack.unpack(method(fun_sym).call(MessagePack.pack(args)))
    }
  end
end

Test1.hs_init(nil, nil)

msg = [1,2.23].to_msgpack
resmsg = Test1.f1_t_export(msg)

puts "Haskell said: #{MessagePack.unpack(resmsg)}"

fib = Test1.make_msgpack_fun(:fib_export)

puts "Haskell fib: #{fib.call(13)}"

sum = 0
(0..99999).each do |i|
  sum += fib.call(15)
end

puts sum

Test1.hs_exit
