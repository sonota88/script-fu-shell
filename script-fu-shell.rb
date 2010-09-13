#!/usr/bin/ruby
# -*- coding: utf-8 -*-

=begin

= COPYRIGHT

Copyright (c) 2009-2010 sonota <yosiot8753@gmail.com>. All rights reserved.
This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

= ACKNOWLDGEMENTS

Derived from a Perl Module Gimp::ScriptFu::Client (Version 1.01, Feb 6, 2007) by Alan Stewart <astewart1@cox.net>.

=end


require "pp"
require "socket"
require "readline"


class ScriptFuShell
  attr_accessor :debug, :error_code

  def initialize
    @soc = TCPSocket.open("localhost", 10008)
    @paren_depth = 0
    @out_of_string = true
    @normal_prompt = "> "
    @continuous_prompt = "| "
    @prompt = @normal_prompt
    @sexp = ""

    @debug = $DEBUG
    @error_code = nil
  end


  def byte2i(x)
    x.unpack("C").first.to_i
  end


  def send_raw(script)
    header = ["G", script.length].pack("an")
    pp [ ["G", script.length].pack("an") ] if @debug
    @soc.write header
    @soc.write script

    #

    IO::select( [@soc], nil, nil, nil)

    magic       = @soc.read(1)
    @error_code = @soc.read(1)
    high_byte   = @soc.read(1)
    low_byte    = @soc.read(1)

    pp [magic, byte2i(@error_code), byte2i(high_byte), byte2i(low_byte)] if @debug

    length = byte2i(high_byte) * 256 + byte2i(low_byte)
    
    @soc.read(length)
  end
  

  def send(script)
    return "<no input>" if /\A\s*\Z/m =~ script
    puts script.strip
    send_raw %Q{(return-item-tostring #{script.strip})}
  end


  def sexp_closed?(line)
    arr = line.split(//)
    pos = 0
    
    loop do
      case arr[pos]
      when "\\"
        pos += 1
      when '"'
        @out_of_string = @out_of_string ? false : true
      when "("
        @paren_depth += 1 if @out_of_string
      when ")"
        @paren_depth -= 1 if @out_of_string
      end

      pos += 1
      break if pos >= line.size
    end

    return ((@paren_depth == 0) ? true : false) && @out_of_string
  end


  def script_fu_init
    send_raw(
      <<-EOB
      (define (return-item-tostring item)
        (cond
          ((eq? #f item) "#f")
          ((eq? #t item) "#t")
          ((string? item) (string-append "\\"" item "\\""))
          ((number? item) (number->string item))
          ((procedure? item) "<procedure>")
          ((vector? item) "<vector>")
          ((null? item) "()")
          ((symbol? item) (string-append
            "<symbol> "
            (symbol->string item)))
          ((pair? item) (string-append 
              "("
              (unbreakupstr
                (map return-item-tostring item)
                " ")
              ")"
              ))
          (else item)))
      EOB
    )
  end
  

  def run
    script_fu_init()
    
    while line = Readline.readline( @prompt, true)
      if sexp_closed?(line)
        result = send(@sexp + line)
        print "=> ", result, "\n"
        
        @paren_depth = 0
        @out_of_string = true
        @sexp = ""
        @prompt = @normal_prompt
      else
        @sexp += line + "\n"
        @prompt = @paren_depth.to_s + @continuous_prompt
        puts "sexp: <<#{@sexp}>>" if @debug
      end
    end
  end
end


if $0 == __FILE__
  ScriptFuShell.new.run
end
