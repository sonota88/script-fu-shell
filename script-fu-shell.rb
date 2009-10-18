#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require "pp"
require "socket"
require "readline"


class ScriptFuShell
  attr_accessor :debug, :error_code

  def initialize
    @soc = TCPSocket.open("localhost", 10008)
#    @soc = TCPSocket.open("192.168.1.115", 10008)
    @paren_depth = 0
    @out_of_string = true
    @normal_prompt = "> "
    @continuous_prompt = "| "
    @prompt = @normal_prompt
    @sexp = ""

    @debug = true
    @error_code = nil
  end


  def byte2i(x)
    x.unpack("C").first
  end

  def send_raw(script)
    header = ["G", script.length].pack("an")
    pp [ ["G", script.length].pack("an") ] if @debug
    @soc.write header
    @soc.write script

    #

    IO::select( [@soc], nil, nil, nil)
    sleep 0.1

    magic       = @soc.read(1)
    @error_code = @soc.read(1)
    high_byte   = @soc.read(1)
    low_byte    = @soc.read(1)

    pp [magic, byte2i(@error_code), byte2i(high_byte), byte2i(low_byte)] if @debug

    length = low_byte.unpack("C").first.to_i
    
    @soc.read(length)
  end
  

  def send(script)
    return "<no input>" if /^\s*$/m =~ script
    puts script
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

=begin
          ((pair? item) (string-append 
              "("
              (unbreakupstr
                (list
                  (return-item-tostring (car item))
                  (return-item-tostring (cdr item)))
                " ")
              ")"))


      (define (return-item-tostring item)
        (cond
          ((pair? item) (string-append 
              "("
              (unbreakupstr
                (map return-item-tostring item)
                " ")
              ")"
              ))
          (else item)))


=end

  def script_fu_init
    send_raw(
      <<-EOB
      (define (return-item-tostring item)
        (cond
          ((eq? #f item) "#f")
          ((eq? #t item) "#t")
          ((string? item) item)
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
        @sexp += "\n " + line
        @prompt = @paren_depth.to_s + @continuous_prompt
      end
    end
  end
end


if $0 == __FILE__
  ScriptFuShell.new.run
end
