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
require "optparse"


class ScriptFuShell
  attr_accessor :debug, :error_code

  def initialize(options)
    @soc = TCPSocket.open(options[:server], options[:port])
    @paren_depth = 0
    @out_of_string = true
    @normal_prompt = "> "
    @continuous_prompt = "| "
    @prompt = @normal_prompt
    @sexp = ""

    @debug = $DEBUG
    @error_code = nil

    @verbose = options[:verbose]
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
    puts script.strip if @verbose
    send_raw %Q{(item->string #{script.strip})}
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
      <<EOB
(define (item->string item)
  (cond ((null? item) "'()")
        ((eq? #f item) "#f")
        ((eq? #t item) "#t")
        ((char? item) (string-append "#\\\\" (string item)))
        ((string? item) (string-append "\\"" item "\\""))
        ((number? item) (number->string item))
        ((symbol? item) (string-append
                         "'"
                         (symbol->string item)))
        ((vector? item) (string-append
                         "#("
                         (unbreakupstr (map item->string (vector->list item)) " ")
                         ")"))
        ((list? item) (string-append
                       "("
                       (unbreakupstr (map item->string item) " ")
                       ")"))
        ((pair? item) (string-append
                       "("
                       (item->string (car item))
                       " . "
                       (item->string (cdr item))
                       ")"))
        ((closure? item) "#<CLOSURE>")
        ((procedure? item) "#<PROCEDURE>")
        (else "<?>")))
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

  
  def one(exp)
    script_fu_init
    send(exp)
  end
end


if $0 == __FILE__
  options = {
    :verbose => false,
    :server => "127.0.0.1",
    :port => 10008
  }

  OptionParser.new {|opt|
    opt.on("-s", "--server=ADDRESS") {|v| options[:port] = v }
    opt.on("-p", "--port=PORT")      {|v| options[:port] = v.to_i }
    opt.on("-v", "--verbose")        {|v| options[:verbose] = true }
    opt.parse!(ARGV)
  }

  sh = ScriptFuShell.new(options)

  if ARGV[0] == "db"
    puts sh.one( '(cadr (gimp-procedural-db-query "" "" "" "" "" "" ""))' )
  else
    sh.run
  end
end
