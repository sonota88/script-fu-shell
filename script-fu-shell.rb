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


class String
  def strip_quote
    self.sub(/\A"(.+)"\Z/m, '\1')
  end


  def indent_and_truncate(n)
    width = 80
    result = ""
    lines = []
    str = self
    
    loop {
      break if str.nil?
      head = str[0..(width-n)]
      tail = str[(width-n+1)..-1]
      lines << head
      str = tail
    }
    
    lines.map{|line|
      " " * n + line
    }.join("\n")
  end
end


def file_at_app_dir(filename)
  File.join(File.dirname(__FILE__), filename)
end


class ScriptFuShell
  attr_accessor :debug, :error_code

  def initialize(options={})
    options[:verbose] ||= false
    options[:server]  ||= "127.0.0.1"
    options[:port]    ||= 10008

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

    script_fu_init
    completion_init
  end


  def script_fu_init
    send_raw( File.read( file_at_app_dir("script-fu-shell-init.scm") ) )
  end


  def completion_init
    cache_path = file_at_app_dir("script-fu-functions-cache.txt")
    if File.exist? cache_path
      $words = File.read(cache_path).strip.split("\n")
    else
      functions_str = send( '(sfs:list-all-functions:lines)' )
      $words = functions_str.strip.strip_quote.split("\n")
      open(cache_path, "w"){|f| f.puts $words }
    end
    
    $words += File.readlines(file_at_app_dir("builtins.txt")
                              ).map{|line| line.strip }

    Readline.completion_proc = proc {|word|
      $words.grep(/\A#{Regexp.quote word}/)
    }
  end

  
  def byte2i(x)
    x.unpack("C").first.to_i
  end


  def send_raw(script)
    header = ["G", script.length].pack("an")
    pp [ ["G", script.length].pack("an") ] if @debug
    @soc.write header
    @soc.write script

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
  
  
  def one(exp)
    send(exp)
  end


  def dup_or_blank?(line)
    if Readline::HISTORY.size >= 2 &&
        Readline::HISTORY[-2] == line
      true
    elsif /^\s*$/ =~ line
      true
    else
      false
    end
  end
  
  
  def run
    while line = Readline.readline( @prompt, true)
      Readline::HISTORY.pop if dup_or_blank?(line)

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

  
  def format_args_values_desc(func_name, arg_or_value)
    names = one( %Q!(function-#{arg_or_value}-name "#{func_name}")! ).strip_quote.split("\n")
    types = one( %Q!(function-#{arg_or_value}-type "#{func_name}")! ).strip_quote.split("\n").map{|x| x.sub(/^PDB-/, "")}
    descs = one( %Q!(function-#{arg_or_value}-desc "#{func_name}")! ).strip_quote.split("\n")

    names.zip(types, descs).map{|x|
      "  %s (%s)\n%s" % [x[0], x[1], x[2].indent_and_truncate(4) ]
    }.join("\n")
  end
  

  def action(options, argv)
    case options[:action]
    when :eval
      puts one(argv.join(" "))
    when :functions_sexp
      puts one('(sfs:list-all-functions)')
    when :function_args_name
      puts one( %Q!(function-args-name "#{argv[0]}")! ).strip_quote
    when :function_desc_full
      func = argv[0]
      puts [ one(%Q!(function-blurb "#{func}")!).strip_quote,
             "Arguments:\n" + format_args_values_desc(func, "args"),
             "Return values:\n" + format_args_values_desc(func, "values"),
             one(%Q!(function-help "#{func}")!).strip_quote,
           ].join("\n\n")
    else
      raise "unknown action"
    end
  end
end


if $0 == __FILE__
  options = {}

  OptionParser.new {|opt|
    opt.on("-s", "--server=ADDRESS") {|v| options[:port] = v }
    opt.on("-p", "--port=PORT")      {|v| options[:port] = v.to_i }
    opt.on("-v", "--verbose")        {|v| options[:verbose] = true }

    opt.on("-e", "--eval")        {|v| options[:action] = :eval }
    opt.on("--functions-sexp")    {|v| options[:action] = :functions_sexp }
    opt.on("--function-args-name"){|v| options[:action] = :function_args_name }
    opt.on("--function-desc-full"){|v| options[:action] = :function_desc_full }

    opt.parse!(ARGV)
  }

  sh = ScriptFuShell.new(options)
  
  if options[:action]
    sh.action(options, ARGV)
  else
    sh.run
  end
end
