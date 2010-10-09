#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require "script-fu-shell"
require "test/unit"

class ScriptFuShellTest < Test::Unit::TestCase
  def setup
    @sh = ScriptFuShell.new
    @sh.debug = !true
  end

  def test_send
    assert_equal( "", @sh.send(""))

    assert_equal( "'()", @sh.send("'()"))
    assert_equal( "'()", @sh.send("()"))

    assert_equal( "#f", @sh.send("#f"))
    assert_equal( "#t", @sh.send("#t"))

    assert_equal( "'foo-symbol", @sh.send("'foo-symbol"))

    assert_equal( "(1 . 2)", @sh.send("(cons 1 2)"))
    assert_equal( "(1 . 2)", @sh.send("'(1 . 2)"))

    assert_equal( "(1 2 3)", @sh.send("(list 1 2 3)"))
    assert_equal( "(1 2 3)", @sh.send("'(1 2 3)"))

    assert_equal( "#(1 2)", @sh.send("#(1 2)"))

    assert_equal( '#\a', @sh.send('#\a'))

    assert_equal( '"123"', @sh.send('"123"'))

    assert_equal( '#<CLOSURE>', @sh.send('print'))
    assert_equal( '#<PROCEDURE>', @sh.send('cons'))

    # escape char
    assert_equal( '"a"b"', @sh.send('"a\"b"'))

    # paren in string
    assert_equal( '"a(b"', @sh.send('"a(b"'))
    assert_equal( '"a)b"', @sh.send('"a)b"'))
    
    assert_equal( "(1)", @sh.send("'(1)"))

    assert_equal( "3", @sh.send("(+ 1 \n 2)"))
    assert_equal( "3", @sh.send("(+ 1 \n ;; comment \n 2)"))

    @sh.send("()")
    assert_equal( 0, @sh.byte2i(@sh.error_code))
    @sh.send("(1)")
    assert_not_equal( 0, @sh.byte2i(@sh.error_code))
  end


  def test_high_byte
    assert_equal('"' + "a"*1 + '"', @sh.send('(make-string 1 #\a)'))
    assert_equal('"' + "a"*254 + '"', @sh.send('(make-string 254 #\a)'))
  end


  def test_parse_line
    assert_equal(""        , @sh.prepare_line(""))
    assert_equal("a"       , @sh.prepare_line("a"))
    assert_equal("a"       , @sh.prepare_line("a;b"))
    assert_equal("a"       , @sh.prepare_line("a;("))
    assert_equal("a"       , @sh.prepare_line( %Q<a;"> ))
    assert_equal('a ";" b' , @sh.prepare_line('a ";" b'))
  end
end
