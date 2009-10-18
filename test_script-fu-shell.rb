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
    assert_equal( "<no input>", @sh.send(""))

    assert_equal( "#f", @sh.send("#f"))
    assert_equal( "#t", @sh.send("#t"))

    # unexpected behaviour!
    # assert_equal( "(1 . 2)", @sh.send("(cons 1 2)"))
    # assert_equal( "(1 . 2)", @sh.send("'(1 . 2)"))

    assert_equal( "(1 2 3)", @sh.send("(list 1 2 3)"))
    assert_equal( "(1 2 3)", @sh.send("'(1 2 3)"))

    assert_equal( '"123"', @sh.send('"123"'))

    # escape char
    assert_equal( '"a"b"', @sh.send('"a\"b"'))

    # paren in string
    assert_equal( '"a(b"', @sh.send('"a(b"'))
    assert_equal( '"a)b"', @sh.send('"a)b"'))
    
    assert_equal( "(1)", @sh.send("'(1)"))
    assert_equal( "()", @sh.send("'()"))
    assert_equal( "()", @sh.send("()"))

    assert_equal( "3", @sh.send("(+ 1 \n 2)"))
    assert_equal( "3", @sh.send("(+ 1 \n ;; comment \n 2)"))

    @sh.send("()")
    assert_equal( 0, @sh.byte2i(@sh.error_code))
    @sh.send("(1)")
    assert_not_equal( 0, @sh.byte2i(@sh.error_code))
  end
end
