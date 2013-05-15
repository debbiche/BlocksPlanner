#!/usr/bin/env ruby
require 'cgi'
puts File.dirname(__FILE__)
require File.dirname(__FILE__) + '/ruby-planner/parser'

HEADER = "Content-type:text/plain\r\n\r\n"

cgi = CGI.new

def main
  tree, grabber, world = cgi['tree'], cgi['grabber'], JSON.parse(cgi['world'])
  initial_world = World.new(world)
  # Worlds are mutable (sorry...) :P
  final_world   = World.new(world)
  tree_wrapper  = Parser.parse(tree)
  tree_wrapper.perform(final_world)
  cgi.header("text/plain")
  cgi.print "#YES"
end