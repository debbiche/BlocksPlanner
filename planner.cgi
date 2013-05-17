#!/usr/bin/env ruby
require 'cgi'
require 'rubygems'
require 'bundler/setup'
require 'json'
BASE_PATH = File.dirname(__FILE__)
require [BASE_PATH, '/ruby-planner/parser'].join
require [BASE_PATH, '/ruby-planner/world'].join

HEADER = "Content-type:text/plain\r\n\r\n"

cgi = CGI.new

def parse_world(world)
  world.split(";").map{|s| s.split(",")}
end

def main(cgi)
  tree, grabber, world = cgi['tree'], cgi['grabber'], parse_world(cgi['world'])
  initial_world = World.new(world)
  # Worlds are mutable (sorry...) :P
  final_world   = World.new(world)
  tree_wrapper  = Parser.parse(tree)
  tree_wrapper.perform(final_world)
  cgi.header("text/plain")
  cgi.print("# YES")
  cgi.print("")
end

main(cgi)