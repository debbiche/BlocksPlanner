require 'sinatra'
require File.dirname(__FILE__) + "/ruby-planner/parser"
require File.dirname(__FILE__) + "/ruby-planner/world"

get '/parser.cgi' do
  content_type("text/plain")
  input = params[:input]
  output = %x(./parser.cgi "#{input}")
end

get '/planner.cgi' do 
  content_type("text/plain")
  holdin, trees, world = params[:holding], params[:trees], params[:world]
  initial_world = World.new(world)
  # Worlds are mutable (sorry...) :P
  final_world   = World.new(world)
  tree_wrapper  = Parser.parse(trees.split("\n").first)
  tree_wrapper.perform(final_world)
  puts initial_world
  puts final_world
  final_world.to_s
end