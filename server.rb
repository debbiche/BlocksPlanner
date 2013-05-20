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
  holding, trees, world = params[:holding], params[:trees], params[:world]
  initial_world = World.new(world, holding)
  # # Worlds are mutable (sorry...) :P
  # final_world   = World.new(world, holding)
  tree_wrapper  = Parser.parse(trees.split("\n").first)
  puts tree_wrapper.tree.command.to_facts(initial_world)
  facts = tree_wrapper.tree.command.to_facts(initial_world)
  command = %Q(runhaskell PlannerFacts "#{initial_world.encode_world}" "#{facts}" "#{initial_world.encode_blocks}")
  plan = `#{command}`
  plan.gsub(/\"/, "").split(";").join("\n")
end