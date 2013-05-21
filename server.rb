require 'sinatra'
require File.dirname(__FILE__) + "/ruby-mapper/parser"
require File.dirname(__FILE__) + "/ruby-mapper/world"

#
# BLOCKSPLANNER WEB SERVER
# 
# This is the Web Server component of the BlockPlanner application
# It handles parser and planner requests from the web client,
# relaying these to appropriate components as needed.
#
# @author Jesper Josefsson
#

# These routes 'simulate' the origin CGI scripts, in order minimize changes to the client code
get '/parser.cgi' do
  content_type("text/plain")
  input = params[:input]
  # Run the Python parser script
  output = %x(./parser.cgi "#{input}")
end

get '/planner.cgi' do 
  content_type("text/plain")
  holding, trees, world = params[:holding], params[:trees], params[:world]
  initial_world = World.new(world, holding)
  tree_wrapper  = Parser.parse(trees.split("\n").first) # We only use the first syntax tree, if there are many
  facts = tree_wrapper.tree.command.to_facts(initial_world)
  command = %Q(cd haskell-planner && runhaskell PlannerFacts "#{initial_world.encode_world}" "#{facts}" "#{initial_world.encode_blocks}")
  puts command
  plan = `#{command}`
  plan.gsub(/\"/, "").split(";").join("\n")
end