require 'treetop'
require 'debugger'

# Find out what our base path is
BASE_PATH = File.expand_path(File.dirname(__FILE__))

require File.join(BASE_PATH, 'node_extensions.rb')

class Parser
  attr_reader :tree
  Treetop.load(File.join(BASE_PATH, 'command.treetop'))
  @@parser = CommandParser.new

  def self.parse(data)
    @tree = @@parser.parse(data)
    
    if @tree.nil?
      debugger
      raise Exception, "Parse error at offset: #{@@parser.index}"
    end

    return @tree
  end

  def get_action
    tree
  end
end