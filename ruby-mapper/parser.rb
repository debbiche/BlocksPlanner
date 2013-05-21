require 'treetop'

require File.dirname(__FILE__) + '/tree_wrapper'

# Find out what our base path is
BASE_PATH = File.expand_path(File.dirname(__FILE__))

require File.join(BASE_PATH, 'node_extensions.rb')

class Parser
  attr_reader :tree
  Treetop.load(File.join(BASE_PATH, 'command.treetop'))
  @@parser = CommandParser.new

  def self.parse(data)
    wrapper = TreeWrapper.new(@@parser.parse(data))
    
    if wrapper.tree.nil? 
      puts "Encountered error: #{@@parser.failure_reason}"
      # raise Exception, "Parse error at offset: #{@@parser.index}"
    end
    return wrapper
  end
end