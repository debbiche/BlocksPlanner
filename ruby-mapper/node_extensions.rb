require File.dirname(__FILE__) + '/array_utils'

module Command
  include ArrayUtils
  class Block < Treetop::Runtime::SyntaxNode
    def get_blocks(world)
      world.with_properties(properties)
    end

    def properties
      _form   = form.text_value
      _color  = color.text_value
      _size   = size.text_value
      {:form => _form, :color => _color, :size => _size}
    end
  end

  class Form < Treetop::Runtime::SyntaxNode
  end

  class Size < Treetop::Runtime::SyntaxNode
  end

  class Color < Treetop::Runtime::SyntaxNode
  end

  class Expression < Treetop::Runtime::SyntaxNode
  end

  class Body < Treetop::Runtime::SyntaxNode
  end

  class StringLiteral < Treetop::Runtime::SyntaxNode
  end

  class WildCard < Treetop::Runtime::SyntaxNode
  end

  class AnyForm < Treetop::Runtime::SyntaxNode
  end

  class AnyColor < Treetop::Runtime::SyntaxNode
  end

  class AnySize < Treetop::Runtime::SyntaxNode
  end

  class Qualifier < Treetop::Runtime::SyntaxNode
    def name
      elements.first.text_value
    end

    ["any", "the", "all"].each do |qual|
      define_method("#{qual}?") do
        name == qual
      end
    end

    def get_blocks(world)
      world.send(name) {fetch_expression.get_blocks(world)}
    end

    def get_all_blocks(world)
      fetch_expression.get_blocks(world)
    end

    def fetch_expression
      qualified_exp.body
    end
  end

  class Root < Treetop::Runtime::SyntaxNode

  end

  class Thatis < Treetop::Runtime::SyntaxNode
    def block
      block_exp.body
    end

    def get_blocks(world)
      world.thatis(:properties => block.properties, :preposition => position.preposition_name) do
        position.get_blocks(world)
      end
    end

    def position
      position_exp.body
    end
  end

  class Preposition < Treetop::Runtime::SyntaxNode
  end

  class Position < Treetop::Runtime::SyntaxNode
    def get_blocks(world)
      expression.get_blocks(world)
    end

    def get_all_blocks(world)
      expression.get_all_blocks(world)
    end

    def preposition_name
      preposition.text_value
    end

    def expression
      exp.body
    end

    def get_coordinates(world)
      Array.wrap(expression.get_blocks(world)).map do |block|
        world.position_of(block)
      end
    end
  end

  class Action < Treetop::Runtime::SyntaxNode
  end

  class Take < Action
    def name
      "take"
    end

    def perform(world)
      world.take(qualifier.get_blocks(world))
    end

    def to_facts(world)
      orBucket = OrBucket.new
      blocks = Array.wrap(qualifier.get_all_blocks(world))
      blocks.each do |block|
        orBucket.facts << "(grabber #{block})"
      end
      return orBucket.to_s
    end

    def args_length
      1
    end

    def qualifier
      exp.body
    end
  end

  class Move < Action
    def name
      "move"
    end

    def perform(world)
      Array.wrap(from.get_blocks(world)).each do |block|
        preposition = to_preposition
        target = to.get_blocks(world)
        args = {:preposition => preposition, :target => target, :source => block}
        world.with_preposition(args)
      end
    end

    def to_facts(world)
      outer_bucket = from.any? ? OrBucket.new : AndBucket.new
      Array.wrap(from.get_blocks(world)).each do |block|
        inner_bucket = OrBucket.new
        preposition = to_preposition
        targets = Array.wrap(to.get_all_blocks(world))
        targets.each do |target_block|
          inner_bucket.facts << "(#{preposition} #{block} #{target_block})"
        end
        outer_bucket.facts << inner_bucket.to_s
      end
      outer_bucket.to_s
    end

    def from
      from_exp.body
    end

    def to
      to_exp.body
    end

    def to_preposition
      to.preposition.text_value
    end

    def args_length
      2
    end
  end

  class Put < Action
    def name
      "put"
    end

    def perform(world)
      coordinate = position.get_coordinates(world)
      if coordinate.respond_to? :each
        coordinate = coordinate.first
      end
      world.put(coordinate)
    end

    def to_facts(world)
      orBucket = OrBucket.new
      blocks = Array.wrap(position.get_all_blocks(world))
      blocks.each do |block|
        orBucket.facts << "(#{position.preposition_name} #{world.grabber} #{block})"
      end
      return orBucket.to_s
    end

    def args_length
      1
    end

    def position
      exp.body
    end
  end

  class Bucket
    attr_accessor :facts
 
    def initialize
      @facts = []
    end
 
    def to_s
      output = []
      facts.each do |f|
        output << "#{f}"
      end
      output = output.join(name)
    end
  end

  class OrBucket < Bucket
    def name
      "|"
    end
  end
 
  class AndBucket < Bucket
    def name
      "&"
    end
  end
end