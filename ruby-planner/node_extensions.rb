require './array_utils'

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
      {form: _form, color: _color, size: _size}
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

    def get_blocks(world)
      world.send(name) {fetch_expression.get_blocks(world)}
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
      world.thatis(properties: block.properties, preposition: position.preposition_name) do
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
      exp.body.get_blocks(world)
    end

    def preposition_name
      preposition.text_value
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
        args = {preposition: preposition, target: target, source: block}
        # puts "Running with_preposition #{args.inspect} "
        world.with_preposition(args)
      end
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

    def args_length
      1
    end
  end
end