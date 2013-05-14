module Command
  class Block < Treetop::Runtime::SyntaxNode
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
  end

  class Root < Treetop::Runtime::SyntaxNode

  end

  class Thatis < Treetop::Runtime::SyntaxNode
  end

  class Preposition < Treetop::Runtime::SyntaxNode
  end

  class Position < Treetop::Runtime::SyntaxNode
  end

  class Action < Treetop::Runtime::SyntaxNode
  end

  class Take < Action
    def name
      "take"
    end

    def perform(world)
      world.take(arguments[0])
    end

    def qualifier
      exp.body
    end

    def args_length
      1
    end

    def arguments
      [qualified_exp]
    end
  end
  
  class Move < Action
    def name
      "move"
    end
    
    def source

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