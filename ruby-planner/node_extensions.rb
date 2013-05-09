module Command
  class Block < Treetop::Runtime::SyntaxNode
    def to_a
      return self.elements
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
  end

  class Root < Treetop::Runtime::SyntaxNode
    def find(nodeType)
    end
    def action

    end
  end

  class Thatis < Treetop::Runtime::SyntaxNode
  end

  class Preposition < Treetop::Runtime::SyntaxNode
  end

  class Take < Treetop::Runtime::SyntaxNode
  end
  
  class Move < Treetop::Runtime::SyntaxNode
  end

  class Put < Treetop::Runtime::SyntaxNode
  end
end