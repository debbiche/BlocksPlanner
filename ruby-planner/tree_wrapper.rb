class TreeWrapper
  attr_reader :tree

  def initialize(tree = nil)
    @tree = tree
  end

  def to_s
    @tree.to_s
  end
end