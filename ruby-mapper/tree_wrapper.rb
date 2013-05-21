class TreeWrapper
  attr_reader :tree

  def initialize(tree = nil)
    @tree = tree
  end

  def perform(world)
    @tree.command.perform(world)
  end

  def to_s
    @tree.to_s
  end
end