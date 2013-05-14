require File.dirname(__FILE__) + "/../world.rb"
require File.dirname(__FILE__) + "/../parser.rb"

describe Parser do
  let(:take) { "( take ( any ( thatis ( block _ tall _ ) ( inside ( the ( block box _ red ) ) ) ) ) )"}
  let(:move) { "(move (the (thatis (block _ _ blue) (leftof (any (block pyramid _ _))))) (inside (any (block box medium _))))"}
  let(:take_wrapper) { Parser.parse take }  
  let(:move_wrapper) { Parser.parse move }
  let(:take_tree) { take_wrapper.tree }
  let(:move_tree) { move_wrapper.tree }
  describe "fundamentals" do
    it "should have a command in the root node" do
      take_tree.command.should_not be_nil
    end
    describe "take" do
      it "has a qualified argument" do
        take_tree.command.qualifier.should_not be_nil
        take_tree.command.qualifier.should be_a Command::Qualifier
      end
    end

    describe "qualifier" do
      it "has a name" do
        take_tree.command.qualifier.should respond_to :name
      end
    end
  end
end