require File.dirname(__FILE__) + "/../world.rb"
require File.dirname(__FILE__) + "/../parser.rb"

describe Parser do
  let(:take) { "( take ( any ( thatis ( block _ small _ ) ( ontop ( the ( block rectangle _ blue ) ) ) ) ) )"}
  let(:move) { "(move (the (thatis (block _ _ blue) (leftof (any (block pyramid _ _))))) (inside (any (block box medium _))))"}
  let(:take2) { "( take ( any ( block _ small _ ) ) )" }
  let(:move2) { "(move (all (block _ _ blue)) (leftof (any (block pyramid _ _))))"}
  let(:move2_tree) { Parser.parse(move2).tree }
  let(:put) { "( put ( leftof ( any ( block box _ red ) ) ) )" }
  let(:take_wrapper) { Parser.parse take }  
  let(:move_wrapper) { Parser.parse move }
  let(:put_wrapper) { Parser.parse put }
  let(:take_tree) { take_wrapper.tree }
  let(:put_tree) { put_wrapper.tree }
  let(:take2_tree) { Parser.parse(take2).tree }
  let(:move_tree) { move_wrapper.tree }
  let(:world) { World.new }
  let(:world_with_grabber) do
    world.take("b")
    world
  end
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
      let(:qualifier) { take_tree.command.qualifier }
      it "has a name" do
        qualifier.should respond_to :name
      end

      it "can be applied to a world" do
        qualifier.get_blocks(world).should eq("b")
      end
    end

    describe "changes to the world" do
      it "parses the take expression correctly" do
        take_tree.command.perform(world)
        world.grabber.should eq("b")
        world.world.should eq([[], ["a"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]])
      end

      it "parses the move expression correctly" do
        move_tree.command.perform(world)
        world.grabber.should be_nil
        world.world.should eq([[], ["b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","a", "m"]])
      end
    end
  end

  describe "facts" do
    describe "put" do
      it "returns correct output" do
        put_tree.command.to_facts(world_with_grabber).should eq("(leftof b j)|(leftof b l)")
      end
    end

    describe "take" do
      it "returns correct output" do
        take_tree.command.to_facts(world).should eq("(grabber b)")
      end

      it "returns correct output for another tree" do
        take2_tree.command.to_facts(world).should eq("(grabber b)|(grabber k)")
      end
    end

    describe "move" do
      it "returns correct output" do
        move_tree.command.to_facts(world).should eq("(inside a l)")
      end

      it "returns correct output for another tree" do
        move2_tree.command.to_facts(world).should eq("(leftof a d)|(leftof a i)&(leftof g d)|(leftof g i)&(leftof m d)|(leftof m i)")
      end
    end
  end 
end

