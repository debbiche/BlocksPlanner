require File.dirname(__FILE__) + "/../world.rb"

describe World do
  let(:world) { World.new }

  it "should have a world and blocks" do
    world.should respond_to(:world)
    world.should respond_to(:blocks)
  end

  describe "color" do
    it "should be able to find blocks of a certain color" do
      world.with_properties(color: "red").should eq(["c", "h", "j", "l"])
      world.with_properties(color: "blue").should eq(["a", "g", "m"])
      world.with_properties(color: "white").should eq(["b", "e"])
      world.with_properties(color: "yellow").should eq(["i", "k"])
      world.with_properties(color: "green").should eq(["d"])
    end

    it "should be able to find all blocks of arbitrary color" do
      world.with_properties(color: "_").should eq(("a".."m").to_a)
    end
  end

  it "should be able to combine queries" do
    world.with_properties(color: "green", form: "pyramid").should eq(["d"])
    world.with_properties(color: "red", form: "box", size: "large").should eq(["j"])
  end

  describe "position" do
    it "should give the right position" do
      world.position_of("a").should eq([1,0])
      world.position_of("b").should eq([1,1])
    end

    it "should give the right coordinate for a leftof" do
      world.leftof_position("a").should eq([0,0])
    end

    it "should give the right coordinate for a rightof" do 
      world.rightof_position("a").should eq([2,2])
    end
  end

  describe "fetching" do
    it "should be possible to do an any fetch" do
      world.any { world.with_properties(color: "green")}.should eq("d")
    end

    it "should be possbile to do a the fetch" do
    end

    it "should be possible to do a thatis fetch" do
      world.thatis(
        properties: {color: "blue"}, 
        preposition: "leftof") { world.the { world.with_properties(color: "green") } }.should eq(["a"])
    end

    it "should be possible to get all blocks leftof" do
      world.is_leftof("c").should eq(["a", "b"])
    end

    it "should be possible to get all blocks rightof" do
      world.is_rightof("k").should eq(["l", "m"])
    end

    it "should be possible to get a block ontop" do
      world.is_ontop("e").should eq("f")
    end

    it "should be possible to get blocks above" do
      world.is_above("g").should eq(["h", "i"])
    end

    it "should be possible to get blocks under" do
      world.is_under("g").should eq(["e", "f"])
    end
  end

  describe "moving" do
    it "does ontop correctly" do
      world.ontop("a", "b")
      world.get_column(1).should eq(["b", "a"])
    end

    it "does above correctly" do
      world.above("c", "a")
      world.get_column(1).should eq(["a", "b", "c"])
      world.get_column(2).should eq(["d"])
    end

    it "does leftof correctly" do
      world.leftof("a", "b")
      world.get_column(0).should eq(["a"])
      world.get_column(1).should eq(["b"])
    end

    it "does rightof correctly" do
      world.rightof("a", "b")
      world.get_column(1).should eq(["b"])
      world.get_column(2).should eq(["c", "d", "a"])
    end

    it "does beside correctly" do
      world.beside("a", "m")
      world.get_column(1).should eq(["b"])
      world.get_column(8).should eq(["a"])
    end

    it "does under correctly" do
      world.under("b", "a")
      world.get_column(1).should eq(["b", "a"])
    end
  end

  describe "inserting" do
    it "should insert blocks correctly" do
      world.insert_block_at_position("x", [1,0])
      world.position_of("x").should eq([1,0])
      world.get_column(1).should eq ["x", "a", "b"]
    end
  end
end