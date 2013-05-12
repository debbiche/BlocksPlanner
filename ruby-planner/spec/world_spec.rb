require File.dirname(__FILE__) + "/../world.rb"

describe World do
  let(:world) { World.new }
  it "should have a world and blocks" do
    world.should respond_to(:world)
    world.should respond_to(:blocks)
  end

  describe "color" do
    it "should be able to find blocks of a certain color" do
      world.where(color: "red").should eq(["c", "h", "j", "l"])
      world.where(color: "blue").should eq(["a", "g", "m"])
      world.where(color: "white").should eq(["b", "e"])
      world.where(color: "yellow").should eq(["i", "k"])
      world.where(color: "green").should eq(["d"])
    end

    it "should be able to find all blocks of arbitrary color" do
      world.where(color: "_").should eq(("a".."m").to_a)
    end
  end

  it "should be able to combine queries" do
    world.where(color: "green", form: "pyramid").should eq(["d"])
    world.where(color: "red", form: "box", size: "large").should eq(["j"])
  end
end