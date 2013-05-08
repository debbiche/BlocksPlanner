package planner;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.PriorityQueue;

import planner.Planner.Movement;

/*
 * Representation of the blockWorld
 */

public class World {
	public enum Block {
		TRIANGLE,SQUARE,CIRCLE,BOX,RECTANGLE,NONE
	}
	private int armX,armY;
	private Block load;
	private ArrayList<ArrayList<Block>> world;
	private PriorityQueue<Movement> plan;
	
	/*
	 * Constructor for the blockWorld
	 */
	public World(int armX, int armY, Block load,
			ArrayList<ArrayList<Block>> world) {
		super();
		this.armX = armX;
		this.armY = armY;
		this.load = load;
		ArrayList<ArrayList<Block>> auxWorld = new ArrayList<ArrayList<Block>>();
		auxWorld.addAll(world);
		this.world = auxWorld;	
		this.plan = new PriorityQueue<Movement>();
	}
	
	public void addMovement(Movement movement) {
		this.plan.add(movement);
	}
	public PriorityQueue<Movement> getPlan() {
		return plan;
	}

	public void setPlan(PriorityQueue<Movement> plan) {
		this.plan = plan;
	}

	public int getArmX() {
		return armX;
	}
	public void setArmX(int armX) {
		this.armX = armX;
	}
	public int getArmY() {
		return armY;
	}
	public void setArmY(int armY) {
		this.armY = armY;
	}
	
	public int isLoading() {
		switch (this.load) {
			case NONE:
				return 0;
			default:
				return 1;
		}
	}
	
	public Block getLoad() { 
		return this.load;
	}
	
	public void setLoad(Block block) {
		this.load = block;
	}
	public ArrayList<ArrayList<Block>> getWorld() { //Modify to return a new one
		return world;
	}
	public void setWorld(ArrayList<ArrayList<Block>> world) {
		this.world = world;
	}
	
	
}
