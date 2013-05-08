package planner;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.PriorityQueue;

import planner.World.Block;



public class Planner {
	PriorityQueue<Movement> plan;
	private static int HEIGHT = 3;
	private static int WIDHT = 2;; //max WIDHT-1 in order to calculate Array boundaries.

	public enum Movement {
	    UP,DOWN,RIGHT,LEFT,PICK,DROP 
	}

	/*
	 * Defining the writeAction for printout
	 */
	private void writeAction (Movement movement){
		switch (movement) {
    		case UP:
    			System.out.println("UP");
    		break;
    		case DOWN:
    			System.out.println("DOWN");
    		break;
    		case RIGHT:
    			System.out.println("RIGHT");
    		break;
    		case LEFT:
    			System.out.println("LEFT");
    		break;
    		case PICK:
    			System.out.println("PICK");
    		break;
    		case DROP:
    			System.out.println("DROP");
    		break;
		}
	}	
	
	/*
	 * Iterates over and writes the plan 
	 */
	public void writePlan(ArrayList<Movement> plan){
		Iterator it =  plan.iterator();
		while (it.hasNext()) {
			writeAction((Movement) it.next());
		}
	} 
	
	/*
	 * The main driver of the planner, it tries to execute the action, given a movement and a world
	 * returns the new world 
	 */
	private World executeAction (Movement movement, World w) throws Ilegal {
		switch (movement) {
			case UP:
				if (w.getArmY() < HEIGHT) 
					return new World(w.getArmX(), w.getArmY()+1, w.getLoad(), w.getWorld());
				else throw new Ilegal();
				
			case DOWN:
                if (w.getArmY() > (w.getWorld().get(w.getArmX()).size() + w.isLoading())) 
                	return new World(w.getArmX(), w.getArmY()+1, w.getLoad(), w.getWorld());
                else throw new Ilegal();
                
			case RIGHT:
				if ((w.getArmY() < WIDHT) && ((w.getWorld().get(w.getArmX()+1).size() + w.isLoading())<=w.getArmY()))
					return new World(w.getArmX()+1, w.getArmY(), w.getLoad(), w.getWorld());
				else throw new Ilegal();
			case LEFT:
				if ((w.getArmX() >0) && ((w.getWorld().get(w.getArmX()-1).size() + w.isLoading()) <= w.getArmY()))
					return new World(w.getArmX()-1, w.getArmY(), w.getLoad(), w.getWorld());
				else throw new Ilegal();
			case DROP:
				if ((w.isLoading()==1) && ((w.getWorld().get(w.getArmX()).size() + 1) == w.getArmY())) {
					World world = new World(w.getArmX(), w.getArmY(), w.getLoad(), w.getWorld());
					world.getWorld().get(world.getArmX()).add(w.getLoad());
					Block emptyBlock = Block.NONE;
					world.setLoad(emptyBlock);
					return world;
				}
				else throw new Ilegal();
			case PICK:
				if ((w.isLoading() == 0) && ((w.getWorld().get(w.getArmX()).size() + 1) == w.getArmY())) {
					World world = new World(w.getArmX(), w.getArmY(), w.getLoad(), w.getWorld());
					Block block = world.getWorld().get(world.getArmX()).remove(world.getWorld().size()-1);
					world.setLoad(block);
					return world;
				}
				else throw new Ilegal();
		}
		throw new Ilegal();
	}

	/*
	 * Constructs the plan given a world by adding the possible different movements
	 * returns the new world
	 */
	private World buildPlan(World currentWorld) throws Ilegal {
		ArrayList<Movement> movements = new ArrayList<Movement>();
		movements.add(Movement.UP);
		movements.add(Movement.DOWN);
		movements.add(Movement.RIGHT);
		movements.add(Movement.LEFT);
		movements.add(Movement.PICK);
		movements.add(Movement.DROP);
		Iterator it = movements.iterator();
		while (it.hasNext()) {
			Movement currentMovement = (Movement) it.next(); 
			try {
				World nextWorld = executeAction(currentMovement, currentWorld);
				nextWorld.addMovement(currentMovement);
				return nextWorld;
			} catch (Exception e) {
				//DO NOTHING
			}
		}
		throw new Ilegal();
	}
	
	/*
	 * Constructing the (universe) tree of plans
	 * if we are finding the final plan it is returned
	 * otherwise we add the other movements to the existing plan and returns a priorityQueue them
	 * 
	 */
	public PriorityQueue<Movement> buildTree(World iw,World fw) throws Ilegal {
		PriorityQueue<World> firstSon = new PriorityQueue<World>();
		firstSon.add(iw);
		PriorityQueue<PriorityQueue<World>> fathers = new PriorityQueue<PriorityQueue<World>>();
		fathers.add(firstSon);
		World nextWorld = iw;
		if (iw != fw )
		while(!fathers.isEmpty()) {
			PriorityQueue<World> sons = fathers.poll();
			while (!sons.isEmpty()) {
				World currentWorld = sons.poll();
				try {
					nextWorld = buildPlan(currentWorld);
					if (nextWorld == fw) return nextWorld.getPlan();
					else sons.add(nextWorld);
				} catch (Exception e) {
					// DO NOTHING
				}
			fathers.add(sons);
			}
		}
		if (!nextWorld.getPlan().isEmpty())
			return nextWorld.getPlan();
		else throw new Ilegal();
	}
	
//	public static void main (String args[]) {
//        // Read the data
//        String holding = args[0];
//        String world = args[1];
//        String[] trees = args[2].split(";");
//
//        // Print the data
//		System.out.println("# Stupid Java planner!");
//		System.out.println("# Holding: " + holding);
//		System.out.println("# World: " + world);
//        for (String t : trees) {
//            System.out.println("# Tree: " + t);
//        }
//
//        // Print the plan
//		System.out.println("This is a stupid move!");
//        int stacknr = 0;
//        for (int i = 0; i < world.length(); i++) {
//            if (world.charAt(i) == ';') 
//                stacknr++;
//            else if (world.charAt(i) != ' ') 
//                break;
//        }
//		System.out.println("pick " + stacknr);
//		System.out.println("drop " + stacknr);
//	}
}
