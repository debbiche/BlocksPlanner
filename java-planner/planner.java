import java.util.ArrayList;


public class planner {
	ArrayList<Movement> = new ArrayList
	public enum Movement {
	    UP,DOWN,RIGHT,LEFT,PICK,DROP 
	}

	public void writeAction (Movement movement){
		switch (day) {
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
	
	public writePlan
		    
	public static void main (String args[]) {
        // Read the data
        String holding = args[0];
        String world = args[1];
        String[] trees = args[2].split(";");

        // Print the data
		System.out.println("# Stupid Java planner!");
		System.out.println("# Holding: " + holding);
		System.out.println("# World: " + world);
        for (String t : trees) {
            System.out.println("# Tree: " + t);
        }

        // Print the plan
		System.out.println("This is a stupid move!");
        int stacknr = 0;
        for (int i = 0; i < world.length(); i++) {
            if (world.charAt(i) == ';') 
                stacknr++;
            else if (world.charAt(i) != ' ') 
                break;
        }
		System.out.println("pick " + stacknr);
		System.out.println("drop " + stacknr);
	}
}
