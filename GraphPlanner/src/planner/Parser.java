package planner;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

public class Parser {
	
	public static ArrayList<ArrayList<String>> parseFinalFacts(String facts) {
		ArrayList<ArrayList<String>> returnValue = new ArrayList<ArrayList<String>>();
		StringTokenizer orFacts = new StringTokenizer(facts,"|");
		while(orFacts.hasMoreTokens()) {
			StringTokenizer andFacts = new StringTokenizer(orFacts.nextToken(),"()&");
			ArrayList<String> aux = new ArrayList<String>();
			while(andFacts.hasMoreTokens()) {
				aux.add(andFacts.nextToken());
			}
			returnValue.add(aux);
		}
		return returnValue;
	}
	
	public static World parseWorld(HashMap<String, Block> _blocks, String input){
		World world = new World(_blocks);
		StringTokenizer facts = new StringTokenizer(input,";");
		int column;
		while (facts.hasMoreTokens()) {
			StringTokenizer elements = new StringTokenizer(facts.nextToken().toString()," ");
			String fact = elements.nextToken().toString();
			String a = elements.nextToken().toString();
			String b = null;
			
			if (elements.hasMoreTokens()) {
				b = elements.nextToken().toString();
			}
			
			if (fact.equals("Over")) 
				if (b.contains("floor")) {
					column = Integer.parseInt(b.split("floor")[1]);
					world.getWorld().get(column).add(a);
				}
				else 
					world.getWorld().get(world.getColumn(b)).add(a);
			if (fact.equals("Grabber")) world.setGrab(a);
		}
		return world;
	}
	
	public static HashMap<String,Block> createBlocks(String input) {
		HashMap<String,Block> _blocks = new HashMap<String, Block>();
		
		StringTokenizer inputs  = new StringTokenizer(input,";");
		while (inputs.hasMoreTokens()) {
			StringTokenizer properties = new StringTokenizer(inputs.nextToken().toString());
			String id = properties.nextToken();
			String shape = properties.nextToken();
			String color = properties.nextToken();
			String size = properties.nextToken();
			Block block = new Block(id,shape, color, size);		
			_blocks.put(id, block);
		}
		return _blocks;
	}
}