package planner;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;

import javax.annotation.Generated;

public class Planner {
	private static HashMap<String,Block> _blocks;
	
	public static String planner(World w, String factsInput ) {
		String plan;
		ArrayList<ArrayList<String>> finalFacts = Parser.parseFinalFacts(factsInput);
		HashMap<String, Integer> derivedFacts = extractInitialFacts(w,finalFacts);
		int i = 0;
//		HashMap<Integer,ActionLayer> actionLayers = new HashMap<Integer, ActionLayer>();


		while(true){
			
			if(containsFactsWithoutMutex(derivedFacts,finalFacts)) 	{
				plan = extractSolution();	// Returns the solution
				if (validPlan(plan)) return plan;
				else return "failure";
			}		HashMap<Integer,Action> _actions = Action.generateActions(_blocks);
			System.out.println(derivedFacts.toString());
			ActionLayer al = new ActionLayer(i, derivedFacts);
			al.createActions(_actions);
			i++;
		}	
	}
	
   
    
	private static boolean containsFactsWithoutMutex(
			HashMap<String, Integer> derivedFacts,
			ArrayList<ArrayList<String>> finalFacts) {
		// TODO Auto-generated method stub
		return false;
	}
	
	private static boolean validPlan(String plan){
		// TODO Check if found valid plan 
		return true;
	}

	private static String extractSolution() {
		// TODO Backtrack the solution
		// Use a heuristics otherwise can be intractable see page 401
		return null;
	}
	
	
//	public static boolean checkFact(String s) {
//		StringTokenizer ta = new StringTokenizer(s," ");
//		String fact = ta.nextToken();
//		String a,b;
//		if(fact.equals("RightOf")) {
//			a = ta.nextToken();
//			b = ta.nextToken();
//			return currentWorld.isRightOf(a, b);
//		}
//		if(fact.equals("LeftOf")) {
//			a = ta.nextToken();
//			b = ta.nextToken();
//			return currentWorld.isLeftOf(a, b);
//		}
//		if(fact.equals("OnTop")) {
//			a = ta.nextToken();
//			return currentWorld.isOnTop(a);
//		}
//		if(fact.equals("AboveOf")) {
//			a = ta.nextToken();
//			b = ta.nextToken();
//			return currentWorld.isAbove(a, b);
//		}
//		if(fact.equals("UnderOf")) {
//			a = ta.nextToken();
//			b = ta.nextToken();
//			return currentWorld.isUnder(a, b);
//		}
//		if(fact.equals("Grabber")) {
//			a = ta.nextToken();
//			return currentWorld.grabContains(a);
//		}
//		if(fact.equals("Empty")) {
//			return currentWorld.grabEmpty();
//		}
//		return false;
//	}
//	
//	private static World pick(int column) {
//		World newWorld = new World(currentWorld);
//		newWorld.pick(column);
//		return newWorld;
//	}
//	
//	private static World drop(int column) {
//		World newWorld = new World(currentWorld);
//		newWorld.drop(column);
//		return newWorld;
//	}
//	
	public static HashMap<String, Integer> extractInitialFacts(World w, ArrayList<ArrayList<String>> finalFacts) {
		String fact ="";
		HashMap<String, Integer> initialFacts = new HashMap<String, Integer>();
		int i = 0;
		if (w.getGrab().equals("null")) {
			fact = "Empty";
			initialFacts.put(fact, i++);
		}
		else {
			fact = "Grabber " + w.getGrab();
			initialFacts.put(fact, i++);
		}
		ArrayList<String> topOfWorld = w.topOfWorld();
		Iterator it = topOfWorld.iterator();
		while (it.hasNext()) {
			String nextItem = (String) it.next();
			initialFacts.put("OnTop " + nextItem,i++);
			if (!nextItem.contains("floor")) {
				String over = nextItem;
				while (true) {
					
					String under = w.getUnder(over);
					if (under.contains("floor")) break;
					initialFacts.put( "Over " + over + " " + under,i++);
					over = under;
				}
			}
		}
		it = finalFacts.iterator();
		int j = i;
		while (it.hasNext()) {
			Iterator it2 = ((ArrayList<String>) it.next()).iterator();
			while(it2.hasNext()) {
				String finalFact = (String) it2.next();
				if (!initialFacts.containsValue(finalFact)) initialFacts.put("Â"+ finalFact,j++);
			}
		}
		if (j == i) System.out.println("The initial World already Satisfies the Final Facts");
		return initialFacts;
	}
	
	public static void main(String[] args) {
		String blocksInput = "a rectangle tall blue;b ball small white; c square large red; d pyramid large green;" +
				" e box large white; f rectangle wide black; g rectangle wide blue; h rectangle wide red;" +
				" i pyramid medium yellow; j box large red; k ball small yellow; l box medium red;" +
				" m ball medium blue";
		String initialFacts = "" +
				"Over a floor1;" +
				"Over c floor2;" +
				"Over e floor4;" +
				"Over j floor7;" +
				"Over l floor9;" +
				"Over b a;" +
				"Over d c;" +
				"Over f e;"+
				"Over g f;" +
				"Over h g;" +
				"Over i h;" +
				"Over k j;" +
//				"Grabber b;" +
				"Over m l;";
		
		String factsInput = "((RightOf m a)&(LeftOf l a)&(RightOf k c)&(AboveOf b a)&(AboveOf i l)&(Over m l))";
		
		_blocks = Parser.createBlocks(blocksInput);
		World w = Parser.parseWorld(_blocks, initialFacts);
		planner(w, factsInput);
	}
}
