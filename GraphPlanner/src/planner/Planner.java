package planner;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;

public class Planner {
	private static HashMap<String, Block> _blocks;
	private static World initialWorld;
	private static World currentWorld;
	private static String currentPlan;
	private static ArrayList<ArrayList<String>> finalFacts;
	private static ArrayList<String> _mutex = new ArrayList<String>();
	private static ArrayList<World> visitedWorlds = new ArrayList<World>();
	
	public static String planner() {
		boolean breakFlag = false;
		String plan = "";
		ArrayList<HashMap<String,World>> worldsToProccess = new ArrayList<HashMap<String,World>>();
		HashMap<String, World> iWorld = new HashMap<String, World>();
		iWorld.put(plan, initialWorld);
		worldsToProccess.add(iWorld);

		while(!breakFlag) {
			HashMap<String,World> sonsBranch = worldsToProccess.get(0);
			Iterator it = sonsBranch.keySet().iterator();
			while (it.hasNext()) {
				currentPlan = (String) it.next();
//				System.out.println(currentPlan);
				currentWorld = sonsBranch.get(currentPlan);
//				if (!isVisited(currentWorld)) {
					visitedWorlds.add(currentWorld);
					if (checkFacts()) {
						breakFlag = true;
					}
					if(breakFlag) break;
					worldsToProccess.remove(0);
					worldsToProccess.add(generateSons(currentPlan));
//				}
			}
		}
		return currentPlan;
	}
	public static HashMap<String,World> generateSons(String plan) {
		HashMap<String,World> _returnValue = new HashMap <String,World>();
		_mutex = createFactsMutex();
		ArrayList<String> _actions = createActions();
		for (int i = 0; i < _actions.size(); i++) {
			StringTokenizer st = new StringTokenizer(_actions.get(i)," ");
			String action = st.nextToken();
			StringTokenizer at = new StringTokenizer(plan,"droppick, ");
			int lastColumn = -1;
			while(at.hasMoreTokens()) lastColumn = Integer.parseInt(at.nextToken());
			int column = Integer.parseInt(st.nextToken());
			if (column != lastColumn) {
				if (action.equals("pick")) {
					World w =  pick(column);
//					if (!isVisited(w))
						_returnValue.put(plan +_actions.get(i)+",",w);
				}
				if (action.equals("drop")) {
					World w = drop(column);
//					if (!isVisited(w))
						_returnValue.put(plan +_actions.get(i)+"," , w);
				}
			}
		}
		return _returnValue;
	}
	
	public static boolean isVisited(World w) {
		Iterator it = visitedWorlds.iterator();
		while(it.hasNext()) {
			World visited = (World) it.next();
			if (visited.compareTo(w)) return true;
		}
		return false;
	}
	
	public static boolean checkFacts () {
		boolean orFlag = false;
		for (int i = 0; i < finalFacts.size(); i++) {
			boolean andFlag = true;
			for (int j = 0; j < finalFacts.get(i).size(); j++) {
				andFlag = andFlag && checkFact(finalFacts.get(i).get(j));
				if (!andFlag) break;
			}
			orFlag= orFlag || andFlag;
		}
		return orFlag;
	}
	
	public static boolean checkFact(String s) {
		StringTokenizer ta = new StringTokenizer(s," ");
		String fact = ta.nextToken();
		String a,b;
		if(fact.equals("RightOf")) {
			a = ta.nextToken();
			b = ta.nextToken();
			return currentWorld.isRightOf(a, b);
		}
		if(fact.equals("LeftOf")) {
			a = ta.nextToken();
			b = ta.nextToken();
			return currentWorld.isLeftOf(a, b);
		}
		if(fact.equals("OnTop")) {
			a = ta.nextToken();
			return currentWorld.isOnTop(a);
		}
		if(fact.equals("AboveOf")) {
			a = ta.nextToken();
			b = ta.nextToken();
			return currentWorld.isAbove(a, b);
		}
		if(fact.equals("UnderOf")) {
			a = ta.nextToken();
			b = ta.nextToken();
			return currentWorld.isUnder(a, b);
		}
		if(fact.equals("Grabber")) {
			a = ta.nextToken();
			return currentWorld.grabContains(a);
		}
		if(fact.equals("Empty")) {
			return currentWorld.grabEmpty();
		}
		return false;
	}
	
	public static ArrayList<String> createFactsMutex() {
		ArrayList<String> _returnValue = new ArrayList<String>();
		Iterator it = currentWorld.topOfWorld().iterator();
		while (it.hasNext()) {
			String currentPosition = (String) it.next();
			if (currentWorld.grabEmpty()) {
				if (currentPosition.contains("floor")) _returnValue.add("0");
				else _returnValue.add("1");
			}
		
			else {
				String g = currentWorld.getGrab();
				if (currentPosition.contains("floor")) _returnValue.add("1");
				else if (currentWorld.fitOver(g, currentPosition)) _returnValue.add("1");
					else _returnValue.add("0");
				}
		}
		return _returnValue;
	}
	
	public static ArrayList <String> createActions() {
		ArrayList <String> _actions = new ArrayList<String>(); 
		for (int i = 0; i < _mutex.size(); i++) {
			if (currentWorld.grabEmpty()) {
				if (!_mutex.get(i).equals("0")) _actions.add("pick " + i);
				}
			else if (!_mutex.get(i).equals("0"))  _actions.add("drop " + i);
		}
		return _actions;
	}
	
	private static World pick(int column) {
		World newWorld = new World(currentWorld);
		newWorld.pick(column);
		return newWorld;
	}
	
	private static World drop(int column) {
		World newWorld = new World(currentWorld);
		newWorld.drop(column);
		return newWorld;
	}
//	private static boolean checkFactsValidity{
//		
//	}
	public static void main(String[] args) {
		String blocksInput = "a rectangle tall blue;b ball small white; c square large red; d pyramid large green;" +
				" e box large white; f rectangle wide black; g rectangle wide blue; h rectangle wide red;" +
				" i pyramid medium yellow; j box large red; k ball small yellow; l box medium red;" +
				" m ball medium blue";
		String initialFacts = "" +
				"OnTop a floor1;" +
				"OnTop c floor2;" +
				"OnTop e floor4;" +
				"OnTop j floor7;" +
				"OnTop l floor9;" +
				"OnTop b a;" +
				"OnTop d c;" +
				"OnTop f e;"+
				"OnTop g f;" +
				"OnTop h g;" +
				"OnTop i h;" +
				"OnTop k j;" +
//				"Grabber b;" +
				"OnTop m l;";
		
		String factsInput = "((RightOf m a)&(LeftOf l a)&(RightOf k c)&(AboveOf b a)&(AboveOf i l)";
		
		_blocks = Parser.createBlocks(blocksInput);
		initialWorld = Parser.parseWorld(_blocks, initialFacts);
		finalFacts = Parser.parseFinalFacts(factsInput);
		currentWorld = new World(initialWorld);
		double timeInit = System.currentTimeMillis();
		System.out.println(planner());
		double timeEnd = System.currentTimeMillis();
		double time = timeEnd - timeInit;
		System.out.println(time);
		System.out.println(currentWorld.toString());
	}
}
