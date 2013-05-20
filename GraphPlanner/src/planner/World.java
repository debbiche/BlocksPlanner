package planner;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class World {
	private String grab = "null";
	private ArrayList<ArrayList<String>> _world;
	private static HashMap<String, Block> _blocks;
	private static int nColumns=10;
	public World(World w) {
		super();
		this.grab = w.getGrab();
		this._world = w.cloneWorld();
		this._blocks = w.get_blocks();
	}
	
	public World(HashMap<String, Block> _blocks) {
		super();
		this.grab = "null";
		this._world = newWorld();
		this._blocks = _blocks;
	}
	
	public ArrayList<ArrayList<String>> cloneWorld() {
		ArrayList<ArrayList<String>> newWorld = new ArrayList<ArrayList<String>>();
		for (int i = 0; i < this._world.size(); i++) {
			ArrayList<String> newColumn = new ArrayList<String>();
			for (int j = 0; j < this._world.get(i).size(); j++) {
				newColumn.add(this._world.get(i).get(j));
			}
			newWorld.add(newColumn);
		}
		return newWorld;
	}
	public ArrayList<ArrayList<String>> newWorld() {
		ArrayList<ArrayList<String>> newWorld= new ArrayList<ArrayList<String>>();
		for (int i = 0; i < nColumns; i++) {
			ArrayList<String> newColumn = new ArrayList<String>();
			newColumn.add("floor"+i);
			newWorld.add(newColumn);
		}
		return newWorld;
	}
	
	public ArrayList<String> topOfWorld() {
		ArrayList<String> returnValue = new ArrayList();
		Iterator it = this.getWorld().iterator();
		String lastBlock = "null";
		while(it.hasNext()) {
			Iterator it2 = ((ArrayList<String>) it.next()).iterator();
			while(it2.hasNext()) {
				 lastBlock = (String) it2.next();
			}
			returnValue.add(lastBlock);
		}
		return returnValue;
	}
	
	public boolean compareTo(Object o) {
		boolean flag = true;
		for (int i = 0; i < this._world.size(); i++) {
			if (this._world.get(i).size() == ((World) o).getWorld().get(i).size())
				for (int j = 0; j < this._world.get(i).size(); j++) {
					if (this._world.get(i).get(j).equals(((World) o).getWorld().get(i).get(j))) flag =flag&&true;
					else flag = false;
			}
			else return false;
		}
		return flag;
	}
	
	public static HashMap<String, Block> get_blocks() {
		return _blocks;
	}

	public static void set_blocks(HashMap<String, Block> _blocks) {
		World._blocks = _blocks;
	}

	public int getColumn(String id) {
		for (int i = 0; i < this._world.size(); i++) {
			for (int j = 0; j < this._world.get(i).size(); j++) {
					if (this._world.get(i).get(j).equals(id)) return i;
			}
		}
		return -1;
	}
	
	public String getUnder(String a) {
		for (int i = 0; i < this._world.size(); i++) {
			for (int j = 0; j < this._world.get(i).size(); j++) {
				if (this._world.get(i).get(j).equals(a)) return this._world.get(i).get(j-1);
			}
		}
		return "null";
	}
	
	public boolean isRightOf(String a, String b) {
		if (a.equals(this.getGrab())) return false;
		if (b.equals(this.getGrab())) return false;
		if ((a.contains("floor")) && b.contains("floor")) {
			int columnA = Integer.parseInt(a.split("floor")[1]);
			int columnB = Integer.parseInt(b.split("floor")[1]);
			return (columnA>columnB);
		}
		else {
		if(!(a.contains("floor"))) return isRightOf(getUnder(a),b);
		if (!(b.contains("floor"))) return isRightOf(a,getUnder(b));
		}
		return false;
	}
	
	public boolean isLeftOf(String a, String b) {
		if (a.equals(this.getGrab())) return false;
		if (b.equals(this.getGrab())) return false;
		if ((a.contains("floor")) && b.contains("floor")) {
			int columnA = Integer.parseInt(a.split("floor")[1]);
			int columnB = Integer.parseInt(b.split("floor")[1]);
			return (columnA<columnB);
		}
		else {
		if(!(a.contains("floor"))) return isLeftOf(getUnder(a),b);
		if (!(b.contains("floor"))) return isLeftOf(a,getUnder(b));
		}
		return false;
	}
	
	public boolean isOnTop(String a) {
		if (a.equals(this.getGrab())) return false;
		int column = getColumn(a);
		int sizeOfColumn = this._world.get(column).size();
		return (a.equals(this._world.get(column).get(sizeOfColumn-1)));
	}
	
	public boolean isAbove(String a, String b) {
		if (a.equals(this.getGrab())) return false;
		if (b.equals(this.getGrab())) return false;
		int columnA = getColumn(a);
//		System.out.println(a + b);
		ArrayList<String> column = _world.get(columnA);
//		System.out.println(columnA);
		int columnB = getColumn(b);
//		System.out.println(columnB);
		int positionA =0;
		int positionB =0;
		for (int i = 0; i < column.size(); i++) {
			if (column.get(i).equals(a)) positionA = i;
			if (column.get(i).equals(b)) positionB = i;
		}
	return ((columnA == columnB) && (positionA>positionB));
	}
	
	public boolean isUnder(String a, String b) {
		return isAbove(b,a);
	}
	
	public boolean grabEmpty() {
		return (grab == "null");
	}
	
	public boolean grabContains(String a) {
		return a.equals(this.getGrab());
	}
	
	public void drop(int column) {
		this.getWorld().get(column).add(grab);
		this.setGrab("null");
	}
	
	public void pick (int column) {
		this.setGrab(this.removeLastItem(column));
	}
	
	public boolean fitOver(String a, String b) {
		Block c = _blocks.get(a);
		Block d = _blocks.get(b);
		return (c.fitsInside(d) && !d.isSpecialShape());
	}
	
	public String getLastItem(int column) {
		return this.getWorld().get(column).get(this.getWorld().get(column).size());
	}
	
	public String removeLastItem(int column) {
		return this.getWorld().get(column).remove(this.getWorld().get(column).size()-1);
	}
	
	public String getGrab() {
		return grab;
	}

	public void setGrab(String grab) {
		this.grab = grab;
	}

	public ArrayList<ArrayList<String>> getWorld() {
		return this._world;
	}

	public void setWorld(ArrayList<ArrayList<String>> world) {
		this._world = world;
	}

	@Override
	public String toString() {
		return "World [grab=" + grab + ", world=" + _world + "]";
	}
	
}
