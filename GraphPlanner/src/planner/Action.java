package planner;



import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;


public class Action {
    private ArrayList<String> _preCondition;
    private ArrayList<String> effects;
    private String opType;
    private ArrayList<Action> mutexActions;

    public Action(ArrayList<String> preConditions, String opType) {
        this.opType = opType;
        this._preCondition = preConditions;
    }

	@Override
	public String toString() {
		return "Action [opTypepreCondition=" + opType + ", effects=" + effects
				+ ", preCondition=" + _preCondition + ", mutexActions=" + mutexActions + "]";
	}
	public void generateEffects () {
		Iterator it = this._preCondition.iterator();
		ArrayList<String> _effects = new ArrayList<String>();
		//PICK EFFECTS
		while (it.hasNext()) {
			String f1 = (String) it.next();
			if(f1.contains("Empty")) {
				String f2 = (String) it.next();
				if(f2.contains("OnTop")) {
					String target = f2.split("OnTop ")[1];
					String e1 = "Grabber " + target;
					_effects.add(e1);
					String e2 = "ÂOnTop " + target;
					_effects.add(e2);
				}
				String f3 = (String) it.next();
				if(f3.contains("Over")) {
					String target = f3.split("Over ")[1].split(" ")[1];
					String e3 = "OnTop " + target;
					_effects.add(e3);
				}
			}
			if(f1.contains("Grabber ")) {
				String t1 = f1.split("Grabber ")[1];
				String e1 = "Empty";
				_effects.add(e1);
				String e2 = "OnTop " +t1;
				_effects.add(e2);
				String f2 = (String) it.next();
				if (f2.contains("OnTop")) {
					String t2 = f2.split("OnTop ")[1];
					String e3 = "ÂOnTop " +t2;
					_effects.add(e3);
					String e4 = "Over "+t1+" " +t2;
					_effects.add(e4);
				}
			}
		}
		this.effects = _effects;
	}
	
	public static HashMap<Integer,Action> generateActions(HashMap<String,Block> _blocks) {
		HashMap<Integer,Action> _actions = new HashMap<Integer, Action>();
		int k = 0;
		//GENERATE PICKS
		Iterator it = _blocks.keySet().iterator();
		String p1 = "Empty";
		//PICKS OVER BLOCKS 
		while (it.hasNext()) {
			Iterator it2 = _blocks.keySet().iterator();
			String keyA = (String) it.next();
			while (it2.hasNext()) {
				String keyB = (String) it2.next();
				if (!keyA.equals(keyB)) {
					ArrayList<String> _preconditions = new ArrayList<String>();
					_preconditions.add(p1);
					String p2 = "OnTop " + keyA;
					_preconditions.add(p2);
					String p3 = "Over " + keyA +" " + keyB;
					_preconditions.add(p3);
					Action action = new Action(_preconditions, "Pick " + keyA + " " +keyB);
					action.generateEffects();
					_actions.put(k++, action);
				}
			}
//			PICKS OVER FLOOR
			for (int i = 0; i < 10; i++) {
				ArrayList<String> _preconditions = new ArrayList<String>();
				_preconditions.add(p1);
				String p2 = "OnTop " + keyA;
				_preconditions.add(p2);
				String p3 = "Over " + keyA +" floor" +i;
				_preconditions.add(p3);
				Action action = new Action(_preconditions, "Pick " + keyA + " floor" +i);
				action.generateEffects();
				_actions.put(k++, action);
			}
		}
			System.out.println(k);
//			//GENERATE DROPS
			it = _blocks.keySet().iterator();
			//DROPS OVER BLOCKS 
			while (it.hasNext()) {
				Iterator it2 = _blocks.keySet().iterator();
				String keyA = (String) it.next();
				p1 = "Grabber " + keyA;
				while (it2.hasNext()) {
					ArrayList<String> _preconditions = new ArrayList<String>();
					String keyB = (String) it2.next();
					Block bA = _blocks.get(keyA);
					Block bB = _blocks.get(keyB);
					if ((!keyA.equals(keyB))&&bA.fitOver(bB)) {
						_preconditions.add(p1);
						String p2 = "OnTop " + keyB;
						_preconditions.add(p2);
						Action action = new Action(_preconditions, "Drop " + keyA + " " + keyB);
						action.generateEffects();
						_actions.put(k++, action);
					}
				}

			//DROPS OVER FLOOR
			for (int i = 0; i < 10; i++) {
				ArrayList<String> _preconditions = new ArrayList<String>();
				_preconditions.add(p1);
				String p2 = "OnTop " + "floor" + i;
				_preconditions.add(p2);
				Action action = new Action(_preconditions, "Drop " + keyA + " floor" +i);
				action.generateEffects();
				_actions.put(k++, action);
			}
		}
		return _actions;
	}

	public ArrayList<String> get_preCondition() {
		return _preCondition;
	}

	public void set_preCondition(ArrayList<String> _preCondition) {
		this._preCondition = _preCondition;
	}

	public ArrayList<String> getEffects() {
		return effects;
	}

	public void setEffects(ArrayList<String> effects) {
		this.effects = effects;
	}

	public String getOpType() {
		return opType;
	}

	public void setOpType(String opType) {
		this.opType = opType;
	}

	public ArrayList<Action> getMutexActions() {
		return mutexActions;
	}

	public void setMutexActions(ArrayList<Action> mutexActions) {
		this.mutexActions = mutexActions;
	}

}