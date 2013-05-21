package planner;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

public class ActionLayer {
	
	int parentActionLayer;
	HashMap<Integer,ArrayList<Integer>> _mutexBetweenEffects = new HashMap<Integer, ArrayList<Integer>>();
	HashMap<Integer,ArrayList<Integer>> _mutexBetweenActions = new HashMap<Integer, ArrayList<Integer>>();
	HashMap<Integer,ArrayList<Integer>> _mutexBetweenPreconditions = new HashMap<Integer, ArrayList<Integer>>() ;
	HashMap<Integer,Action> _actions = new HashMap<Integer,Action>();
	HashMap<String,Integer> _preconditions;
	HashMap<String,Integer> _effects = new HashMap<String, Integer>();
	static HashMap<Integer,ActionLayer>  _layers = new HashMap<Integer, ActionLayer>();
	int id= 0;
	
	public ActionLayer(int i, HashMap<String, Integer> _preconditions) {
		super();
		this.id = i;
		System.out.println(i);
		this.parentActionLayer = i-1;
		if (id != 0)
			this._mutexBetweenPreconditions.putAll(_layers.get(parentActionLayer).get_mutexBetweenEffects());
		else
			this._mutexBetweenPreconditions = new HashMap<Integer, ArrayList<Integer>>();
		if (id == 0) this._preconditions = _preconditions;
		else this._preconditions = _layers.get(parentActionLayer).get_effects();
	}
	
	public void createEffects() {
		this._effects.putAll(_preconditions);
		Iterator it  = _actions.values().iterator();
		while (it.hasNext()) {
			Action action = (Action) it.next();
			ArrayList<String> aEffects= action.getEffects();
			for (int i = 0; i < aEffects.size(); i++) {
				if(!this._effects.containsKey(aEffects.get(i))) _effects.put(aEffects.get(i), this._effects.size()-1);
			}
		}
	}
	public void createActions(HashMap<Integer,Action> allActions) {
		Iterator it = allActions.keySet().iterator();
		int k = 0;
		while (it.hasNext()) {//ITERATE OVER ALL THE ACTIONS
			boolean actionAdded = false;
			int actionKey = (Integer) it.next();
			Iterator pcsAction = allActions.get(actionKey).get_preCondition().iterator();
			ArrayList<Integer> _pKeys = new ArrayList<Integer>();
			while (pcsAction.hasNext()) { //ITERATE OVER ALL THE PRECONDITIONS OF THE CURRENT ACTION
				String p1 = (String) pcsAction.next();
				if (!_preconditions.containsKey(p1)) {//IF THE PRECONDITION OF THE ACTION IS NOT ON PRECONDITIONS OF THE LAYER WE BREAK;
					actionAdded = true;
					break; 
				}
				else _pKeys.add(_preconditions.get(p1));
			}
			Iterator it2 = _mutexBetweenPreconditions.values().iterator();
			if(!_pKeys.isEmpty())
			while ((id == 0 || (it2.hasNext()))&& !actionAdded) {
				int collisions = 0;
				if(id !=0) {
					ArrayList<Integer> mutex = (ArrayList<Integer>) it2.next();
					for (int i = 0; i < _pKeys.size(); i++) {
						if(mutex.contains(_pKeys.get(i))) collisions++;
					}
				}
				if (collisions <=1) {
					_actions.put(k++,allActions.get(actionKey));
					actionAdded = true;

				}
			}
		}
		this._actions = _actions;
		createActionMutexByPreconditions();
		createActionMutexByEffects();
		createActionsMutesByEffectsAndPreconditions();
		createEffects();
		createEffectsMutex();
		_layers.put(this.id, this);
	}
	
	private void createEffectsMutex() {
		for (int i = 0; i < _actions.size(); i++) {
			ArrayList<Integer> _mutexActionEffect = new ArrayList<Integer>();
			for(int j=i+1; j< _actions.size(); j++) {
				ArrayList<String> p1 = _actions.get(i).getEffects();
				ArrayList<String> p2 = _actions.get(j).getEffects();
				for (int k = 0; k < p1.size(); k++) {
					for(int l=k+1; l< p2.size(); l++) {
						if(p1.get(k).contains(p2.get(l)) && ((p1.get(k).startsWith("Â")||p2.get(l).startsWith("Â"))))
								if(p1.get(k).contains("Grabber") && p2.get(l).contains("Grabber")) {
									String targetp1 = p1.get(k).split("Grabber ")[1];
									String targetp2 = p2.get(l).split("Grabber ")[1];
								if(!targetp1.equals(targetp2)) _mutexActionEffect.add(j);
								}
					}
				}
			}
			_mutexBetweenEffects.put(i, _mutexActionEffect);
		}
	}
	private void createActionMutexByPreconditions() {
		for (int i = 0; i < _actions.size(); i++) {
			ArrayList<Integer> _mutexAction = new ArrayList<Integer>();
			for(int j=i+1; j< _actions.size(); j++) {
				ArrayList<String> p1 = _actions.get(i).get_preCondition();
				ArrayList<String> p2 = _actions.get(j).get_preCondition();
				for (int k = 0; k < p1.size(); k++) {
					for(int l=k+1; l< p2.size(); l++) {
						if(p1.get(k).contains(p2.get(l)) && ((p1.get(k).startsWith("Â")||p2.get(l).startsWith("Â"))))
								if(p1.get(k).contains("Grabber") && p2.get(l).contains("Grabber")) {
									String targetp1 = p1.get(k).split("Grabber ")[1];
									String targetp2 = p2.get(l).split("Grabber ")[1];
								if(!targetp1.equals(targetp2)) _mutexAction.add(j);
								}
					}
				}
			}
			_mutexBetweenActions.put(i, _mutexAction);
		}
	}
	
	private void createActionMutexByEffects() {
		for (int i = 0; i < _actions.size(); i++) {
			ArrayList<Integer> _mutexAction = _mutexBetweenActions.get(i);
			for(int j=i+1; j< _actions.size(); j++) {
				ArrayList<String> p1 = _actions.get(i).getEffects();
				ArrayList<String> p2 = _actions.get(j).getEffects();
				for (int k = 0; k < p1.size(); k++) {
					for(int l=k+1; l< p2.size(); l++) {
						if(p1.get(k).contains(p2.get(l)) && (p1.get(k).startsWith("Â")||p2.get(l).startsWith("Â")))
								if(p1.get(k).contains("Grabber") && p2.get(l).contains("Grabber")) {
									String targetp1 = p1.get(k).split("Grabber ")[1];
									String targetp2 = p2.get(l).split("Grabber ")[1];
								if(!targetp1.equals(targetp2)) 
									if (!_mutexBetweenActions.get(i).contains(j)) _mutexBetweenActions.get(i).add(j);
								}
					}
				}
			}
			_mutexBetweenActions.put(i, _mutexAction);
		}
	}
	
	private void createActionsMutesByEffectsAndPreconditions() {
		for (int i = 0; i < _actions.size(); i++) {
			ArrayList<Integer> _mutexAction = _mutexBetweenActions.get(i);
			for(int j=0; j< _actions.size(); j++) {
				if (i == j) break;
				ArrayList<String> p1 = _actions.get(i).get_preCondition();
				ArrayList<String> p2 = _actions.get(j).getEffects();
				for (int k = 0; k < p1.size(); k++) {
					for(int l=k+1; l< p2.size(); l++) {
						if(p1.get(k).contains(p2.get(l)) && (p1.get(k).startsWith("Â")||p2.get(l).startsWith("Â")))
								if(p1.get(k).contains("Grabber") && p2.get(l).contains("Grabber")) {
									String targetp1 = p1.get(k).split("Grabber ")[1];
									String targetp2 = p2.get(l).split("Grabber ")[1];
								if(!targetp1.equals(targetp2)) 
									if (!_mutexBetweenActions.get(i).contains(j)) _mutexBetweenActions.get(i).add(j);
								}
						}
					}
			}
			_mutexBetweenActions.put(i, _mutexAction);
		}
	}
	
	private void createFactsMutex() {
		// TODO Auto-generated method stub
		/*
		if facts negation of each other mutex
		if all pairwise actions are mutex for 2 given facts these 2 facts are mutex
		*/
		/*
			for(i=0; i < Facts.length; i++){
				for(j = i+1; j < Facts.length; j++){
					if (all actions are mutex for i and j) then mutex between i and j
					if (fact instanceof finalfact) Add to derivedFactsMutex
				}
			}*/
		}
	
		private static void linkActionToEffects() {
			// TODO Create the links
			
		}
		private static void linkActionToPrecondition() {
			// TODO Create the links
			
		}
		
		public HashMap<Integer,String> generateEffects() {
			return null;
		}
		
		
		
		public int getParentActionLayer() {
			return parentActionLayer;
		}

		public void setParentActionLayer(int parentActionLayer) {
			this.parentActionLayer = parentActionLayer;
		}

		public HashMap<Integer, ArrayList<Integer>> get_mutexBetweenEffects() {
			return _mutexBetweenEffects;
		}

		public void set_mutexBetweenEffects(
				HashMap<Integer, ArrayList<Integer>> _mutexBetweenEffects) {
			this._mutexBetweenEffects = _mutexBetweenEffects;
		}

		public HashMap<Integer, ArrayList<Integer>> get_mutexBetweenActions() {
			return _mutexBetweenActions;
		}

		public void set_mutexBetweenActions(
				HashMap<Integer, ArrayList<Integer>> _mutexBetweenActions) {
			this._mutexBetweenActions = _mutexBetweenActions;
		}

		public HashMap<Integer, ArrayList<Integer>> get_mutexBetweenPreconditions() {
			return _mutexBetweenPreconditions;
		}

		public void set_mutexBetweenPreconditions(
				HashMap<Integer, ArrayList<Integer>> _mutexBetweenPreconditions) {
			this._mutexBetweenPreconditions = _mutexBetweenPreconditions;
		}

		public HashMap<Integer, Action> get_actions() {
			return _actions;
		}

		public void set_actions(HashMap<Integer, Action> _actions) {
			this._actions = _actions;
		}

		public HashMap<String, Integer> get_preconditions() {
			return _preconditions;
		}

		public void set_preconditions(HashMap<String, Integer> _preconditions) {
			this._preconditions = _preconditions;
		}

		public HashMap<String, Integer> get_effects() {
			return _effects;
		}

		public void set_effects(HashMap<String, Integer> _effects) {
			this._effects = _effects;
		}

		public HashMap<Integer, ActionLayer> get_layers() {
			return _layers;
		}

		public void set_layers(HashMap<Integer, ActionLayer> _layers) {
			this._layers = _layers;
		}

		public int getId() {
			return id;
		}

		public void setId(int id) {
			this.id = id;
		}
	
}
