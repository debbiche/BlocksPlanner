package planner;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class Fact {
	/*
	Ontop(a,b)
	not Ontop(a,b)
	...
	Ontop(l,m)
	not OnTop(l,m)
	
	EmptyGrabber
	notEmptyGrabber
	
	Grabber(a)
	...
	Grabber(m)
	
	TopOfTheColumn(c0)
	...
	TopOfTheColumn(c9)
	
	+ 
	all finalfacts
	all not finalfacts
	*/
}
public void generateActions() {
    HashMap<Integer,Action> _actions = new HashMap<Integer, Action>();
    Iterator it = _preconditions.keySet().iterator();
    String grabbedItem = "null";		    
    boolean grabberEmpty=false;
    int i = 0, pKey,pKey2;
    while (it.hasNext()) { // precondition pair comparation
    	pKey = (Integer) it.next();
    	String fact = _preconditions.get(pKey);
    	if (fact.contains("Empty") || fact.contains("Grabber")) {
    		ArrayList<Integer> _preconditionsSubSet = new ArrayList<Integer>();
    		_preconditionsSubSet.add(pKey);
    		Iterator it2 = _preconditions.keySet().iterator();
    		while(it2.hasNext()) {
    			pKey2 = (Integer) it2.next();
    			if(pKey != pKey2) {
    				if (this.id == 0) {
    					_preconditionsSubSet.add(pKey2);
    				}
    				else if(!_mutexBetweenPreconditions.get(pKey).contains(pKey2)) _preconditionsSubSet.add(pKey2);
    			}
    		}
    		_actions.putAll(generateActionsSubSet(i++,_preconditionsSubSet));
    	}		    	
    }
   this._actions = _actions;
}

private HashMap<Integer,Action> generateActionsSubSet(int actionId, ArrayList<Integer> _preconditionsSubSet) {			
	int GrabberState = _preconditionsSubSet.get(0);
	HashMap<Integer, Action> _actionSubSet = new HashMap<Integer, Action>();
		for (int i = 1; i < _preconditionsSubSet.size(); i++) {
			for (int j = i+1; j < _preconditionsSubSet.size(); j++) {
				try {
					Action action = generateAction(0, i, j);
					_actionSubSet.put(actionId++, action);
				} catch (Exception e) {
					// TODO: handle exception
				}
			}
		}
	System.out.println(_actionSubSet.toString());
	return _actionSubSet;
}

public Action generateAction(int iG,int iF1, int iF2) throws Exception {
	String grabberState = _preconditions.get(iG);
	String f1="",f2="",f1t1="",f1t2="",f2t1="",f2t2 ="";
	if (grabberState.contains("Empty")) {
		 f1 = _preconditions.get(iF1);
		 f2 = _preconditions.get(iF2);
		if (f1.contains("OnOver")&& f2.contains("Top")) {
			f2t1 = f2.split("OnTop ")[1];
			f1t1 = f1.split("Over ")[1].split(" ")[1];
			f1t2 = f1.split("Over ")[1].split(" ")[2];
			if (f2t1.equals(f1t1)) {
				if (!f1t2.contains("floor")) {
					ArrayList<Integer> preConditions = new ArrayList<Integer>();
					preConditions.add(iG); preConditions.add(iF1); preConditions.add(iF2);
					Action action = new Action(preConditions, "Pick " + f1t1 +" " +f1t2);
					return action;
				}
			}
		}
		if(f1.contains("OnTop")&& f2.contains("Over")) {
			f1t1 = f2.split("OnTop ")[1];
			f2t1 = f1.split("OnOver ")[1].split(" ")[1];
			f2t2 = f1.split("OnOver ")[1].split(" ")[2];
			if (f2t1.equals(f1t1)) {
				if(!f2t2.contains("floor")) {
					ArrayList<Integer> preConditions = new ArrayList<Integer>();
					preConditions.add(iG); preConditions.add(iF1); preConditions.add(iF2);
					Action action = new Action(preConditions, "Pick " + f2t1 +" " +f2t2);

					return action;
				}
			}
		}
	}
	
	if (grabberState.contains("Grabber ")) {
		 f1 = _preconditions.get(iF1);
		 f2 = _preconditions.get(iF2);
		 String gT = grabberState.split("Grabber ")[1];
		 if(f1.contains("OnTop")) {
				f1t1 = f1.split("OnTop ")[1];
				Block gt = (Block) _blocks.get(gT);
				Block bt = (Block) _blocks.get(f1t1);
				System.out.println(f1t1);
				if((f1t1.contains("floor"))||f1t1.equals(gT) && gt.fitOver(bt)) {
					ArrayList<Integer> preConditions = new ArrayList<Integer>();
					preConditions.add(iG); preConditions.add(iF1);
					Action action = new Action(preConditions, "Drop " + gT +" " +f1t1);
					 System.out.println(f1+f2);
					return action;
				}
		 }
		 if((f1t1.contains("floor"))||f2.contains("OnTop")) {
				f2t1 = f2.split("OnTop ")[1];
				Block gt = (Block) _blocks.get(gT);
				Block bt = (Block) _blocks.get(f2t1);
				if(f2t1.equals(gT) && gt.fitOver(bt)) {
					ArrayList<Integer> preConditions = new ArrayList<Integer>();
					preConditions.add(iG); preConditions.add(iF2);
					Action action = new Action(preConditions, "Drop " + gT +" " +f2t1);
					System.out.println(action.toString());
					return action;
				}
		 }
	}
	throw new Exception();
}