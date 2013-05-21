		    		if (fact.contains("Empty")) {
		    		
		    		
		    		
		    		
		    		
		    		
		    		int factKey = (Integer) it.next();
		    		String fact = preconditions.get(factKey);
		    		
		    			grabberEmpty = true;
		    		}
		    		if (fact.contains("Grabber")){
		    			grabbedItem = fact.split("Grabber ")[1];
		    			System.out.println(grabbedItem);
		    		}
		    		if (fact.contains("Top")) {
		    			String[] items = fact.split("OnTop ");
		    			if(!grabberEmpty) {
							String target = items[1];
							System.out.println(target + grabbedItem);
		    				if(target.contains("floor")||(_blocks.get(grabbedItem).fitOver(_blocks.get(target))))
		    						_actions.put(i++, "Drop"+ " " +items[1]);
		    			}
		    			else {
		    				if (!items[1].contains("floor"))
		    					_actions.put(i++, "Pick"+ " " + items[1]);
		    			}
		    		}