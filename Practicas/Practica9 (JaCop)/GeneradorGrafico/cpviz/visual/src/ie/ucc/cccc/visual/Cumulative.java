// BEGIN LICENSE BLOCK
// Version: CMPL 1.1
//
// The contents of this file are subject to the Cisco-style Mozilla Public
// License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License.  You may obtain a copy of the License
// at www.eclipse-clp.org/license.
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
// the License for the specific language governing rights and limitations
// under the License. 
// 
// The Original Code is  CPViz Constraint Visualization System
// The Initial Developer of the Original Code is  Helmut Simonis
// Portions created by the Initial Developer are
// Copyright (C) 2009-2010 Helmut Simonis
// 
// Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
//			
// 
// END LICENSE BLOCK
// ----------------------------------------------------------------------
package ie.ucc.cccc.visual;

public class Cumulative implements Constraint {
Task[] tasks;
int limit;

	public Cumulative(Task[] tasks,int limit){
		this.tasks = tasks;
		this.limit = limit;
	}
	@Override
	public void snapshot(VisualProblem visual) {
		visual.startTagArgument("tasks");
		int n = tasks.length;
		for(int i=0;i<n;i++){
			visual.startTagTuple(i);
			visual.tagVariable("start",tasks[i].getStart());
			visual.tagInteger("dur",tasks[i].getDuration());
			visual.tagInteger("res",tasks[i].getResourceUse());
			visual.endTagTuple();		
		}
		visual.endTagArgument();
		visual.startTagArgument("limit");
		visual.tagInteger(0,limit);
		visual.endTagArgument();

	}

}
