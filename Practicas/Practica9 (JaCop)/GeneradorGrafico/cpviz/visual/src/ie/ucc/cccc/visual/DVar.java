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

public class DVar implements Var {
int index;
int min;
int max;

	public DVar(int index,int min,int max){
		this.index = index;
		this.min = min;
		this.max = max;
	}
	
	@Override
	public int getIndex() {
		// TODO Auto-generated method stub
		return index;
	}

	@Override
	public int getMax() {
		// TODO Auto-generated method stub
		return max;
	}

	@Override
	public int getMin() {
		// TODO Auto-generated method stub
		return min;
	}
	
	// in reality this is more complex
	@Override
	public Boolean isInDomain(int v){
		if (v >= min && v <= max) {
			return true;
		} else {
			return false;
		}
	}

}
