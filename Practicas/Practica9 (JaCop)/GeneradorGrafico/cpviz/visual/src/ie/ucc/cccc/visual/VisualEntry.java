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

public class VisualEntry {
int nr;
Constraint constraint;
Var var;
Var[] vector;
Var[][] matrix;
VisualEntryType type;

public VisualEntry(Constraint c,int nr){
	this.nr = nr;
	this.constraint = c;
	this.type = VisualEntryType.CONSTRAINT;
}
public VisualEntry(Var var,int nr){
	this.nr = nr;
	this.var = var;
	this.type = VisualEntryType.VARIABLE;
}
public VisualEntry(Var[] vector,int nr){
	this.nr = nr;
	this.vector = vector;
	this.type = VisualEntryType.VECTOR;
}
public VisualEntry(Var[][] matrix,int nr){
	this.nr = nr;
	this.matrix = matrix;
	this.type = VisualEntryType.MATRIX;
}
	
	public int getNr(){
		return nr;
	}
	public VisualEntryType getType(){
		return type;
	}
	public Constraint getConstraint(){
		return constraint;
	}
	public Var getVariable(){
		return var;
	}
	public Var[] getVector(){
		return vector;
	}
	public Var[][] getMatrix(){
		return matrix;
	}
			
}
