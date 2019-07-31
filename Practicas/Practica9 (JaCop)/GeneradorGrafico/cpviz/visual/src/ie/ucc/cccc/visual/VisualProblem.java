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

public interface VisualProblem extends Visual {
	/**
	 * register a constraint for visualization in the ConstraintVariableLog
	 * it will be drawn at every search step or when the log is updated manually
	 * @param constraint
	 */
	public void register(Constraint constraint);
	
	public void register(Var var);
	
	public void register(Var[] varArray);
	
	public void register(Var[][] varMatrix);

	public void snapshot();

	// for implementors only
	public void startTagArgument(String index);
	public void startTagArgument(int index);
	public void endTagArgument();
	
	public void startTagCollection(String index);
	public void startTagCollection(int index);
	public void endTagCollection();
	
	public void startTagTuple(String index);
	public void startTagTuple(int index);
	public void endTagTuple();

	/**
	 * report current state of variable Var 
	 * the position/name of the variable is reported via the index
	 * @param index
	 * @param var
	 */
	void tagVariable(Var var);
	void tagVariable(String index, Var var);
	void tagVariable(int index, Var var);
	
	void tagInteger(String index,int value);
	void tagInteger(int index,int value);

}
