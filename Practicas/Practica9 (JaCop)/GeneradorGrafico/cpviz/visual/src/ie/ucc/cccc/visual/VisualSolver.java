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

public interface VisualSolver extends Visual {
	/**
	 * add unique root node for SearchTreeLog, before any other nodes are created
	 * @param id int
	 */
	public void addRootNode(int id);
	/**
	 * add a choice node for the SearchTreeLog, where the choice succeeded
	 * @param id
	 * @param parentId
	 * @param variableName
	 * @param size
	 * @param value
	 */
	public void addSuccessNode(int id, int parentId, String variableName,int size,int value);
	public void addSuccessNode(int id, int parentId, String variableName, int size, String choice);
	
	/**
	 * adds a failure node for the SearchTreeLog, where the attempted 
	 * choice failed due to propagation
	 * @param id
	 * @param parentId
	 * @param variableName
	 * @param size
	 * @param value
	 */
	public void addFailureNode(int id, int parentId, String variableName,int size, int value);		
	public void addFailureNode(int id, int parentId, String variableName, int size, String choice);
	/**
	 * label a Success node as a solution,i.e. the current variable assignment satisfies 
	 * all constraints
	 * @param id
	 */
	public void labelSolutionNode(int id);
	

}
