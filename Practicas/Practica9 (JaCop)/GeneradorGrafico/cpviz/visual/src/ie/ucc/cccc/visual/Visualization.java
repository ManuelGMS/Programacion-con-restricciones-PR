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

import java.io.PrintWriter;
import java.util.ArrayList;

public class Visualization implements VisualSolver,VisualProblem {
PrintWriter treeWriter;
PrintWriter vizWriter;
ArrayList<VisualEntry> visualEntries=new ArrayList<VisualEntry>();
int stateId = 0;
int vId = 0;

	public Visualization(PrintWriter treeWriter,PrintWriter vizWriter){
		this.treeWriter = treeWriter;
		treeWriter.println("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
		treeWriter.println("<tree version=\"1.0\"");
		treeWriter.println("  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
		treeWriter.println("  xsi:noNamespaceSchemaLocation=\"tree.xsd\">");
		this.vizWriter = vizWriter;
		vizWriter.println("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
		vizWriter.println("<visualization version=\"1.0\"");
		vizWriter.println("  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
		vizWriter.println("  xsi:noNamespaceSchemaLocation=\"visualization.xsd\">");
		stateId = 0;
		vId = 0;
	}
	
	@Override
	public void finalize() {
		treeWriter.println("</tree>");
		vizWriter.println("</visualization>");
		
	}
	
	@Override
	public void snapshot(){
		snapshot(-1);
	}
	
	protected void snapshot(int treeNode){
		stateId++;
		vizWriter.println("<state id=\""+stateId+"\" treeNode=\""+treeNode+"\">");
		for(VisualEntry entry : visualEntries){
			vizWriter.println("<visualizer_state id=\""+entry.getNr()+"\">");
			switch (entry.getType()){
			case CONSTRAINT:
				entry.getConstraint().snapshot((VisualProblem) this);
				break;
			case VECTOR:
				snapshot(entry.getVector());
				break;
			case MATRIX:
				snapshot(entry.getMatrix());
				break;
		
			case VARIABLE:
				tagVariable(0,entry.getVariable());
				break;
			}
			vizWriter.println("</visualizer_state>");
		}
		vizWriter.println("</state>");
	}
	
	
	@Override
	public void addFailureNode(int id, int parentId, String variableName,
			int size, String choice) {
		treeWriter.println("<failc id=\""+id+
				"\" parent=\""+parentId+
				"\" name=\""+variableName+
				"\" size=\""+size+
				"\" choice=\""+choice+
				"\"/>");
		snapshot(id);
	}

	@Override
	public void addFailureNode(int id, int parentId, String variableName,
			int size, int value) {
		treeWriter.println("<fail id=\""+id+
				"\" parent=\""+parentId+
				"\" name=\""+variableName+
				"\" size=\""+size+
				"\" value=\""+value+
				"\"/>");
		snapshot(id);
	}

	@Override
	public void addRootNode(int id) {
		treeWriter.println("<root id=\""+id+"\"/>");
		snapshot(id);
	}

	@Override
	public void labelSolutionNode(int id) {
		treeWriter.println("<succ id=\""+id+"\"/>");
		snapshot(id);
	}

	@Override
	public void addSuccessNode(int id, int parentId, String variableName,
			int size, String choice) {
		treeWriter.println("<tryc id=\""+id+
				"\" parent=\""+parentId+
				"\" name=\""+variableName+
				"\" size=\""+size+
				"\" choice=\""+choice+
				"\"/>");
		snapshot(id);
	}

	@Override
	public void addSuccessNode(int id, int parentId, String variableName, int size,
			int value) {
		treeWriter.println("<try id=\""+id+
				"\" parent=\""+parentId+
				"\" name=\""+variableName+
				"\" size=\""+size+
				"\" value=\""+value+
				"\"/>");
		snapshot(id);
	}

	@Override
	public void tagVariable(Var var) {
		vizWriter.println("<dvar index=\""+var.getIndex()+
				"\" domain=\""+var.getMin()+".."+var.getMax()+"\"/>");
	}

	@Override
	public void tagVariable(String index,Var var) {
		vizWriter.println("<dvar index=\""+index+
				"\" domain=\""+getDomainAsList(var)+"\"/>");
	}
	@Override
	public void tagVariable(int index,Var var) {
		vizWriter.println("<dvar index=\""+index+
				"\" domain=\""+getDomainAsList(var)+"\"/>");
	}
	
	protected String getDomainAsList(Var var){
		String string=""+var.getMin();
		for(int i=var.getMin()+1;i<=var.getMax();i++){
			if(var.isInDomain(i)) {
				string = string+" "+i;
			}
		}
		return string;
	}
	@Override
	public void tagInteger(String index,int value) {
		vizWriter.println("<integer index=\""+index+
				"\" value=\""+value+"\"/>");
	}
	@Override
	public void tagInteger(int index,int value) {
		vizWriter.println("<integer index=\""+index+
				"\" value=\""+value+"\"/>");
	}

	public void snapshot(Var[] vector){
		int n = vector.length;
		for(int i=0;i<n;i++){
			tagVariable(Integer.toString(i),vector[i]);
		}
	}
	
	public void snapshot(Var[][] matrix){
	}
	

	@Override
	public void register(Constraint constraint) {
		visualEntries.add(new VisualEntry(constraint,vId++));
	}

	@Override
	public void register(Var var) {
		visualEntries.add(new VisualEntry(var,vId++));
	}

	@Override
	public void register(Var[] varArray) {
		visualEntries.add(new VisualEntry(varArray,vId++));
	}

	@Override
	public void register(Var[][] varMatrix) {
		visualEntries.add(new VisualEntry(varMatrix,vId++));
	}

	public void visualizer(int id,String type,String display,
			int x,int y,int width,int height,int group,int min,int max){
		vizWriter.println("<visualizer id=\""+id+
				"\" type=\""+type+
				"\" display=\""+display+
				"\" x=\""+x+
				"\" y=\""+y+
				"\" width=\""+width+
				"\" height=\""+height+
				"\" group=\""+group+
				"\" min=\""+min+
				"\" max=\""+max+
				"\"/>");
	}
	
	public void startTagArgument(String index){
		vizWriter.println("<argument index=\""+index+"\">");
	}
	public void startTagArgument(int index){
		vizWriter.println("<argument index=\""+index+"\">");
	}
	public void endTagArgument(){
		vizWriter.println("</argument>");
	}
	public void startTagCollection(String index){
		vizWriter.println("<collection index=\""+index+"\">");
	}
	public void startTagCollection(int index){
		vizWriter.println("<collection index=\""+index+"\">");
	}
	public void endTagCollection(){
		vizWriter.println("</collection>");
	}
	public void startTagTuple(String index){
		vizWriter.println("<tuple index=\""+index+"\">");
	}
	public void startTagTuple(int index){
		vizWriter.println("<tuple index=\""+index+"\">");
	}
	public void endTagTuple(){
		vizWriter.println("</tuple>");
	}
}
