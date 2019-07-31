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

import ie.ucc.cccc.viz.Viz;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class Test {


	/**
	 * @param args
	 */
	public static void main(String[] args) {
		PrintWriter treeWriter = null;
		PrintWriter vizWriter = null;
		String treeString = "tree.tree";
		String vizString = "vis.viz";
		String configString = "configuration.xml";
		try {
			File treeFile = new File(".",treeString);
			treeWriter = new PrintWriter(new FileWriter(treeFile));
			File vizFile = new File(".",vizString);
			vizWriter = new PrintWriter(new FileWriter(vizFile));
		} 
		catch (IOException e) {
			System.out.println("IO Error: "+e);
		}
		Visualization cpViz = new Visualization(treeWriter,vizWriter);
		int n =8;
		DVar[] vars = new DVar[n];
		for(int i=0;i<n;i++){
			vars[i] = new DVar(i,0,n-1);
		}
		Task[] tasks = new Task[n];
		for(int i=0;i<n;i++){
			tasks[i] = new Task(vars[i],2,2);
		}
		Alldifferent alldifferent = new Alldifferent(vars);
		cpViz.register(vars);
		cpViz.visualizer(0,"vector", "expanded",0,0,n,n,1,0,n);
		cpViz.register(alldifferent);
		cpViz.visualizer(1,"alldifferent", "expanded",0,0,n,n,1,0,n);
		Cumulative cumulative = new Cumulative(tasks,5);
		cpViz.register(cumulative);
		cpViz.visualizer(2,"cumulative", "profile",0,0,n,n,1,0,n);
		cpViz.visualizer(3,"cumulative", "gantt",0,0,n,n,1,0,n);
		cpViz.snapshot(-1);
		cpViz.addRootNode(0);
		cpViz.addSuccessNode(1,0,"a",2,1);
		cpViz.addSuccessNode(2,1,"b",4,2);
		cpViz.addFailureNode(3,2,"c",3,1);
		cpViz.addSuccessNode(4,2,"c",3,2);
		cpViz.addSuccessNode(5,4,"d",2,2);
		cpViz.labelSolutionNode(5);
		
		cpViz.finalize();
		
		treeWriter.close();
		vizWriter.close();
		// create SVG from files
		try{
			Viz.runViz(configString, treeString, vizString);
		} catch (Exception e) {
			System.out.println("FileError "+e);
			e.printStackTrace();
		}
	}

}
