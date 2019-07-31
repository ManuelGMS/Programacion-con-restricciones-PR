# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  CPViz Constraint Visualization System
# The Initial Developer of the Original Code is  Helmut Simonis
# Portions created by the Initial Developer are
# Copyright (C) 2009-2010 Helmut Simonis
# 
# Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
#			
# 
# END LICENSE BLOCK
# ----------------------------------------------------------------------
NAIVE/tree.tre: top.ecl
	eclipse -b "top" -e "top:top"

pdf:
	svg2pdf CREDIT/*.svg FF/*.svg LAYERED/*.svg NAIVE/*.svg RANDOM/*.svg

clean:
	rm -f CREDIT/*.tre FF/*.tre LAYERED/*.tre NAIVE/*.tre RANDOM/*.tre
	rm -f CREDIT/*.viz FF/*.viz LAYERED/*.viz NAIVE/*.viz RANDOM/*.viz
	rm -f CREDIT/*.svg FF/*.svg LAYERED/*.svg NAIVE/*.svg RANDOM/*.svg
	rm -f CREDIT/*.idx FF/*.idx LAYERED/*.idx NAIVE/*.idx RANDOM/*.idx
	rm -f CREDIT/*.pdf FF/*.pdf LAYERED/*.pdf NAIVE/*.pdf RANDOM/*.pdf
