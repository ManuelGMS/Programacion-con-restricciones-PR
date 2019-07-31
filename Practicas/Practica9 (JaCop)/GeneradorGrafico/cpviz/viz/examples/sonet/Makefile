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
T21/tree.tre: top.ecl
	eclipse -b "top" -e "top:top"


pdf:
	svg2pdf T21/*.svg T22/*.svg T23/*.svg

clean:
	rm -f T21/*.tre T22/*.tre T23/*.tre
	rm -f T21/*.viz T22/*.viz T23/*.viz
	rm -f T21/*.svg T22/*.svg T23/*.svg
	rm -f T21/*.idx T22/*.idx T23/*.idx
	rm -f T21/*.pdf T22/*.pdf T23/*.pdf
