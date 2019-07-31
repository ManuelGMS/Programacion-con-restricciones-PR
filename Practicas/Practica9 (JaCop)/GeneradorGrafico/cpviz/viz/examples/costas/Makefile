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
MIDDLE/tree.tre: top.ecl
	eclipse -b "top" -e "top:top"


pdf:
	svg2pdf MIDDLE/*.svg MODEL/*.svg NAIVE/*.svg

clean:
	rm -f MIDDLE/configuration_*.xml MODEL/configuration_*.xml NAIVE/configuration_*.xml
	rm -f MIDDLE/*.tre MODEL/*.tre NAIVE/*.tre
	rm -f MIDDLE/*.viz MODEL/*.viz NAIVE/*.viz
	rm -f MIDDLE/*.svg MODEL/*.svg NAIVE/*.svg
	rm -f MIDDLE/*.idx MODEL/*.idx NAIVE/*.idx
	rm -f MIDDLE/*.pdf MODEL/*.pdf NAIVE/*.pdf
