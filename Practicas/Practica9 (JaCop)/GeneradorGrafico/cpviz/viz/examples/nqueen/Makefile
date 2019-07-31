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
FULL/tree.tre: top.ecl
	eclipse -b "top.ecl" -e "top:top"

pdf:
	svg2pdf FULL/*.svg COMPACT/*.svg CREDIT/*.svg FF/*.svg FIRST_FAIL/*.svg 
	svg2pdf MIDDLE/*.svg NAIVE/*.svg QUEEN4/*.svg COMPACT4/*.svg

clean:
	rm -f FULL/*.tre COMPACT/*.tre CREDIT/*.tre FF/*.tre FIRST_FAIL/*.tre 
	rm -f MIDDLE/*.tre NAIVE/*.tre QUEEN4/*.tre COMPACT4/*.tre	
	rm -f FULL/*.viz COMPACT/*.viz CREDIT/*.viz FF/*.viz FIRST_FAIL/*.viz 
	rm -f MIDDLE/*.viz NAIVE/*.viz QUEEN4/*.viz COMPACT4/*.viz
	rm -f FULL/*.svg COMPACT/*.svg CREDIT/*.svg FF/*.svg FIRST_FAIL/*.svg 
	rm -f MIDDLE/*.svg NAIVE/*.svg QUEEN4/*.svg COMPACT4/*.svg
	rm -f FULL/*.idx COMPACT/*.idx CREDIT/*.idx FF/*.idx FIRST_FAIL/*.idx 
	rm -f MIDDLE/*.idx NAIVE/*.idx QUEEN4/*.idx COMPACT4/*.idx
	rm -f FULL/*.pdf COMPACT/*.pdf CREDIT/*.pdf FF/*.pdf FIRST_FAIL/*.pdf 
	rm -f MIDDLE/*.pdf NAIVE/*.pdf QUEEN4/*.pdf COMPACT4/*.pdf
