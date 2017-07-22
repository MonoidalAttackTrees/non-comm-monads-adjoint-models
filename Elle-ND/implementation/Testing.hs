module Testing where

import Syntax
import Parser
import Pretty

------------------------------------------------------------------------
-- Lambda testing                                                     --
------------------------------------------------------------------------

lamtest0_T = parseTester_T lamParse_T "\\(x:Unit).y"
lamtest0_S = parseTester_T lamParse_T "\\l(x:Unit).y"

lamtest1_T = parseTester_T lamParse_T "\\(x:Unit (x) Unit).y"

lamtest'_T = parseTester_T lamParse_T "\\(x:Unit(x)Unit.(tens y z)"

lamtest2_T = parseTester_T lamParse_T "\\(x:Unit-oUnit).y"

lamtest3_T = parseTester_T lamParse_T "\\(x:Unit -o Unit).y"






























