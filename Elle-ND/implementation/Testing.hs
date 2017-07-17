module Testing where

import Syntax
import Parser
import Pretty

------------------------------------------------------------------------
-- Lambda testing                                                     --
------------------------------------------------------------------------

lamtest0 = parseTester lamParse "\\(x:Unit).y"

lamtest1 = parseTester lamParse "\\(x:Unit (x) Unit).y"

lamtest' = parseTester lamParse "\\(x:Unit(x)Unit.(tens y z)"

lamtest2 = parseTester lamParse "\\(x:Unit-oUnit).y"

lamtest3 = parseTester lamParse "\\(x:Unit -o Unit).y"






























